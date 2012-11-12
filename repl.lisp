;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;
;;;; An interactive REPL.
;;;; Based on rkrepl by Richard M Kreuter <kreuter@progn.net>.
;;;;

(defpackage "REPL"
  (:use "COMMON-LISP")
  (:export "USE-REPL"
           "UNUSE-REPL"
           "DEFCMD"
           "*COMMAND-OUTPUT*"
           "*COMMAND-CHAR*"
           "*COMMAND-PACKAGES*"))

(in-package "REPL")

(defvar *command-output* (make-synonym-stream '*standard-output*)
  "Output stream for top-level commands.")

(defmacro defcmd (name &rest args)
  "Define a command for the top-level repl.

This just defines a zero-argument function named NAME. Commands should
emit output to *COMMAND-OUTPUT*, and may read from *STANDARD-INPUT* to
obtain user input."
  `(defun ,name () ,@args))

(defun run-command (command)
  (let ((results))
    (with-input-from-string (*standard-input*
                             (with-output-to-string (*command-output*)
                               (setq results (multiple-value-list
                                              (funcall command)))))
      (loop for line = (read-line nil nil)
            while line
            do (format *command-output* "; ~A~%" line))
      (values-list results))))

(defun prompt (*standard-output*)
  (format t "~&~A> "
          (first (sort (cons (package-name *package*)
                             (package-nicknames *package*))
                       '< :key 'length))))

(defvar *command-char* #\:
  "Character that begins REPL command line.")

(defvar *command-packages* (list #.(find-package *package*))
  "List of packages in which to search for commands.")

(define-condition command-not-found (error)
  ((name :reader command-name :initarg :name)))

(defun find-command (name &optional (not-found-error-p t) &aux
                     (name (string name)))
  (or (loop for package in *command-packages*
            thereis (let ((symbol (find-symbol name (find-package package))))
                      (when (fboundp symbol) symbol)))
      (and not-found-error-p (error 'command-not-found :name name))))

(defun read-form (*standard-input* *standard-output*)
  (cond ((prog1 (char= (peek-char t) *command-char*)
           ;; Wart/flaw/the-universe-sucks: if we don't force the output
           ;; stream back to column 0, the pretty-printer will start at some
           ;; dynamic column number based on the length of the prompt string.
           (terpri))
         (read-char)
         (let ((name (let ((*package* (find-package "KEYWORD")))
                       (symbol-name (read-preserving-whitespace)))))
           (run-command (or (find-command name nil)
                            (lambda ()
                              (format *command-output*
                                      "Command not found: ~A~%" name)
                              (values))))))
        (t (read-preserving-whitespace))))

(defun use-repl ()
  ;; FIXME: also define a repl-fun-generator function for threaded Lisps.
  (macrolet ((save-and-set (var symbol)
               `(progn
                  (unless (get ,symbol ',var)
                    (setf (get ,symbol ',var) ,var))
                  (setf ,var ,symbol))))
    (save-and-set sb-int:*repl-prompt-fun* 'prompt)
    (save-and-set sb-int:*repl-read-form-fun* 'read-form))
  (values))

(defun unuse-repl ()
  (macrolet ((restore (var symbol)
               `(setf ,var (get ,symbol ',var)
                      (get ,symbol ',var) nil)))
    (restore sb-int:*repl-prompt-fun* 'prompt)
    (restore sb-int:*repl-read-form-fun* 'read-form))
  (values))

(defun peek-to-newline ()
  "Like PEEK-CHAR, but if a #\Newline is seen in the initial
whitespace, return NIL."
  (loop for char = (read-char)
        do (case char
             ((#\Space #\Tab))
             (#\Newline (return nil))
             (otherwise (unread-char char) (return char)))))

;; The universal translator for all your lettercase needs.  Note that
;; INVERT-STRING is its own inverse.
(defun invert-string (string &aux (string (string string)))
  (cond ((every (lambda (character)
                  (if (alpha-char-p character)
                      (upper-case-p character)
                      t))
                string)
         (string-downcase string))
        ((every (lambda (character)
                  (if (alpha-char-p character)
                      (lower-case-p character)
                      t))
                string)
         (string-upcase string))
        (t string)))

(defun internalize-string (string &optional (customary-case :lower))
  (ecase customary-case
    (:lower (invert-string string))
    (:upper (string string))))

(defun externalize-string (string &optional (customary-case :upper))
  (ecase customary-case
    (:lower (invert-string string))
    (:upper (string string))))

(defun read-stringy-argument (&optional (newline-error-p t) newline-value)
  (case (peek-to-newline)
    (#\" (read))
    ((nil) (if newline-error-p
               (format *command-output* "No argument before a newline.~%")
               newline-value))
    (otherwise (with-output-to-string (*standard-output*)
                 (loop for char = (read-char nil nil)
                       until (or (null char)
                                 (member char '(#\Space #\Newline)))
                       do (write-char char))))))

(defcmd cd
    (let ((pathname (pathname
                     (read-stringy-argument nil (user-homedir-pathname)))))
      (format *command-output* "Default pathname: ~A"
              (setf *default-pathname-defaults* pathname))))

(defvar *last-compiled-file* nil)
(defcmd cc
    (compile-file
     (setq *last-compiled-file*
           (pathname (read-stringy-argument nil *last-compiled-file*)))))

(defvar *last-loaded-file* nil)
(defcmd ld
    (load
     (setq *last-loaded-file*
           (pathname (read-stringy-argument nil *last-loaded-file*)))))

(defcmd re 
    (let ((module (internalize-string (read-stringy-argument))))
      (format *command-output*
              "~:[~;~&Module ~A loaded.~%~]"  (require module) module)))

(defcmd rm
    (delete-file (pathname (read-stringy-argument))))

(defcmd pa
    (let* ((name (internalize-string (read-stringy-argument nil "cl-user")))
           (package (find-package name)))
      (if package
          (format *command-output* "Package: ~A"
                  (package-name (setf *package* package)))
          (error "No package named ~A exists." name))))

;;; CLWEB stuff.
#+#.(cl:if (cl:member "CLWEB" cl:*modules* :test 'cl:equalp) '(cl:and) '(cl:or))
(progn
  (defvar *last-loaded-web* nil)
  (defcmd lw
      (handler-bind ((style-warning #'muffle-warning))
        (clweb:load-web
         (setq *last-loaded-web*
               (pathname (read-stringy-argument nil *last-loaded-web*))))))

  (defvar *last-tangled-file* nil)
  (defcmd tf
      (handler-bind ((style-warning #'muffle-warning))
        (clweb:tangle-file
         (setq *last-tangled-file*
               (pathname (read-stringy-argument nil *last-tangled-file*))))))

  (defvar *last-woven-file* nil)
  (defcmd we
      (handler-bind ((style-warning #'muffle-warning))
        (clweb:weave
         (setq *last-woven-file*
               (pathname (read-stringy-argument nil *last-woven-file*)))))))

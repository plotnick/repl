;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; 
;;;; rkrepl.lisp: My REPL.  Inspired by sb-aclrepl, sorta, but
;;;; slightly more dynamically implemented.
;;;; 
;;;; Copyright 2009, Richard M Kreuter <kreuter@progn.net>
;;;; 
;;;; Time-stamp: <2009-02-19 08:54:52 kreuter>

(defpackage "RKREPL"
  (:use "COMMON-LISP")
  (:export "USE-REPL"
           "UNUSE-REPL"
           "DEFCMD"
           "*COMMAND-OUTPUT*"))

(in-package "RKREPL")

(defvar *command-output* (make-synonym-stream '*standard-output*))

;; Todo: commands should be made able to document themselves.
(defmacro defcmd (keywords &body code)
  "Define a command for each keyword in KEYWORDS.  KEYWORDS is a
designator for a list of symbols.  CODE is treated as the body of a
lambda expression invoked when the input to the REPL begins with any
keyword in KEYWORDS.  Commands should emit output to *COMMAND-OUTPUT*."
  (setq keywords (if (listp keywords) keywords (list keywords)))
  (let ((keyword (gensym)))
    `(dolist (,keyword ',keywords ',keywords)
       (setf (get ,keyword 'command)
             ,(typecase code
                ;; note: the first two typecases aren't documented,
                ;; and should maybe not be supported.
                ((cons symbol null) (list 'quote code))
                ((cons (cons (member function lambda) *) *) code)
                (t `(lambda () ,@code)))))))

(defun peek-to-newline ()
  "Like PEEK-CHAR, but if a #\Newline is seen in the initial
whitespace, return NIL."
  (loop for char = (read-char)
        do (case char
             ((#\space #\tab))
             (#\newline (return nil))
             (otherwise (unread-char char) (return char)))))

;; The universal translator for all your lettercase needs.  Note that
;; INVERT-STRING is its own inverse.
(defun invert-string (string &optional (customary-case :lower))
  (setq string (string string))
  (cond ((every #'(lambda (character)
                    (if (alpha-char-p character)
                        (upper-case-p character)
                        t))
                string)
         (string-downcase string))
        ((every #'(lambda (character)
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
                                 (member char '(#\space #\newline)))
                       do (write-char char))))))

(defcmd :cd
    (let ((pathname (pathname
                     (read-stringy-argument nil (user-homedir-pathname)))))
      (format *command-output* "Default pathname: ~A"
              (setf *default-pathname-defaults* pathname))))

(defvar *last-compiled-file* nil)
(defcmd :cc
    (compile-file
     (setq *last-compiled-file*
           (pathname (read-stringy-argument nil *last-compiled-file*)))))

(defvar *last-loaded-file* nil)
(defcmd :ld
    (load
     (setq *last-loaded-file*
           (pathname (read-stringy-argument nil *last-loaded-file*)))))

(defcmd :re 
    (let ((module (internalize-string (read-stringy-argument))))
      (format *command-output*
              "~:[~;~&Module ~A loaded.~%~]"  (require module) module)))

(defcmd :rm
    (delete-file (pathname (read-stringy-argument))))

(defcmd :pa
    (let* ((name (internalize-string (read-stringy-argument nil "cl-user")))
           (package (find-package name)))
      (if package
          (format *command-output* "Package: ~A"
                  (package-name (setf *package* package)))
          (error "No package named ~A exists." name))))

(defun run-command (keyword)
  (funcall (get keyword 'command)))

(defun rkrepl-prompt-fun (*standard-output*)
  (format t "~&~A> "
          (first (sort (cons (package-name *package*)
                             (package-nicknames *package*))
                       '< :key 'length))))

(defun rkrepl-read-form-fun (*standard-input* *standard-output*)
  (let ((thing (read-preserving-whitespace)))
    ;; Wart/flaw/the-universe-sucks: if we don't force the output
    ;; stream back to column 0, the pretty-printer will start at some
    ;; dynamic column number based on the length of the prompt string.
    (write-char #\return)
    (if (keywordp thing)
        (let ((results))
         (with-input-from-string (*standard-input*
                                  (with-output-to-string (*command-output*)
                                    (setq results
                                          (multiple-value-list
                                           (run-command thing)))))
           (loop for line = (read-line nil nil)
                 while line
                 do (format *command-output* "; ~A~%" line))
           (values-list results)))
        thing)))

(defun use-repl ()
  ;; FIXME: also define a repl-fun-generator function for threaded
  ;; Lisps.
  (setf sb-int:*repl-prompt-fun* 'rkrepl-prompt-fun
        sb-int:*repl-read-form-fun* 'rkrepl-read-form-fun))

(defun unuse-repl ()
  (setf sb-int:*repl-prompt-fun*  'sb-impl::repl-prompt-fun
        sb-int:*repl-read-form-fun* 'sb-impl::repl-read-form-fun))

;;; CLWEB stuff.
(defvar *last-loaded-web* nil)
(defcmd :lw
    (handler-bind ((style-warning #'muffle-warning))
     (clweb:load-web
      (setq *last-loaded-web*
            (pathname (read-stringy-argument nil *last-loaded-web*))))))

(defvar *last-tangled-file* nil)
(defcmd :tf
    (handler-bind ((style-warning #'muffle-warning))
     (clweb:tangle-file
      (setq *last-tangled-file*
            (pathname (read-stringy-argument nil *last-tangled-file*))))))

(defvar *last-woven-file* nil)
(defcmd :we
    (handler-bind ((style-warning #'muffle-warning))
      (clweb:weave
       (setq *last-woven-file*
             (pathname (read-stringy-argument nil *last-woven-file*))))))

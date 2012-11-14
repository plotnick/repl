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

(defvar *registered-commands* nil
  "List of names of all known commands.")

(deftype lambda-list-keyword () `(member ,@lambda-list-keywords))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-arg-readers (lambda-list)
    "Parse a command lambda-list and return the function lambda list
and associated argument reader forms.

This function understands a highly restricted subset of the general
lambda-list syntax. Required and optional arguments may both be given
either by symbols or by lists of the form (NAME READER-FORM). If a reader
form is given, it is collected and will be used to produce a form to be
supplied as the NAME argument. If no reader form is given, NIL will be
supplied."
    (loop with state = :required
          and args = '()
          and reader-forms = '()
          for x in lambda-list
          do (case state
               ((:required &optional)
                (etypecase x
                  (lambda-list-keyword (push x args)
                                       (setq state x))
                  (symbol (push x args)
                          (push nil reader-forms))
                  (list (push (car x) args)
                        (push (cadr x) reader-forms))))
               (otherwise (push x args)))
          finally (return (values (nreverse args) (nreverse reader-forms))))))

(defun make-thunk (form)
  (if form
      (coerce `(lambda () ,form) 'function)
      (constantly nil)))

(defun make-command-form (command)
  `(,command ,@(mapcar #'funcall (get command 'arg-reader-functions))))

(defmacro defcmd (name lambda-list &body body)
  "Define a command for the top-level repl.

The lambda list may contain argument reader forms for the required and
optional arguments; see PARSE-ARG-READERS for details."
  (multiple-value-bind (args reader-forms) (parse-arg-readers lambda-list)
    `(progn
       (pushnew ',name *registered-commands*)
       (setf (get ',name 'arg-reader-functions)
             (mapcar #'make-thunk '(,@reader-forms)))
       (defun ,name ,args ,@body))))

(defun prompt (*standard-output*)
  (format t "~&~A> "
          (first (sort (cons (package-name *package*)
                             (package-nicknames *package*))
                       '< :key 'length))))

(defvar *command-char* #\:
  "Character that begins REPL command line.")

(defvar *command-packages* (list *package*)
  "List of packages in which to search for commands.")

(define-condition command-not-found (error)
  ((name :reader command-name :initarg :name)))

(defun find-command (name &optional (not-found-error-p t) &aux
                     (name (string name)))
  (or (loop for package in *command-packages*
            thereis (let ((symbol (find-symbol name (find-package package))))
                      (when (fboundp symbol) symbol)))
      (and not-found-error-p (error 'command-not-found :name name))))

(defun slurp ()
  "Discard all pending input on standard input."
  (loop while (read-char-no-hang)))

(defun read-form (*standard-input* *standard-output*)
  (cond ((prog1 (char= (peek-char t) *command-char*)
           ;; Wart/flaw/the-universe-sucks: if we don't force the output
           ;; stream back to column 0, the pretty-printer will start at some
           ;; dynamic column number based on the length of the prompt string.
           (terpri))
         (read-char)
         (let* ((name (let ((*package* (find-package "KEYWORD")))
                        (symbol-name (read-preserving-whitespace))))
                (command (find-command name nil)))
           (if command
               (make-command-form command)
               (progn
                 (format *command-output* "Command not found: ~A~%" name)
                 (slurp)
                 '(values)))))
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

(define-condition required-argument-missing (error) ())

(defun read-stringy-argument (&optional (newline-error-p t) newline-value)
  (case (peek-to-newline)
    (#\" (read))
    ((nil) (if newline-error-p
               (error 'required-argument-missing)
               newline-value))
    (otherwise (with-output-to-string (*standard-output*)
                 (loop for char = (read-char nil nil)
                       until (or (null char)
                                 (member char '(#\Space #\Newline)))
                       do (write-char char))))))

(defcmd help (&optional (name (internalize-string (read-stringy-argument nil))))
  "Display help for commands."
  (let ((command (and name (find-command name nil))))
    (if command
        (format *command-output* "~A" (documentation command 'function))
        (format *command-output* "Available commands:~{ ~A~}."
                (sort *registered-commands* 'string<)))
    (values)))

(defcmd cd (&optional (pathname (read-stringy-argument nil)))
  "Set default pathname."
  (setf *default-pathname-defaults*
        (if pathname
            (make-pathname :directory (pathname-directory (truename pathname)))
            (user-homedir-pathname))))

(defcmd re ((name (read-stringy-argument)))
  "Reload a module."
  (let ((module (internalize-string name)))
    (format *command-output* "~:[~;~&Module ~A loaded.~%~]"
            (require module) module)))

(defcmd rm ((pathname (read-stringy-argument)))
  "Delete a file."
  (delete-file (pathname pathname)))

(defcmd pa ((name (read-stringy-argument nil)) &aux
            (name (if name (internalize-string name) "COMMON-LISP-USER"))
            (package (find-package name)))
  "Change the current package."
  (if package
      (setf *package* package)
      (error "No package named ~A exists." name)))

(defun read-pathname-argument (command &optional prompt)
  "Try to read a pathname argument for COMMAND, optionally prompting with
PROMPT if none is available or cached in COMMAND's plist."
  (or (read-stringy-argument nil)
      (get command 'last-file)
      (when prompt
        (loop with *standard-input* = *query-io*
              and *standard-output* = *query-io*
              do (princ prompt)
                 (force-output)
              thereis (read-stringy-argument nil)))))

(defmacro define-simple-file-command (name (&key documentation
                                            (prompt "File name: ")
                                            function))
  "Define a command that operates on a single file, defaulting to the last
file given to that command. If the command has not been run before, prompt
for a file name."
  (check-type function symbol)
  (check-type prompt string)
  `(defcmd ,name ((pathname (read-pathname-argument ',name ,prompt)))
     ,@(if documentation (list documentation) nil)
     (,function (setf (get ',name 'last-file) (pathname pathname)))))

(define-simple-file-command cc
  (:documentation "Compile a file."
   :prompt "Compile file: "
   :function compile-file))

(define-simple-file-command ld
  (:documentation "Load a file."
   :prompt "Load file: "
   :function load))

;;; CLWEB stuff.
#+#.(cl:if (cl:member "CLWEB" cl:*modules* :test 'cl:equalp) '(cl:and) '(cl:or))
(progn
  (define-simple-file-command lw
    (:documentation "Load a web."
     :prompt "Load web: "
     :function clweb:load-web))

  (define-simple-file-command tf
    (:documentation "Tangle a web."
     :prompt "Tangle web: "
     :function clweb:tangle-file))

  (define-simple-file-command we
    (:documentation "Weave a web."
     :prompt "Weave web: "
     :function clweb:weave)))

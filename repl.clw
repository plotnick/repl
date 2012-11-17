% -*- mode: CLWEB -*-
\font\sc=cmcsc10
\def\<#1>{\leavevmode\hbox{$\mkern-2mu\langle${\it #1\/}$\rangle$}}
\def\etc.{{\it \char`&c.\spacefactor1000}}
\def\eof{{\sc eof}}
\def\repl{{\sc repl}}

@*REPL. This module provides a command-line interface designed to sit atop
an interactive, top-level |read-eval-print| loop (\repl). It is based on
{\tt RKREPL} by Richard~M. Kreuter \<kreuter@@progn.net>, but has diverged
quite a bit in detail, if not in spirit.

The system is designed to hook into the \repl\ during the reading phase (i.e.,
just prior to evaluation), and works by translating characters on standard
input into Lisp forms to be evaluated---but only sometimes by calling |read|
directly. It currently supports only SBCL, although it should be relatively
easy to port to other Lisp implementations that support similar \repl\ hooks.
It was also designed to be used in conjunction with an interface that includes
a facility for remembering and recalling user input (e.g., |inferior-lisp|
or something similar), and so does not attempt to provide a history mechanism
of its own.

@l
(provide "REPL")
@e
(defpackage "REPL"
  (:use "COMMON-LISP" #+sbcl "SB-EXT")
  (:export "USE-REPL"
           "UNUSE-REPL"
           "DEFCMD"
           "*COMMAND-OUTPUT*"
           "*COMMAND-CHAR*"
           "*COMMAND-PACKAGES*"
           "*IGNORE-EOF*"))
@e
(in-package "REPL")

@t*Test suite. The test suite for this system uses Richard Waters's
{\sc rt} library. For more information on {\sc rt}, see Richard C.~Waters,
``Supporting the Regression Testing of Lisp Programs,''
{\it SIGPLAN Lisp Pointers}~4, no.~2 (1991): 47--53.

We use the sleazy trick of manually importing the external symbols of
the {\sc rt} package instead of the more sensible |(use-package "RT")|
because many compilers issue warnings when the use-list of a package
changes, which would occur if the |defpackage| form above were evaluated
after the tests have been loaded.

@l
(in-package "REPL")
@e
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-rt)
  (do-external-symbols (symbol (find-package "SB-RT"))
    (import symbol)))

@ We'll define our global variables and condition classes as we need them,
but we'd like them to appear near the top of the tangled output.

@l
@<Global variables@>
@<Condition classes@>

@ Our reader supports a notion of {\it commands}, which are essentially
function calls that use a special, non-parenthesized syntax that is quick
to type and which supports some useful shortcuts, like reading `stringy'
arguments without quotes, automatic case-folding, \etc. The idea is that at
the \repl\ prompt, you can type either an ordinary Lisp form to be read in
the usual way (viz., by |read|), or a command name optionally followed by
some arguments. In the latter case, the command and its arguments are then
used to construct a call to the function which implements the command, and
it is that function call which gets evaluated.

A command's functionality is implemented primarily by a function with
the same name (under |string=|) as the command; we'll call such functions
{\it command functions\/}, or just `commands' where no confusion can arise.
A command's name is defined to be the |symbol-name| of the underlying command
function, which means that commands are named by strings, and that there is
a single namespace for all commands.

[In theory, it might be useful to support package-prefixed commands.
The problem is that after you've read a symbol using |read|, you can't
tell whether the string representation used to name that symbol included
a package prefix or not. If Common Lisp provided a |read-token| function,
this wouldn't be an issue; but it doesn't, and it's surprisingly tricky
to write one (and probably impossible to do so portably).]

To avoid cluttering the current package with interned command names, we'll
read all command names with the {\tt KEYWORD} package as the current package.
We use |read-preserving-whitespace| so that we can treat newlines specially
when collecting arguments (see below).

@l
(defun read-command-name (&optional stream)
  (let ((*package* (find-package "KEYWORD")))
    (symbol-name (read-preserving-whitespace stream))))

@ Commands are distinguished from ordinary Lisp forms by looking at the
first character available on standard input. If it is the same under
|char=| as the value of |*command-char*|---which we call, unsurprisingly,
the {\it command character}---then the symbol named by the immediately
following characters (i.e., what |read| returns starting just {\it after\/}
the command character) is taken as a command name. The command character
is {\it not\/} part of the command name; it just tells the reader that a
command name follows.

The command character defaults to~`:', but many other characters would be
suitable; e.g., `/', `${\char`\\}$', `!'. Really, any character except~`('
that isn't commonly used at the beginning of a symbol name is probably fine.

@<Global variables@>=
(defvar *command-char* #\:
  "Character that introduces a REPL command.")

@t Ensure that command names are read correctly and that no symbols are
interned in the current package.

This is the first, but not the last, time that we'll need a temporary
package for a test, so we'll define a little macro to make that easy.
@l
(defmacro with-temporary-package
    ((&optional (name (format nil "TEMP-PACKAGE-~D" *gensym-counter*)))
     &body body)
  (let ((temp-package (gensym)))
    `(let* ((,temp-package (make-package ,name))
            (*package* ,temp-package))
       (unwind-protect (progn ,@body)
         (delete-package ,temp-package)))))

(deftest read-command-name
  (with-temporary-package ()
    (with-input-from-string (stream "FOO")
      (values (read-command-name stream)
              (null (loop for symbol being each present-symbol
                          collect symbol)))))
  "FOO"
  t)

@ To find a command by its name, we'll look for an fbound symbol with the
same name in each of the packages listed in |*command-packages*|. The list
is searched in order so that user-supplied commands may override default
commands if they register their packages using |register-command-package|.

@l
(defun find-command (name &optional (not-found-error-p t) &aux ;
                     (name (string name)))
  (loop for package in *command-packages*
        thereis (let ((symbol (find-symbol name (find-package package))))
                  (when (fboundp symbol) symbol))
        finally (and not-found-error-p (error 'command-not-found :name name))))

(defun register-command-package (package)
  (push package *command-packages*))

@ @<Global variables@>=
(defvar *command-packages* (list *package*)
  "List of packages in which to search for commands.")

@t To test |find-command| as well as some of the argument reader helper
functions later on, it will be convenient to be able to generate temporary
commands. So here's a little macro that evaluates its body in a lexical
environment in which each of a list of symbols is bound to a fresh command
function whose name is interned in a temporary package. It uses a little
helper macro to ensure that the commands can be found via the specified
search procedure.

@l
(defmacro with-temporary-command-package (&body body)
  `(with-temporary-package ()
     (let ((*command-packages* (cons *package* *command-packages*)))
       ,@body)))

(defmacro with-temporary-commands ((&rest commands) &body body)
  `(with-temporary-command-package ()
     (let ,@(mapcar (lambda (name)
                      `((,name (intern ,(string name)))))
                    commands)
       ,@(loop for name in commands
               collect `(setf (symbol-function ,name) #'identity))
       ,@body)))

@t@l
(deftest find-command
  (with-temporary-commands (foo)
    (let ((bar (intern "BAR")))
      (values (eq (find-command "FOO") foo)
              (find-command "BAR" nil) ; not fbound
              (find-command "BAZ" nil))))
  t
  nil
  nil)

@ None of the code in this module currently signals an error when a command
is not found, but it should be possible to provide some convenient restarts
in |find-command|.

@<Condition classes@>=
(define-condition command-not-found (error)
  ((name :reader command-name :initarg :name)))

@ After a command name is read and its command function found, we'll
immediately look for arguments to pass it. We could just use |read|,
but we want to support various kinds of shortcut syntaxes, so we'll
let the commands themselves determine how to read their arguments.

When commands are defined, each required and optional parameter may be
accompanied by a {\it reader form}, which, when evaluated, should read
and return an argument to be supplied for that parameter. But rather than
calling |eval| on these forms when we need to read an argument, we'll have
the command defining form compile them into thunks and store them in the
command function's property list.

@l
(defun collect-command-arguments (command)
  (mapcar #'funcall (get command 'arg-reader-functions)))

@ We're now ready to define our main reader function, |read-form|. This
function peeks at the first character on standard input, waiting if none
is available. If it is the command character, it reads the command name
which follows and returns a compound form with the command function's name
in the |car| and the list of collected arguments in the~|cdr|. Otherwise,
it just calls~|read|.

@l
(defun slurp ()
  "Discard all pending input on standard input."
  (loop while (read-char-no-hang)))

(defun read-form (*standard-input* *standard-output*)
  (handler-case
      (cond (@<Is the first available character...@>
             (read-char)                ; discard command character
             (let* ((name (read-command-name))
                    (command (find-command name nil)))
               (if command
                   `(,command ,@(collect-command-arguments command))
                   (progn
                     (format *command-output* "Command not found: ~A~%" name)
                     (slurp)
                     '(values)))))
            (t (read)))
    (end-of-file () @<Handle \eof\ on standard input@>)))

@ Wart/flaw/the-universe-sucks: if we don't immediately force the output
stream back to column~0, the pretty-printer will start at some dynamic
column number based on the length of the prompt string.

@<Is the first available character a command character?@>=
(prog1 (char= (peek-char t) *command-char*)
  (terpri))

@ Like SBCL's default form reader, we respect the venerable Unix convention
of interpreting \eof\ as a request to exit the process. Unlike SBCL, however,
we also provide an option to disable this behavior.

@<Handle \eof...@>=
(if *ignore-eof* '(values) (exit))

@ This variable behaves like the `ignoreeof' option in the Bourne shell.

@<Global variables@>=
(defvar *ignore-eof* nil
  "If true, ignore EOF on standard input.")

@ Now that we've shown how commands are read, let's define some functions
that commands can use to collect their arguments.

The function |read-stringy-argument| is used to collect a string-like
argument. A newline as the first non-space character denotes an empty
argument; a leading quotation mark denotes a string; and any other
sequence of non-space characters denotes a single-word string.

@l
(defun peek-to-newline ()
  "Like PEEK-CHAR, but if a #\Newline is seen in the initial whitespace,
return NIL."
  (loop for char = (read-char)
        do (case char
             ((#\Space #\Tab))
             (#\Newline (return nil))
             (otherwise (unread-char char) (return char)))))

(defun read-stringy-argument (&optional (newline-error-p t) default)
  (case (peek-to-newline)
    (#\" (read))
    ((nil) (if newline-error-p (error 'unexpected-newline) default))
    (otherwise (with-output-to-string (*standard-output*)
                 (loop for char = (read-char nil nil)
                       until (or (null char)
                                 (member char '(#\Space #\Newline)))
                       do (write-char char))))))

@ @<Condition classes@>=
(define-condition unexpected-newline (error) ())

@ Command arguments are frequently strings that denote Lisp objects whose
names are internalized in some way, like package and symbol names. As a
convenience to the user, we'll support reading such arguments with the same
kind of case folding the Lisp reader gives us for symbol names.

We provide three helper functions to handle this: |invert-string|,
|internalize-string|, and~|externalize-string|. (This last is currently
unused, but is provided for symmetry and for use by user-defined commands.)
Note that |invert-string| is its own inverse.

@l
(defun invert-string (string &aux (string (string string)))
  "The universal translator for all your lettercase needs."
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

@ We're almost ready to turn to the command defining form, but first we
need one more piece of bookkeeping: a list of all of the commands so
defined. Right now, only the `help' command uses this, but it might be
useful later for, e.g.,~prefix matching.

@<Global variables@>=
(defvar *commands* nil
  "A list of the names of all known commands.")

@ |defcmd| is the primary command defining form. Its surface syntax
is identical to that of |defun|, and in fact it consists of little more
than a wrapper around that macro. But the lambda list provided to |defcmd|
is not an ordinary lambda list: it supports a special syntax for defining
argument readers for the optional and required parameters of the command
function.

Our lambda list parsing routine returns two values: an ordinary lambda
list containing all of the supplied parameter specifiers (including any
|&rest| and |&key| specifiers), and a list of reader forms. If a reader
form is not supplied with a parameter, |nil| will be used as a placeholder
in the list of forms, and we'll default to using |read-preserving-whitespace|
as the argument reader for that parameter.

Note that the argument reader thunks are constructed in the null lexical
environment.

@l
@<Define command lambda list parsing routine@>

(defmacro defcmd (name lambda-list &body body)
  "Define a command for the top-level REPL."
  (multiple-value-bind (params reader-forms) (parse-arg-readers lambda-list)
    `(progn
       (pushnew ',name *commands*)
       (setf (get ',name 'arg-reader-functions)
             (mapcar #'make-arg-reader '(,@reader-forms)))
       (defun ,name ,params ,@body))))

(defun make-arg-reader (form)
  (if form
      (coerce `(lambda () ,form) 'function)
      #'read-preserving-whitespace))

@ The syntax of command lambda lists differs from that of ordinary lambda
lists only in the required and optional parameter specifications. Required
and optional parameters may be specified either by symbols or by lists of
the form |(name reader-form)|.

Note that this syntax conflicts with the usual specification for optional
parameters. In particular, neither init-forms nor supplied-p parameters
are currently supported for optional parameters to commands. Instead, the
argument readers should perform whatever defaulting is required.

This function is used by the |defcmd| macro, and so its definition must be
available at compile time.

@<Define command lambda list parsing...@>=
(deftype lambda-list-keyword () `(member ,@lambda-list-keywords))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-arg-readers (lambda-list)
    (loop with state = '&required
          and params = '()
          and reader-forms = '()
          for x in lambda-list
          do (case state
               ((&required &optional)
                (etypecase x
                  (lambda-list-keyword (push x params)
                                       (setq state x))
                  (symbol (push x params)
                          (push nil reader-forms))
                  (list (push (car x) params)
                        (push (cadr x) reader-forms))))
               (otherwise (push x params)))
          finally (return (values (nreverse params)
                                  (nreverse reader-forms))))))

@ In addition to a user-defined reader function, the top-level \repl\ also
provides a hook for printing a custom prompt. We'll use the shortest
available package nickname, followed by `$>$'.

@l
(defun prompt (*standard-output*)
  (format t "~&~A> "
          (first (sort (cons (package-name *package*) ;
                             (package-nicknames *package*))
                       #'< :key #'length))))

@ Having defined our reader and prompt hooks, we're now ready to
interface with the top-level \repl. The functions |use-repl| and
|unuse-repl| install and remove, respectively, our hook functions.
The old values are stashed in the plists of our hook functions so that
we don't have to hard-code the defaults.

These functions are obviously highly implementation-specific. We should
also define a repl-fun-generator function for threaded Lisps, but do not
yet do so.

@l
(defun use-repl ()
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

@ Commands may occasionally wish to print informative messages, and we'll
generally send all such output to the stream stored in |*command-output*|.
This will generally be a synonym for standard output, but by using it
instead of standard output directly, we make it easy for the user to
silence all command output or redirect it to a file.

@<Global variables@>=
(defvar *command-output* (make-synonym-stream '*standard-output*)
  "Output stream for REPL commands.")

@*Basic commands. We're finally ready to start defining some actual commands.
We'll start with a sort of meta-command which provides help for the available
commands.

@l
(defcmd help (&optional (name (internalize-string (read-stringy-argument nil))))
  "Display help for commands."
  (let ((command (and name (find-command name nil))))
    (if command
        (format *command-output* "~A" (documentation command 'function))
        (format *command-output* "Available commands:~{ ~A~}."
                (sort *commands* 'string<)))
    (values)))

@ We generally don't care about the working directory of the Lisp process,
but being able to quickly switch |*default-pathname-defaults*| is handy.
This command ensures that its argument names a directory, even if the
trailing slash is elided.

@l
(defcmd cd (&optional (pathname (read-stringy-argument nil)))
  "Set default pathname."
  (setf *default-pathname-defaults*
        (if pathname
            (make-pathname :directory (pathname-directory (truename pathname)))
            (user-homedir-pathname))))

@ |provide| and |require| may be deprecated, but they still win over most
of the available alternatives.

@l
(defcmd re ((name (read-stringy-argument)))
  "Reload a module."
  (let ((module (internalize-string name)))
    (format *command-output* "~:[~;~&Module ~A loaded.~%~]"
            (require module) module)))

@ @l
(defcmd rm ((pathname (read-stringy-argument)))
  "Delete a file."
  (delete-file (pathname pathname)))

@ Fast package switching is also useful.

@l
(defcmd pa ((name (read-stringy-argument nil)) &aux
            (name (if name (internalize-string name) "COMMON-LISP-USER"))
            (package (find-package name)))
  "Change the current package."
  (if package
      (setf *package* package)
      (error "No package named ~A exists." name)))

@ This next command is pure syntactic sugar, but it's handy sugar.
It pretty-prints the macro expansion of an implicitly quoted form.

@l
(defcmd ma ((form `(quote ,(read))))
  "Pretty-print the macro expansion of FORM."
  (write (macroexpand form) :stream *command-output* :escape t :pretty t)
  (values))

@ Next we'll define a suite of commands that operate on files:
load a file, compile a file, \etc. Since it's common to operate on
the same file many times in a row, we'll define an argument reader
function that caches the last value given in the command's plist.
It can also prompt the user for a file name to use.

@l
(defun read-pathname-argument (command &optional prompt)
  "Try to read a pathname argument for COMMAND, optionally prompting with
PROMPT if none is available or cached in COMMAND's plist."
  (setf (get command 'last-file)
        (or (read-stringy-argument nil)
            (get command 'last-file)
            (handler-case
                (when prompt
                  (loop with *standard-input* = *query-io*
                        and *standard-output* = *query-io*
                        do (princ prompt)
                           (force-output)
                        thereis (read-stringy-argument nil)))
              (end-of-file ())))))

@t In this test, the space after {\tt foo} serves as the delimiter for that
token, and the newline that follows should cause |read-pathname-argument|
to look for a cached value.

@l
(deftest read-pathname-argument
  (with-input-from-string (*standard-input* (format nil "foo ~%"))
    (with-temporary-commands (foo)
      (values (read-pathname-argument foo)
              (read-pathname-argument foo))))
  "foo"
  "foo")

@ Single-function commands that operate on a single pathname argument
are fairly common, we'll define a little defining form just for them.

@l
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
     (,function (pathname pathname))))

@ These are probably the most commonly used commands, but their definitions
are now pleasantly simple.

@l
(define-simple-file-command cc
  (:documentation "Compile a file."
   :prompt "Compile file: "
   :function compile-file))

(define-simple-file-command ld
  (:documentation "Load a file."
   :prompt "Load file: "
   :function load))

@*Index.
@t*Index.

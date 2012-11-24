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
  (:use "COMMON-LISP" "SB-EXT" "SB-WALKER")
  (:import-from "SB-POSIX" "CHDIR" "GETCWD")
  (:export "USE-REPL"
           "UNUSE-REPL"
           "DEFCMD"
           "*COMMAND-OUTPUT*"
           "*COMMAND-CHARS*"
           "*COMMAND-PACKAGE*"
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
A command's name is defined to be the |symbol-name| of the underlying
command function; there is thus a single namespace for all commands.

@ All of the command functions live in a dedicated package (the
{\it command package\/}), which is stored in the global |*command-package*|
variable. The name of the command package is unimportant and should not
be depended on. Note that this package does {\it not\/} use or import any
symbols from {\tt COMMON-LISP} package; this is important so that we may
define commands whose names are the same as symbols in that package.

@<Global variables@>=
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *command-package* (make-package "REPL-COMMANDS")
    "Package which contains command functions."))

@ To avoid cluttering the current package with interned command names,
we'll read all command names with the command package as the current
package. We use |read-preserving-whitespace| so that we can treat newlines
specially when collecting arguments (see below).

@l
(defun read-command-name (&rest args)
  (let ((*package* (find-package *command-package*)))
    (let ((symbol (apply #'read-preserving-whitespace args)))
      (and symbol (symbol-name symbol)))))

@t Ensure that command names are read correctly and that no symbols are
interned in the current package.

To test this, we'll use a temporary package as the current package, and
another temporary package as the command package (so that we don't clutter
up the real one). This is halfway to defining temporary commands, an ability
that we'll need later on (when we test |find-command|), so we might as well
just do that now.

|with-temporary-package| executes its body with |*package*| bound
to a temporary package, which is deleted when the body exits.
|with-temporary-commands| executes its body with a fresh command
package containing command functions for each of the given command
names.

@l
(defmacro with-temporary-package ;
    ((&optional (name (format nil "TEMP-PACKAGE-~D" *gensym-counter*)))
     &body body)
  (let ((temp-package (gensym)))
    `(let* ((,temp-package (make-package ,name))
            (*package* ,temp-package))
       (unwind-protect (progn ,@body)
         (delete-package ,temp-package)))))

(defmacro with-temporary-commands ((&rest commands) &body body)
  `(with-temporary-package ()
     (let ((*command-package* *package*)
           ,@(loop for name in commands
                   collect `(,name (intern ,(string name)))))
       ,@(loop for name in commands
               collect `(export ,name *command-package*)
               collect `(setf (symbol-function ,name) #'identity))
       ,@body)))

(deftest read-command-name
  (with-temporary-commands ()
    (with-temporary-package ()
      (with-input-from-string (stream "FOO")
        (values (read-command-name stream)
                (null (loop for symbol being each present-symbol
                            collect symbol))))))
  "FOO"
  t)

@ All of the external symbols of the command package are taken to be command
names.

@l
(defmacro do-commands ((var &optional result-form) &body body)
  `(do-external-symbols (,var *command-package* ,result-form)
     ,@body))

(defun available-commands (&aux commands)
  (do-commands (command commands)
    (push command commands)))

@t This test also serves to exercise the |do-commands| macro. We'll have
occasion to use the set equality predicate later.

@l
(defun set-equalp (list-1 list-2 &rest args)
  (null (apply #'set-exclusive-or list-1 list-2 args)))

(deftest available-commands
  (with-temporary-commands (foo bar baz quux)
    (set-equalp (list foo bar baz quux) (available-commands)))
  t)

@ At the \repl\ prompt, commands are distinguished from ordinary Lisp forms
by looking at the first character available on standard input. If it
matches any of the keys of the alist |*command-chars*|, then it is a
{\it command character}, and the associated command name will be used to
construct a compound form which calls that command. The default command
character has no associated name, and so the name will be read from the
characters immediately following.

The default command character is~`:', but any character except~`(' that
isn't commonly used at the beginning of a symbol name is probably fine.

@<Global variables@>=
(defvar *command-chars* '((#\: . nil)))

@ The primary lookup routine for commands is |find-command|, which takes a
name and attempts to find and return the command function with that name.
But we'll need a few helper functions before we define it, since we want to
make command entry and recovery from errors as easy as possible for the user.

For instance, we'll accept any unambiguous prefix of a defined command's
name as a designator for that command. Exact matches are always considered
unambiguous. Since the list of defined commands is expected to be small (a
few dozen, maybe), a simple linear search is fine here.

@l
(defun string-prefix-p (prefix string)
  (let ((l (length (string prefix)))
        (m (length (string string))))
    (and (>= m l)
         (let ((n (min l m)))
           (string-equal prefix string :end1 n :end2 n)))))

(defun match-commands (name &aux matches)
  (do-commands (command matches)
    (cond ((string-equal command name) (return (list command)))
          ((string-prefix-p name command) (push command matches)))))

@t@l
(deftest string-prefix-p
  (values (string-prefix-p "" "foo")
          (string-prefix-p "f" "foo")
          (string-prefix-p "foo" "foo")
          (string-prefix-p "foo-bar" "foo")
          (string-prefix-p "abc" "foo"))
  t
  t
  t
  nil
  nil)

(deftest match-commands
  (with-temporary-commands (foo foo-bar foo-baz bar baz quux)
    (values (set-equalp (match-commands "FOO") (list foo))
            (set-equalp (match-commands "FOO-") (list foo-bar foo-baz))))
  t
  t)

@ If a command can not be found---either because it does not name a valid
command or because it is an ambiguous prefix---we can prompt the user to
choose a different name. Note that this function calls |find-command| on
the value entered, making the two routines mutually recursive. It is used
to provide arguments to |invoke-restart-interactively|, which is why it
returns a list.

@l
(defun prompt-for-command ()
  (format *query-io* "Enter a command name: ")
  (force-output *query-io*)
  (list (find-command (read-command-name *query-io*))))

@ If the user chooses to abort instead of entering a new command name,
it's nice to give them the opportunity to throw away whatever's left on
standard input, on the assumption that it's only worth evaluating as part
of the aborted command. (SBCL's debugger has a command called {\tt SLURP}
from which this idea was taken, but that command does not also invoke the
|abort| restart).

@l
(defun slurp ()
  (with-output-to-string (*standard-output*)
    (loop with char
          while (setq char (read-char-no-hang nil nil))
          do (write-char char))))

@ Here, then, is the definition of |find-command|. If there is exactly one
command matching the given name, it is returned; otherwise, a correctable
error is signaled.

@l
(defun find-command (name)
  (restart-case
      (let ((command-names (match-commands name)))
        (case (length command-names)
          (0 (error 'command-not-found :name name))
          (1 (return-from find-command (first command-names)))
          (t (error 'ambiguous-command-name ;
                    :name name ;
                    :matches command-names))))
    (use-value (value)
      :report "Specify a command name to use."
      :interactive prompt-for-command
      (return-from find-command value))
    (slurp ()
      :report "Abort command and discard remaining input."
      :interactive slurp
      (abort))))

@ @<Condition classes@>=
(define-condition command-not-found (error)
  ((name :reader command-name :initarg :name))
  (:report (lambda (condition stream)
             (format stream "Command not found: ~A." ;
                     (command-name condition)))))

(define-condition ambiguous-command-name (command-not-found)
  ((matches :reader command-matches :initarg :matches))
  (:report (lambda (condition stream)
             (format stream "Ambiguous command ~A:~{ ~A~}."
                     (command-name condition) (command-matches condition)))))

@t There are three main cases to test for |find-command|: successful lookup,
unsuccessful lookup, and lookup based on an ambiguous prefix.

@l
(deftest (find-command found)
  (with-temporary-commands (foo)
    (eq (find-command "FOO") foo))
  t)

(deftest (find-command not-found)
  (with-temporary-commands ()
    (handler-case (not (find-command (intern "FOO")))
      (command-not-found () t)))
  t)

(deftest (find-command ambig)
  (with-temporary-commands (bar baz)
    (let ((prefix "B")
          (resolved))
      (values (eq (handler-bind ((ambiguous-command-name
                                  (lambda (condition)
                                    (setq resolved ;
                                          (eq (command-name condition) prefix))
                                    (use-value bar))))
                    (find-command prefix))
                  bar)
              resolved)))
  t
  t)

@ Although not usually necessary, it's sometimes handy to invoke command
functions directly. We might as well make that easy by utilizing the
machinery of |find-command|.

@l
(defun run-command (name &rest args)
  (apply (find-command name) args))

@t This test relies on the fact that the commands defined by
|with-temporary-commands| are all fbound to the identity function.
Also note the abbreviation of the command name.

@l
(deftest run-command
  (with-temporary-commands (foo-bar)
    (let ((x (cons nil nil)))
      (eq (run-command 'foo x) x)))
  t)

@ We also support a notion of command aliases. The machinery is simple
enough: aliases are non-fbound (but still external) symbols in the command
package with the |alias| indicator on their plist set to another command
name. Note that we do not check for alias loops, so don't do that.

@l
(defun resolve-aliases (name)
  (do ((name name (get name 'alias)))
      ((fboundp name) name)))

@t@l
(deftest resolve-aliases
  (with-temporary-commands (foo bar baz)
    (fmakunbound bar)
    (fmakunbound baz)
    (setf (get bar 'alias) foo)
    (setf (get baz 'alias) bar)
    (every (lambda (name) (eq (resolve-aliases name) foo))
           (list foo bar baz)))
  t)

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
is available. If it is a command character, it either uses the associated
command or reads the command name which follows, then returns a compound
form with the command function's name in the |car| and the list of
collected arguments in the~|cdr|. Otherwise, it just calls~|read|.

@l
(defun read-form (*standard-input* *standard-output*)
  (handler-case
      (let ((entry (assoc (peek-char t) *command-chars*)))
        (cond ((consp entry)
               (read-char) ; discard command character
               @<Reset the output stream to column~0@>
               (let ((command (resolve-aliases ;
                               (find-command (or (cdr entry) ;
                                                 (read-command-name))))))
                 `(,command ,@(collect-command-arguments command))))
              (t (read))))
    (end-of-file () @<Handle \eof\ on standard input@>)))

@ Wart/flaw/the-universe-sucks: if we don't immediately force the output
stream back to column~0, the pretty-printer will start at some dynamic
column number based on the length of the prompt string.

@<Reset the output stream...@>=
(fresh-line)

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

@ Command arguments frequently denote Lisp objects whose names are
internalized in some way, like package, symbol, and file names. We'll
case-fold such argument strings so that the user may be enter them in
their system's customary case without quotation. See \S 19.2.2.1.2 of
the Common Lisp standard (`Case in Pathname Components') for more on
customary case.

Note that |invert-string| is its own inverse, and that each of them have
|nil| as a fixpoint.

@l
(defun invert-string (string &aux (string (and string (string string))))
  "The universal translator for all your lettercase needs."
  (cond ((null string) nil)
        ((every (lambda (character)
                  (if (alpha-char-p character) ;
                      (upper-case-p character) ;
                      t))
                string)
         (string-downcase string))
        ((every (lambda (character)
                  (if (alpha-char-p character) ;
                      (lower-case-p character) ;
                      t))
                string)
         (string-upcase string))
        (t string)))

(defun internalize-string (string &optional (customary-case :lower))
  (when string
    (ecase customary-case
      (:lower (invert-string string))
      (:upper (string string)))))

(defun externalize-string (string &optional (customary-case :upper))
  (when string
    (ecase customary-case
      (:lower (invert-string string))
      (:upper (string string)))))

@t@l
(deftest internalize/externalize-string
  (loop for (input internal external)
        in '(("ZEBRA" "zebra" "ZEBRA")
             ("Zebra" "Zebra" "Zebra")
             ("zebra" "ZEBRA" "zebra")
             ("" "" "")
             (nil nil nil))
        always (and (equal (internalize-string input) internal)
                    (equal (externalize-string input) external)))
  t)

@ |defcmd| is the primary command defining form. Its surface syntax is
similar to that of |defun|, and in fact it consists of little more than a
wrapper around that macro. The primary difference is that the lambda list
provided to |defcmd| is not an ordinary lambda list: it supports a special
syntax for defining argument readers for the optional and required
parameters of the command function.

Our lambda list parsing routine returns two values: an ordinary lambda
list containing all of the supplied parameter specifiers (including any
|&rest| and |&key| specifiers), and a list of reader forms. If a reader
form is not supplied with a parameter, |nil| will be used as a placeholder
in the list of forms, and we'll default to using |read-preserving-whitespace|
as the argument reader for that parameter.

Note that the argument reader thunks are constructed in the null lexical
environment. Note also that because the command name is interned in the
command package, the block established by |defun| is probably not named
what you think it is, which means that in the body of a command defined
using |(defcmd name)|, |(return-from name value)| probably won't do what
you want.

We also provide support for options: if |name| is a list, it must be of the
form |(name options)|, where |options| is a plist of option/value pairs.

@l
@<Define command lambda list parsing routine@>

(defmacro defcmd (name-and-options lambda-list &body body)
  "Define a command for the top-level REPL."
  (flet ((command-name (name) (intern (symbol-name name) *command-package*)))
    (destructuring-bind (name . options) (if (listp name-and-options) ;
                                             name-and-options ;
                                             (list name-and-options))
      (let ((name (command-name name)))
        (multiple-value-bind (params reader-forms) ;
            (parse-arg-readers lambda-list)
          `(progn
             (defun ,name ,params ,@body)
             (export ',name *command-package*)
             (setf (get ',name 'arg-reader-functions) ;
                   (mapcar #'make-arg-reader '(,@reader-forms)))
             ,@@<Process |defcmd| options for |name|@>))))))

(defun make-arg-reader (form)
  (if form
      (coerce `(lambda () ,form) 'function)
      #'read-preserving-whitespace))

@ As we process the command definition options, we accumulate forms to be
included in the macro expansion.

The |:alias| option sets up an alternate name for a command. The alias is
made to be external in the command package, and we set its |alias| plist
entry to the target command. We also establish a link the other way, by
recording the new alias in the target command's |aliases| plist entry.

The |:command-char| option is used to push a new pair onto the
|*command-chars*| alist, which makes |read-form| recognize the given
command character as a shortcut for this command.

@<Process |defcmd| options...@>=
(loop for (option value) on options by #'cddr
      if (eql option :alias)
        do (setq value (command-name value))
        and collect `(export ',value *command-package*)
        and collect `(setf (get ',value 'alias) ',name)
        and collect `(pushnew ',value (get ',name 'aliases))
      if (eql option :command-char)
        collect `(pushnew (cons ,value ',name) *command-chars* ;
                          :key #'car :test #'char=))

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
(defcmd help (&optional (name (internalize-string (read-stringy-argument nil)))
              &aux (command-list-format "~<~@{~A~^ ~:_~}~:>"))
  "Display help for commands."
  (let ((command (and name
                      (handler-case (resolve-aliases (find-command name))
                        (command-not-found (condition)
                          (format *command-output* "~A~%" condition))))))
    (cond ((null command)
           (format *command-output* "Commands: ~@?."
                   command-list-format (sort (available-commands) #'string<)))
          (t (format *command-output* "~A: ~A"
                     command (or (documentation command 'function) ;
                                 "No documentation available."))
             (let ((aliases (get command 'aliases)))
               (when aliases
                 (format *command-output* "~&Aliases: ~@?."
                         command-list-format (sort aliases #'string<))))))
    (values)))

@ Commands to exit are handy, especially if you're working in a package
that doesn't use the {\tt SB-EXT} package or if you can't remember the
distinction between `exit' and~`quit'.

@l
(defcmd (exit :alias quit) ()
  "Terminate the Lisp process."
  (exit))

@ Lisp programs usually use |*default-pathname-defaults*| to resolve
relative file names, and so don't much care about the {\sc posix} current
working directory. But it matters to any sub-processes we may spawn (even
Lisp interpreters, since |*default-pathname-defaults*| is typically
initialized from the current working directory), and so it's nice to keep
them in sync.

@l
(defcmd cd (&optional (pathname (read-stringy-argument nil)) &aux
            (pathname (if pathname
                          (make-pathname :directory (pathname-directory ;
                                                     (truename pathname)))
                          (user-homedir-pathname))))
  "Change the current working directory and set default pathname defaults."
  (when (zerop (chdir pathname))
    (setf *default-pathname-defaults* pathname)))

(defcmd pwd ()
  "Print the current working directory and default pathname defaults."
  (format *command-output* "Current working directory: ~S.~%~
                            Default pathname defaults: ~S."
          (getcwd) *default-pathname-defaults*)
  (values))

@ Building on the |cd| command just defined, we can implement a simple
directory stack {\it \'a~la\/} the C-shell. The directory stack always has
|*default-pathname-defaults*| as its implicit top element.

@l
(defvar *dirs* '()
  "The directory stack.")

(defvar *pushd-silent* nil
  "Print the directory stack after PUSHD and POPD.")

(defun maybe-dirs ()
  (unless *pushd-silent* (run-command 'dirs))
  (values))

(defcmd dirs ()
  "Print the directory stack."
  (format *command-output* "~{~A~^ ~}~%" ;
          (cons *default-pathname-defaults* *dirs*))
  (values))

(defcmd pushd (&optional (pathname (or (read-stringy-argument nil) ;
                                       (pop *dirs*))))
  "Push the working directory onto the stack and change directories.
If no pathname is given, exchange the current working directory and
the top of the directory stack."
  (let ((cwd *default-pathname-defaults*))
    (run-command 'cd pathname)
    (push cwd *dirs*))
  (maybe-dirs))

(defcmd popd ()
  "Pop the topmost entry off the directory stack and make it the current
working directory."
  (cond (*dirs* (run-command 'cd (pop *dirs*))
                (maybe-dirs))
        (t (format *command-output* "Directory stack empty.~%")
           (values))))

@ |provide| and |require| may be deprecated, but they still win over most
of the available alternatives.

@l
(defcmd reload ((name (read-stringy-argument)))
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
(defcmd package ((name (read-stringy-argument nil)) &aux
                 (name (if name
                           (internalize-string name)
                           "COMMON-LISP-USER"))
                 (package (find-package name)))
  "Change the current package."
  (if package
      (setf *package* package)
      (error "No package named ~A exists." name)))

@ These next two commands pretty-print the macro expansion of an implicitly
quoted form. The second uses SBCL's code walker to do a full code walk,
including macro expansion.

@l
(defcmd macroexpand ((form `(quote ,(read))))
  "Pretty-print the macro expansion of FORM."
  (write (macroexpand form) :stream *command-output* :escape t :pretty t)
  (values))

(defcmd walk ((form `(quote ,(read))))
  "Pretty print the full macro expansion FORM."
  (let ((*walk-form-expand-macros-p* t))
    (write (walk-form form) :stream *command-output* :escape t :pretty t))
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

@ Shell commands.

@l
(defcmd (env :command-char #\$) ((name (read-stringy-argument)))
  "Look up and return the value of an environment variable."
  (posix-getenv name))

(defcmd (shell :command-char #\!)
    ((command (string-trim '(#\Space #\Tab #\Newline) (slurp))))
  "Execute a shell command."
  (let* ((shell (or (posix-getenv "SHELL") "/bin/sh"))
         (status (process-exit-code
                  (run-program shell (list "-c" command)
                               :search t
                               :wait t
                               :output *command-output*
                               :error *command-output*))))
    (if (zerop status)
        (values)
        (error "Command \"~A\" exited with status ~D." shell status))))

@*Index.
@t*Index.

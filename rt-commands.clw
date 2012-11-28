% -*- mode: CLWEB -*-
\noinx
\font\sc=cmcsc10
\def\rt{{\sc rt}}

@*\rt\ commands. \rt\ doesn't have that many interface functions; we'll
give most of them dedicated commands.

@l
@e
(require "REPL")
@e
(in-package "REPL")

@ We have to use this sneaky import trick so that we don't get warnings
when the {\tt REPL} package is redefined; some compilers complain when the
|use-list| of a package changes, which it would if we just just said
|(use-package "SB-RT")|.

@l
@e
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "SB-RT")
  (do-external-symbols (symbol (find-package "SB-RT"))
    (import symbol)))

@ Test names are symbols, and default to the current value of |sb-rt:*test*|.

@l
(defun read-test-name ()
  (if (peek-to-newline) (read) *test*))

@ The commands themselves are just trivial wrappers around the top-level
\rt\ commands.

@l
(defcmd dt (&optional (name `(quote ,(read-test-name))))
  "Do one test (defaults to *TEST*)."
  (do-test name))

(defcmd dts ()
  "Do all tests."
  (do-tests))

(defcmd pt ()
  "Get pending tests."
  (pending-tests))

(defcmd rem-all-tests ()
  "Remove all tests."
  (rem-all-tests))

(defcmd rem-test (&optional (name `(quote ,(read-test-name))))
  "Remove one test (defaults to *TEST*)."
  (rem-test name))

% -*- mode: CLWEB -*-
\noinx
\def\CLWEB{{\tt CLWEB}}

@*\CLWEB\ commands.

@l
@e
(require "REPL")
@e
(in-package "REPL")

@ We sneakily import the external symbols of the \CLWEB\ package so that we
don't get warnings when the {\tt REPL} package is redefined; some compilers
complain when the |use-list| of a package changes, which it would if we just
just said |(use-package "CLWEB")|.

@l
@e
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "CLWEB")
  (do-external-symbols (symbol (find-package "CLWEB"))
    (import symbol)))

@ The commands are just trivial wrappers around the top-level \CLWEB\
functions.

@l
(define-simple-file-command lw
  (:documentation "Load a web."
   :prompt "Load web: "
   :function load-web))

(define-simple-file-command tf
  (:documentation "Tangle a web."
   :prompt "Tangle web: "
   :function tangle-file))

(define-simple-file-command we
  (:documentation "Weave a web."
   :prompt "Weave web: "
   :function weave))

(defcmd wt ((pathname (read-pathname-argument 'we "Weave & TeX web: ")))
  "Weave and then TeX a web."
  (let ((tex-file (clweb:weave pathname)))
    (when tex-file
      (run "tex" tex-file))))

;;;; gnuplot-command-strings.asd

(asdf:defsystem #:gnuplot-command-strings
  :serial t
  :components ((:file "package")
               (:file "gnuplot-command-strings"))
  :depends-on (:lisp-unit
	       :anaphora
	       :my-utils))


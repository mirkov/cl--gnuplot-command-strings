;;;; package.lisp

(defpackage #:gnuplot-command-strings
  (:nicknames :gcs)
  (:use #:cl :lisp-unit :anaphora)
  (:import-from :my-utils
		:matrix->nested-list)
  (:documentation "Package exports functions for generating strings
  that are to be passed to a gnuplot process"))


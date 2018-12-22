(defpackage cl-solid-asd
  (:use :cl :asdf))

(in-package :cl-solid-asd)

(defsystem "cl-solid"
  :class :package-inferred-system
  :version "0.1.0"
  :author "Frederick C Gibson, Mark Watson"
  :license "GNU General Public License Version 3"
  :depends-on ("clack"
               "lack"
               "caveman2"
               "envy"
               "cl-ppcre"
               "uiop"
	       "wilbur"
	       "cl-ppcre"
	       "parse-float"
	       "simple-date-time"
	       "cl-date-time-parser"
	       "xmls"
               ;; for @route annotation
               "cl-syntax-annot"

               ;; HTML Template
               "djula"

               ;; for DB
               "datafly"
               "sxql"
	       "clath"
	       "cl-solid/main"
	       )
  :description "Common Lisp library for the Solid framework: https://github.com/solid/solid-spec"
  :in-order-to ((test-op (test-op "cl-solid-test"))))

(in-package :cl-user)
(defpackage #:cl-solid/src/serialize
  (:use :cl
	:cl-solid/src/db
	:cl-solid/src/util
	)
  (:import-from :cl-solid/src/config
                :config)
  (:export 
	   ))

(in-package :cl-solid/src/serialize)

(defun get-triples->turtle (&key s p o g)
  "This uses the Allegrograph get-triples function https://franz.com/agraph/support/documentation/current/lisp-reference.html#function.get-triples"
  (when (?allegrograph)
    (let ((s (if s s ""))
	  (p (if p p ""))
	  (o (if o o ""))
	  (g (if g g "")))
      (server-script "get-triples-to-turtle" (list `("s" . ,s)`("p" . ,p)`("o" . ,o)`("g" . ,g))))))

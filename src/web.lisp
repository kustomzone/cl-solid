(in-package :cl-user)
(defpackage #:cl-solid/src/web
  (:use :cl
        :caveman2
        :cl-solid/src/config
        :cl-solid/src/view
        :cl-solid/src/db
	:cl-solid/src/util
	:cl-json-ld
        :datafly
        :sxql)
  (:export :*web*))
(in-package :cl-solid/src/web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defroute ("/node/:node" :METHOD :GET) (&key node)
  (let ((result ""))
    (if node
	(let* ((int (get-object node))
	       (iri (get-iri (string+ (config :namespace) int)))
	       )
	  (if int
	      (let* ((query (string+ "select ?s ?p ?o ?g where {graph ?g { ?s ?p ?o . " iri " ?p ?o . }}"))
		     (res (sparql-values query))
		     )
		(setf result (string+ result "query: "query))
		(if res
		    (let ((stream-string (get-output-stream-string (jsd-to-string (car (from-rdf (triples->nquads res)))))))
		      (setf result (string+ result " output: " stream-string)))
		    (setf result (string+ result " No Linked Data Found"))
		    ))
	      (setf result (string+ "Bad ID used" result))
	      ))
	(setf result (string+ result "Missing Node ID")))
	(format nil "~A" result)
	))

;;for turtle output, use PHP https://github.com/njh/easyrdf


(defroute "/" ()
  (render #P"index.html"))



;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))

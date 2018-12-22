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
  (:import-from :wilbur
		:node-uri
		:triple
		:triple-subject
		:triple-predicate
		:triple-object
		:node
		:db-add-triple
		:db-make-triple
		:db
		:add-namespace
		:namespaces
		:db-triples
		:query
		)
  (:export :*web*))
(in-package :cl-solid/src/web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

(defparameter *server-options* '(:accept-patch "application/sparql-update"
				 :allow "GET,HEAD"
				 :access-control-expose-headers
  "Authorization, User, Location, Link, Vary, Last-Modified, ETag, Accept-Patch, Accept-Post, Updates-Via, Allow, WAC-Allow, Content-Length, WWW-Authenticate, Accept, accept"
				 :access-control-max-age 1728000
				 :access-control-allow-methods "OPTIONS,HEAD,GET,PATCH,POST,PUT,DELETE"
				 :access-control-allow-headers "Access-Control-Allow-Headers, Origin, accept, X-Requested-With, Content-Type, Access-Control-Request-Method, Access-Control-Request-Headers, Accept, accept"
				 :access-control-allow-credentials "true"
				 :Access-Control-Allow-Origin "*"
				 :vary "Accept, Authorization, Origin"))

;;
;; Routing rules

;; Note - use route "/" to map to a single page app:  (defroute "/" ()
;; In your own app after setting to this package in your own code
;;  (in-package :cl-solid/src/web)

(defroute ("/*" :METHOD :OPTIONS) (&key splat)  ;;TODO specialize on specific resources / ACLs
  (let* ((headers (request-headers caveman2:*request*))
	 (origin (gethash "origin" headers))
	 (origin (if (stringp origin)
		     origin
		     "*")))
    (setf (response-headers caveman2:*response*) *server-options*)
    (setf (getf (response-headers caveman2:*response*) :Access-Control-Allow-Origin) origin) 
    (setf (getf (response-headers caveman2:*response*) :link)  (string+ "<" (car splat) ".acl>; rel=\"acl\""))
    (setf (response-status caveman2:*response*) 204)
					(print (response-headers caveman2:*response*))
    (render-text "ok")))

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

(in-package #:cl-user)
(defpackage #:cl-solid
  (:use #:cl
	#:cl-solid/src/util
	#:cl-solid/src/config
	#:cl-solid/src/web
	#:cl-solid/src/view
	#:cl-solid/src/db
	#:cl-solid/src/authentication
	#:cl-solid/src/make
	#:cl-solid/src/serialize
	)
  (:import-from :cl-solid/src/config
                :config)
  (:import-from :clack
                :clackup)
  (:export :start
           :stop
	   :sparql-query
	   :sparql-values
	   :get-pod-graph
	   :get-root
	   :get-well-known
	   :getk
	   :config
	   :get-graphs
	   :productionp
	   :developmentp
	   :appenv
	   :production
	   :development
	   :make-webid
	   :make-company
	   :create-new-id
	   :create-new-ids
	   :create-triple
	   :string+
	   :split-string
	   :?iri
	   :?uri
	   :get-iri
	   :get-uri
	   :get-repository
	   :get-triples->turtle
	   :?typed-literal
	   :get-literal-string
	   :string->literal
	   :literal->string
	   :term->string
	   :get-literal-type
	   :?language-literal
	   :get-literal-language
	   :triples->turtle
	   :triples->db
	   :triples->agraph
	   :triples->agraph-delete
	   :db->turtle
	   :list->string
	   :plist->json
	   :json->plist
	   :json-string->plist
	   :json-string->list
	   :json->lisp
	   :program-stream
	   :process-error
	   :db->agraph
	   :replace-all
	   :render
	   :render-json
	   :render-turtle
	   :render-text
   )
  )

(in-package :cl-solid)

(defvar *appfile-path*
  (asdf:system-relative-pathname :cl-solid #P"app.lisp"))

(defvar *handler* nil)

(defun start (&rest args &key server port debug &allow-other-keys)
  (declare (ignore server port debug))
  (when *handler*
    (restart-case (error "Server is already running.")
      (restart-server ()
        :report "Restart the server"
        (stop))))
  (setf *handler*
        (apply #'clackup *appfile-path* args)))

(defun stop ()
  (prog1
      (clack:stop *handler*)
    (setf *handler* nil)))

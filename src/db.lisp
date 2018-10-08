(in-package :cl-user)
(defpackage #:cl-solid/src/db
  (:use :cl
	:cl-solid/src/util
	)
  (:import-from :cl-solid/src/config
                :config)
  (:import-from :dexador)
  (:export :connection-settings
           :db
           :with-connection
	   :*graphdb*
	   :sparql-query
	   :sparql-values
	   ))
(in-package :cl-solid/src/db)

(defun connection-settings (&optional (db :maindb))
  (cdr (assoc db (config :databases))))

(defun db (&optional (db :maindb))
  (apply #'connect-cached (connection-settings db)))

(defmacro with-connection (conn &body body)
  `(let ((*connection* ,conn))
     ,@body))

;;graphdb

(defun sparql-query (query)
  "this will take any query as a string and send to server"
  (let* ((graphdb (config :graphdb))
	 (repository (string+ (getk :domain graphdb) ":" (getk :port graphdb) (getk :repository graphdb)))
	 )
  (handler-case
      (multiple-value-bind (result http-status response-hash uri stream)
	  (dex:post repository
		    :basic-auth `(,(getk :user graphdb) . ,(getk :password graphdb))
		    :headers (list '("content-type"."application/sparql-query")
				   '("charset"."utf-8")
				   '("accept"."application/json")
				   )
		    :content query
		    )
	(declare (ignore http-status response-hash uri))
	(close stream)
	result)
	
    (error (err) (process-error err)))))

(defun sparql-values (query)
  (let ((result (sparql-query query)))
    (if (stringp result)
	(cdr (assoc :values
		       (json-string->list result))))))

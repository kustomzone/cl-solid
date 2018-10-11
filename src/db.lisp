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
	   :sparql-query
	   :sparql-values
	   :create-triple
	   :delete-triple
	   ))

(in-package :cl-solid/src/db)

(defun connection-settings (&optional (db :maindb))
  (cdr (assoc db (config :databases))))

(defun db (&optional (db :maindb))
  (apply #'connect-cached (connection-settings db)))

(defmacro with-connection (conn &body body)
  `(let ((*connection* ,conn))
     ,@body))

;;graphdb - tested for Allegrograph

(defun get-agent-iri ()
  (if (config :namespace)
      (string+ "<" (config :namespace) "1>")
      (progn
	(format t "~%ERROR: Need to Configure :namespace in :cl-solid/src/util")
	nil)))
	
(defun get-lock-iri ()
  (if (config :namespace)
      (string+ "<" (config :namespace) "2>")
      (progn
	(format t "~%ERROR: Need to Configure :namespace in :cl-solid/src/util")
	nil)))

(defparameter *agent* (get-agent-iri) "IRI for the main Solid agent managing all PODs in this repository")
(defparameter *lock* (get-lock-iri) "IRI for the lock used to prevent mutliple id writes at the same time")


(defun initiate-solid ()
    (when (not (sparql-values (string+ "select ?o where { " *agent* " " (config :p.type) "?o .}")))	
	(create-triple *agent* (config :p.type) (config :e.agent))
	(create-triple *agent* (config :p.label) (config :agent-name))
	(create-triple *agent* (config :p.id) "2" :typed "<http://www.w3.org/2001/XMLSchema#int>" )
	(create-triple lock (config :p.type) (config :e.progress-code) )
	(create-triple lock (config :p.label) "Locked")))

(defun get-current-id ()
  (let ((values (sparql-values (string+ "select ?o where {" *agent* (config :p.id) " ?o}"))))
    (when values
      (get-object (caar values)))))

(defun create-new-id ()
  (let* ((IRI (create-new-ids))
	 (IRI (if (listp IRI)(car IRI) nil)))
    IRI))

(defun create-new-ids (&optional qty)
  ;;TODO write in lock function to prevent overlapping id requests
  (let* ((current-id (get-current-id))
	 (id-list nil)
	 (qty (if qty qty 1))
	 )
    (dotimes (i qty)(setf id-list (cons (+ 1 i current-id) id-list)))
    (when id-list
      (let* ((result (write-new-id (first id-list) current-id 0))
	     (result-lisp (if (or (stringp result)(string= (type-of result) "BOOLEAN"))
			      result
			      (json->lisp result)))
	     (final (if (equal result-lisp T)
			(reverse id-list)
			result-lisp)))
	(if final
	    (mapcar #'(lambda(x)(string+ "<" (config :namespace) (write-to-string x) ">")) final))
      ))))

(defun write-new-id (int current-id iteration)
  (if (eq iteration 5)
      (progn ;TODO trigger email notification or other that this happened
	(sleep 1)
	(write-new-id int current-id 0)
	)
      (let ((iteration (+ iteration 1)))
	(if (?locked)
	    (write-new-id int current-id iteration)
	    (when (integerp int)
	      (lock-current-id)
	      (create-triple *agent* (config :p.id) (write-to-string int) :typed (config :xsd-int))
	      (delete-triple *agent* (config :p.id) (write-to-string current-id) :typed (config :xsd-int))
	      (unlock-current-id)
	      )))))

(defun ?locked ()
  (let ((locked (sparql-values (string+ "select ?l where { " *agent* " " (config :p.status) " ?l .}"))))
    (if locked
	(when (string= (caar locked) (config :n.locked))
	    t)
	)))

(defun lock-agent ()
  (if (not (?locked))
      (let ((result (create-triple *agent* (config :p.status) (config :n.locked))))
	result
	)))

(defun unlock-agent ()
  (if (?locked)
      (let ((result (delete-triple *agent* (config :p.status) (config :n.locked))))
	result
	)))

(defun create-triple (subject predicate object &key (graph *agent*) typed)
  (modify-triple subject predicate object :graph graph :typed typed :action "INSERT"))

(defun delete-triple (subject predicate object &key (graph *agent*) typed)
  (modify-triple subject predicate object :graph graph :typed typed :action "DELETE"))

(defun modify-triple (subject predicate object &key (graph *agent*) typed action) 
  (let ((subject (get-iri subject))
	(predicate (get-iri predicate))
	(object (if (?iri typed)
		    (string+  "\"" object "\"^^" typed)
		    (if (?iri object)
			object
			(if (?uri object)
			    (get-iri object)
			    (if (stringp object)
				(string+ "\"" object "\""))))))
	(graph (if (?iri graph)
		   graph
		   (get-iri graph))))
  (when (and subject predicate object graph)
    (let ((result (sparql-query (string+ action " DATA { GRAPH " graph " { " subject " " predicate " " object " .} }"))))
      (when result (json-string->list result))))))



(defun get-object (item)
  (let ((parse (split-string #\^ item)))
    (if (= (length parse) 3)
	(let ((object (first parse))
	      (type (third parse)))
	  (cond ((string= type (config :xsd-int))
		 (parse-integer (second (split-string #\" object))))
		))
	item
	)))

(defun sparql-query (query)
  "this will take any query as a string and send to server"
  (let* ((repository (string+ (config :domain) ":" (config :port) (config :repository)))
	 )
  (handler-case
      (multiple-value-bind (result http-status response-hash uri stream)
	  (dex:post repository
		    :basic-auth `(,(config :user) . ,(config :password))
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

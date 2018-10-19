(in-package :cl-user)
(defpackage #:cl-solid/src/db
  (:use :cl
	:cl-solid/src/util
	)
  (:import-from :cl-solid/src/config
                :config
		:productionp
		:developmentp)
  (:import-from :dexador)
  (:export :connection-settings
           :db
           :with-connection
	   :sparql-query
	   :sparql-values
	   :create-triple
	   :delete-triple
	   :create-new-id
	   :get-typed-literal
	   :grow
	   :get-object
	   :*agent*
	   :initiate-solid
	   :server-script
	   :?allegrograph
	   :update-server-scripts
	   :get-repo-name
	   :get-ontologies
	   :get-profile
	   :get-root
	   :get-settings
	   :get-inbox
	   :get-card
	   :get-webid
	   :get-public
	   :get-favicon
	   :get-index
	   :get-robots
	   :get-public-index
	   :get-private-index
	   :get-preferences
	   :get-well-known
	   ))

(in-package :cl-solid/src/db)

(defun connection-settings (&optional (db :maindb))
  (cdr (assoc db (config :databases))))

(defun db (&optional (db :maindb))
  (apply #'connect-cached (connection-settings db)))

(defmacro with-connection (conn &body body)
  `(let ((*connection* ,conn))
     ,@body))


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

(defun get-server()
  (string+ (config :domain) ":" (config :port)))

(defun get-repository ()
  (string+ (get-server) (config :repository)))

(defun get-repo-name ()
  (car (last (split-string #\/ (config :repository)))))

(defun get-ontologies ()
  (get-pl-keys (config :ontology)))

(defun get-auth ()
  `(,(config :user) . ,(config :password)))

;;Solid standard items

(defun get-webid (item)
  (get-location item "/profile/card#me"))

(defun get-profile (item)
  (get-location item "/profile/"))

(defun get-card (item)
  (get-location item "/profile/card"))

(defun get-root (item)
  (get-location item "/"))

(defun get-settings (item)
  (get-location item "/settings/"))

(defun get-inbox (item)
  (get-location item "/inbox/"))

(defun get-public (item)
  (get-location item "/public/"))

(defun get-favicon (item)
  (get-location item "/favicon.ico"))

(defun get-index (item)
  (get-location item "/index.html"))

(defun get-robots (item)
  (get-location item "/robots.txt"))

(defun get-public-index (item)
  (get-location item "/settings/publicTypeIndex.ttl"))

(defun get-private-index (item)
  (get-location item "/settings/privateTypeIndex.ttl"))

(defun get-preferences (item)
  (get-location item "/settings/preferences.ttl"))

(defun get-well-known (item)
  (get-location item "/.well-known/"))

(defun get-location (item target)
  (when (and item (stringp target))
    (let ((base-uri (get-base-uri item)))
      (when base-uri
	(get-iri (string+ base-uri target))))))


;;graphdb - tested for Allegrograph

(defun ?allegrograph ()
  (when (string= (config :graphdb) "Allegrograph")
    t
    ))

(defun server-script (script arguments)
  (when (?allegrograph)
    (handler-case
	(dex:post (string+ (get-repository) "/custom/" script)  :basic-auth (get-auth) :content arguments)
      (error (err) (process-error err)))))

(defun update-server-scripts ()
  (when (?allegrograph)
    (let* ((file "serialize.lisp")
	   (rows
	    (with-open-file (stream (asdf:system-relative-pathname 'cl-solid (string+ "src/server-scripts/" file)))
	      (loop for line = (read-line stream nil)
		 while line
		 collect line)))
	   (content ""))
      (dolist (row rows)
	(setf content (string+ content (format nil "~a~%" row))))
      (print content)
      (progn
	(dex:put (string+ (get-repository) "/scripts/" file) :basic-auth (get-auth) :content content)
	(dex:put (string+ (get-server) "/initfile") :basic-auth (get-auth) :content content))	
      )))

(defun upload-ontologies ()
  (when (?allegrograph)
    (dolist (ontology (get-ontologies))
      (format t "~%Loading ~A..." ontology)
      (server-script "upload-ontology" (list `("repo" . ,(get-repo-name))
					     `("location" . ,(getf (config :ontology-ttl) ontology))
					     `("base-uri" . ,(getf (config :ontology) ontology))
					     `("graph" . ,(getf (config :ontology-uri) ontology)))))))

(defun update-index ()
  (when (?allegrograph)
    (dex:put (string+ (get-repository) "/freetext/indices/web")
	     :basic-auth (get-auth)
	     :content "{\"predicates\":[],\"indexLiterals\":true,\"indexResources\":\"short\",\"indexFields\":[\"subject\",\"predicate\",\"object\",\"graph\"],\"minimumWordSize\":3,\"stopWords\":[\"and\",\"are\",\"but\",\"for\",\"into\",\"not\",\"such\",\"that\",\"the\",\"their\",\"then\",\"there\",\"these\",\"they\",\"this\",\"was\",\"will\",\"with\"],\"wordFilters\":[],\"innerChars\":[],\"borderChars\":[],\"tokenizer\":\"default\"}")))

(defun initiate-solid ()
  "Initiates new repository on graph db - WARNING - this will delete current repostitory if it exists"
  ;;first check if desired repository exists
  (when (?allegrograph)
    (format t "~%Creating new Repository...")
    (dex:delete (get-repository) :basic-auth (get-auth))
    (dex:put (get-repository) :basic-auth (get-auth))
    (format t "~%Uploading Allegrograph Server Scripts...")
    (update-server-scripts)
    (format t "~%Uploading Solid related ontologies...")
    (upload-ontologies)
    )
  (format t "~%Creating new Repository Agent and locking ontology...")
  (when (not (sparql-values (string+ "select ?o where { " *agent* " " (config :p.type) "?o .}")))	
    (create-triple *agent* (config :p.type) (config :e.agent))
    (create-triple *agent* (config :p.label) (config :agent-name))
    (create-triple *agent* (config :p.id) "2" :typed :int )
    (create-triple *lock* (config :p.type) (config :e.progress-code) )
    (create-triple *lock* (config :p.label) "Locked"))
  (format t "~%Generating Free Text Index...")
  (update-index)
  )

(defun get-current-id ()
  (let ((values (sparql-values (string+ "select ?o where {" *agent* (config :p.id) " ?o}"))))
    (when values
      (get-object (caar values)))))

(defun create-new-id ()
  (let* ((IRI (create-new-ids))
	 (IRI (if (listp IRI)(car IRI) nil)))
    IRI))

(defun create-new-ids (&optional qty)
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
	      (lock-agent)
	      (create-triple *agent* (config :p.id) (write-to-string int) :typed :int)
	      (delete-triple *agent* (config :p.id) (write-to-string current-id) :typed :int)
	      (unlock-agent)
	      )))))

(defun ?locked ()
  (let ((locked (sparql-values (string+ "select ?l where { " *agent* " " (config :p.status) " ?l .}"))))
    (if locked
	(when (string= (caar locked) *lock*)
	    t)
	)))

(defun lock-agent ()
  (if (not (?locked))
      (let ((result (create-triple *agent* (config :p.status) *lock*)))
	result
	)))

(defun unlock-agent ()
  (if (?locked)
      (let ((result (delete-triple *agent* (config :p.status) *lock*)))
	result
	)))

(defun create-triple (subject predicate object &key (graph *agent*) typed)
  (modify-triple subject predicate object :graph graph :typed typed :action "INSERT"))

(defun delete-triple (subject predicate object &key (graph *agent*) typed)
  (modify-triple subject predicate object :graph graph :typed typed :action "DELETE"))

(defun modify-triple (subject predicate object &key (graph *agent*) typed action) 
  (let ((subject (grow subject))
	(predicate (grow predicate))
	(object (if typed
		    (get-typed-literal object typed)
		    (cond ((?iri object)
			   object)
			  ((?compact object)
			   (grow object))
			  ((stringp object)
			   (string+ "\"" object "\"")))))
	(graph (grow graph)))
    (when (and subject predicate object graph)
      (let ((result (sparql-query (string+ action " DATA { GRAPH " graph " { " subject " " predicate " " object " .} }"))))
	(when result (json-string->list result))))))

(defmethod get-typed-literal ((item string)(type string))
  (when (and (stringp item) (member type (get-pl-values (config :xsd)) :test #'string=))
    (string+  "\"" item "\"^^" (grow type))
    ))

(defmethod get-typed-literal ((item string)(type symbol))
  (get-typed-literal item (getf (config :xsd) type )))

(defmethod ?compact ((item string))
  (let ((parse (split-string #\: item)))
    (when (= (length parse) 2)
      (when (member (get-keyword (car parse)) (get-ontologies))
	t))))

(defmethod ?compact ((item t))
  nil)

(defmethod grow ((item string))
  "Expands an ontology:term string to an iri, such as foaf:maker"
  (if (get-iri item)
      (get-iri item)
      (when (?compact item)
	(let ((parse (split-string #\: item)))
	  (let ((ns (get-keyword (first parse)))
		(node (second parse)))
	    (get-iri (string+ (get-uri (getf (config :ontology) ns)) node)))))))

(defmethod grow ((item t))
  nil)

(defun get-object (item)
  (cond ((?compact item)
	 (grow item))
	(t (let ((parse (split-string #\^ item)))
	     (if (= (length parse) 3)
		 (let ((object (first parse))
		       (type (third parse)))
		   (cond ((string= type (getf (config :xsd) :int))
			  (parse-integer (second (split-string #\" object))))
			 ))
		 (string-trim '(#\") item)
		 )))))

(defmacro ag-request (url &key accept content (verb :get) (charset "utf-8") content-type)
  "This is the basic building block for http requests to the Allegrograph db"
  (when (member verb '(:get :post :delete :put :patch))
    (handler-case
	(multiple-value-bind (body status response-headers uri stream)
	    `(,(intern (symbol-name verb) "DEX") ,url
	       :basic-auth (quote (,(config :user) . ,(config :password)))
	       :headers (list (quote ("accept" . ,accept))
			      (quote ("content-type" . ,content-type))
			      (quote ("charset" . ,charset))
			      )
	       :content ,content)
	  (values body status response-headers uri stream))
      (error (err) (process-error err)))))


(defun sparql2 (query &key (accept :json))
  (let* ((repository (string+ (config :domain) ":" (config :port) (config :repository)))
	 (accept (cond ((keywordp accept)
			(getf (config :sparql-output) accept))
		       ((stringp accept)
			accept))))
    (ag-request repository
		:verb :post
		:content-type "application/sparql-query"
		:accept accept
		:content query)))
  

(defun sparql-query (query &key (output :json))
  "this will take any query as a string and send to server"
  (let* ((repository (get-repository))
	 (output (cond ((keywordp output)
			(getf (config :sparql-output) output))
		       ((stringp output)
			output)))
	 )
  (handler-case
      (multiple-value-bind (result http-status response-hash uri stream)
	  (dex:post repository
		    :basic-auth (get-auth)
		    :headers (list '("content-type"."application/sparql-query")
				   '("charset"."utf-8")
				   `("accept". ,output)
				   )
		    :content query
		    )
	(declare (ignore http-status response-hash uri))
	(close stream)
	result)
	
    (error (err) (process-error err)))))

(defun sparql-values (query)
  (let ((result (sparql-query query :output :json)))
    (if (stringp result)
	(cdr (assoc :values
		       (json-string->list result))))))

;;Need to serialize turtle - use of Allegrograph sessions and server scripting here

;;first write script to the server from library file

(defun db-session (script store command arguments &key (content-type (config :urlencoded)))
  "Triples in the form of a string"
  (when (?allegrograph)
    (handler-case
	(let ((url
	       (dex:post (string+ (get-repository) "/session")
			 :basic-auth (get-auth)
			 :headers (list `("script" . ,script)
					`("store" . ,store)
					'("charset" . "utf-8")
					`("content-type" . ,content-type))
			 )))
	  (format t "~%URL: ~A" url)
	  (when (< (length (string-left-trim (config :domain) url)) (length url))
	    (dex:post (string+ url "/custom/" command)
		      :basic-auth (get-auth)
		      :headers (list '("charset" . "utf-8")
				     `("content-type" . ,content-type))
		      :content arguments
		      )))
      (error (err) (process-error err)))))




  
								      

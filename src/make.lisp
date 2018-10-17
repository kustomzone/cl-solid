(in-package :cl-user)
(defpackage #:cl-solid/src/make
  (:use :cl
	:cl-solid/src/db
	:cl-solid/src/util
	)
  (:import-from :cl-solid/src/config
                :config)
  (:export :make-webid
	   :make-agent
	   ))

(in-package :cl-solid/src/make)

;;NOTE: This implementation is using the web-id as a named graph for all triples related to that web-id for easy full graph retrieval
;;user can select an id that is checked for uniqueness so that the resulting webid and their agent is http://userid.domain/profile/card#me


(defun make-webid (&key agent name image nickname key container)
  "minimum required webid: webid is instance of e.personal-profile-document, has p.primary-topic with a valid Agent type"
  (let* ((webid (create-new-id))
	 (agent (when webid
		  (if (get-iri agent)
		      (get-iri agent)
		      (when name
			(make-agent name :image image :nickname nickname :key key :container container :graph webid))))))
    (when (and agent webid)
      (create-triple webid (config :p.label) (get-uri webid) :graph webid :typed (config :xsd-string))
      (create-triple webid (config :p.type) (config :e.personal-profile-document) :graph webid)
      (create-triple webid (config :p.primary-topic) agent :graph webid)
      webid
      )))

(defmethod make-agent ((name string) &key (type (config :e.person)) image nickname key container graph)
  "minimum agent: p.name. Note: this function should typically not be run directly to get the web-id iri as graph value on first creation"
  (when (and name (config :agent-name) graph)
    (let ((type (get-iri type)))
      (when (member type (list (config :e.person) (config :e.group) (config :e.organization)) :test #'string=)
	(let ((agent-id (create-new-id))
	      (cont (if (get-iri container)
			(get-iri container)
			(make-container :graph graph)
			)))
	  (create-triple agent-id (config :p.type) type :graph graph)
	  (create-triple agent-id (config :p.storage) cont :graph graph)
	  (create-triple agent-id (config :p.name) name :graph graph)
	  (create-triple agent-id (config :p.label) name :graph graph)
	  (when (get-iri image) (create-triple agent-id (config :p.image) (get-iri image) :graph graph))
	  (when (stringp nickname) (create-triple agent-id (config :p.nickname) nickname :graph graph))
	  (when (get-iri key) (create-triple agent-id (config :p.key) (get-iri key) :graph graph))
	  agent-id
	  )))))

;;Containers: https://www.w3.org/TR/2015/REC-ldp-20150226/

(defun make-container (&key (type (config :e.basic-container)) (label (config :agent-name)) graph)
  (let ((container (create-new-id)))
    (when container
      (create-triple container (config :p.type) type :graph graph)
      (create-triple container (config :p.label) label :graph graph)
      container
      )))
		    

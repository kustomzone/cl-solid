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


(defun make-webid (userid &key name image nickname key email company company-name company-id)
  "minimum required webid: webid is instance of e.personal-profile-document, has p.primary-topic with a valid Agent type"
  ;;check that webid is unique
  (when (stringp userid)
    (let* ((parse-namespace (split-string #\/ (config :namespace)))
	   (base-web-id (string+ (first parse-namespace) "//" userid "." (third parse-namespace)))
	   (webid (get-webid base-web-id))
	   )
      (if (sparql-values (string+ "select ?o where {" webid " ?p ?o .}"))
	  (format t "~%ERROR: This webid already exists - try again with another userid")
	  (when webid

	    (format t "~%Creating WebID...")
	    (make-type webid (list "schema:Person" "foaf:Person") webid)
	    (when (stringp name)(create-triple webid "vcard:fn" name :graph webid :typed (getf (config :xsd) :string))
		  (create-triple webid "foaf:name" name :graph webid :typed (getf (config :xsd) :string)))
	    
	    (let ((inbox (get-inbox base-web-id))
		  (prefs (get-preferences base-web-id))
		  (account (get-root base-web-id))
		  (well-known (get-well-known base-web-id))
		  (pro (get-profile base-web-id))
		  (pub (get-public base-web-id))
		  (set (get-settings base-web-id))
		  (favicon (get-favicon base-web-id))
		  (index (get-index base-web-id))
		  (robots (get-robots base-web-id))
		  (public-index (get-public-index base-web-id))
		  (private-index (get-private-index base-web-id))
		  (card (get-card base-web-id))
		  )

	      (format t "~%Creating Profile...")
	      (make-type pro "foaf:PersonalProfileDocument" webid)
	      (create-triple pro "foaf:maker" webid :graph webid)
	      (create-triple pro "foaf:primaryTopic" webid :graph webid)
	      (make-contain pro card webid)

	      (format t "~%Creating WebID Account...")
	      (create-triple webid "ldp:inbox" inbox :graph webid)
	      (make-type inbox (list "ldp:BasicContainer" "ldp:Container") webid)

	      (create-triple webid "space:preferencesFile" prefs :graph webid)
	      (make-type prefs "space:ConfigurationFile" webid)
	      (create-triple prefs "dcterms:title" "Preferences file" :graph webid :typed (getf (config :xsd) :string))

	      (make-type account (list "ldp:BasicContainer" "ldp:Container") webid)
	      (make-contain account
			    (list well-known
				  inbox
				  pro
				  pub
				  set)
			    webid)
	      
	      (make-type well-known (list "ldp:BasicContainer" "ldp:Container" "ldp:Resource") webid)

	      (make-type favicon (list "ldp:Resource" "<http://www.w3.org/ns/iana/media-types/image/vnd.microsoft.icon#Resource>") webid)

	      (make-type inbox (list "ldp:BasicContainer" "ldp:Container" "ldp:Resource") webid)

	      (make-type index (list "<http://www.w3.org/ns/iana/media-types/text/html#Resource>" "ldp:Resource") webid)

	      (make-type pro (list "ldp:BasicContainer" "ldp:Container" "ldp:Resource") webid)

	      (make-type pub (list "ldp:BasicContainer" "ldp:Container" "ldp:Resource") webid)
	      (make-contain pub (list public-index
				      favicon
				      robots
				      index)
			    webid)

	      (make-type robots (list "<http://www.w3.org/ns/iana/media-types/text/plain#Resource>" "ldp:Resource") webid)

	      (make-type set (list "ldp:BasicContainer" "ldp:Container" "ldp:Resource") webid)
	      (make-contain set prefs webid)

	      (create-triple webid "solid:account" account :graph webid)	    
	      (create-triple webid "space:storage" account :graph webid)
	      (create-triple webid "solid:privateTypeIndex" private-index :graph webid)
	      (create-triple webid "solid:publicTypeIndex" public-index :graph webid)

	      (when (get-iri image) (create-triple webid "foaf:img" (get-iri image) :graph webid))
	      (when (stringp nickname) (create-triple webid "foaf:nick" nickname :graph webid :typed (getf (config :xsd) :string)))
	      (when (get-iri key) (create-triple webid "cert:key" (get-iri key) :graph webid))
	      (when (stringp email) (create-triple webid "foaf:mbox" (string+ "<mailto:" email ">") :graph webid))

	      (format t "~%Adding Web Access Control ACLs...")
	      (make-acl webid webid webid :owner t)
	      (make-acl account webid webid :owner t)
	      (make-acl-public pub webid)
	      (make-acl-public pro webid)
	      (make-acl inbox "foaf:Agent" webid :append t :type :agent-class)
	      (make-acl inbox webid webid :read t)

	      ;;if company is true, make a company by that name
	      
	      webid
	      ))))))

;;Containers: https://www.w3.org/TR/2015/REC-ldp-20150226/

    
(defmethod make-contain ((container string) (item string) (graph string))
  (create-triple container "ldp:contains" item :graph graph))

(defmethod make-contain ((container string) (item cons) (graph string))
  (dolist (i item)
    (make-contain container i graph)))

(defmethod make-type ((subject string)(type string)(graph string))
  (create-triple subject "rdf:type" type :graph graph))

(defmethod make-type ((subject string)(type cons)(graph string))
  (dolist (i type)
    (make-type subject i graph)))

(defun make-acl (node agent graph &key (type :agent) owner read write control append)
  "Adds autorization acl to node - Current default when :owner flagged receives :control permission unless other permissions flagged"
  ;;TODO check for exising acl on node and deal with implications / changes
  (let ((node (grow node))
	(agent (grow agent))
	(graph (grow graph)))
    (when (and node agent
	       (or read write control append owner)
	       (keywordp type))
      
      (when owner
	(when (not (member t (list read write control append)))
	  (setf control t)))
      
      (let ((auth (create-new-id)))
	(make-type auth "acl:Authorization" graph)
	
	(when read
	  (create-triple auth "acl:mode" "acl:Read" :graph graph))
	(when write
	  (create-triple auth "acl:mode" "acl:Write" :graph graph))
	(when control 
	  (create-triple auth "acl:mode" "acl:Control" :graph graph))
	(when append
	  (create-triple auth "acl:mode" "acl:Append" :graph graph))
	
	(cond ((eq type :agent)
	       (create-triple auth "acl:agent" agent :graph graph))
	      ((eq type :agent-group)
	       (create-triple auth "acl:agentGroup" agent :graph graph))
	      ((eq type :agent-class)
	       (create-triple auth "acl:agentClass" agent :graph graph)))
	
	(when owner
	  (create-triple auth "acl:owner" agent :graph graph))

	(create-triple auth "acl:accessTo" node :graph graph)))))
	      
(defun make-acl-public (node graph)
  (make-acl node "foaf:Agent" graph :read t :type :agent-class))


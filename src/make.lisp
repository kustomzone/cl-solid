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


(defun make-webid (userid &key name image nickname key email)
  "minimum required webid: webid is instance of e.personal-profile-document, has p.primary-topic with a valid Agent type"
  ;;check that webid is unique
  (when (stringp userid)
    (let* ((parse-namespace (split-string #\/ (config :namespace)))
	   (base-web-id (string+ (first parse-namespace) "//" userid "." (third parse-namespace)))
	   (profile (string+ base-web-id "/profile/card"))
	   (webid (get-iri (string+ profile "#me")))
	   (profile (get-iri profile))
	   )
      (if (sparql-values (string+ "select ?o where {" webid " ?p ?o .}"))
	  (format t "~%ERROR: This webid already exists - try again with another userid")
	  (when (and webid profile)
	    (format t "~%Creating Profile...")
	    (make-type profile "foaf:PersonalProfileDocument" webid)
	    (create-triple profile "foaf:maker" webid :graph webid)
	    (create-triple profile "foaf:primaryTopic" webid :graph webid)

	    (format t "~%Creating WebID...")
	    (make-type webid (list "schema:Person" "foaf:Person") webid)
	    (when (stringp name)(create-triple webid "vcard:fn" name :graph webid :typed (getf (config :xsd) :string))
		  (create-triple webid "foaf:name" name :graph webid :typed (getf (config :xsd) :string)))
	    
	    (format t "~%Creating WebID Account...")
	    (let ((inbox (get-iri (string+ base-web-id "//inbox/")))
		  (prefs (get-iri (string+ base-web-id "/settings/prefs.ttl")))
		  (account (get-iri (string+ base-web-id "/")))
		  (well-known (get-iri (string+ base-web-id "//.well-known/")))
		  (pro (get-iri (string+ base-web-id "//profile/")))
		  (pub (get-iri (string+ base-web-id "//public/")))
		  (set (get-iri (string+ base-web-id "//settings/")))
		  (favicon (get-iri (string+ base-web-id "//favicon.ico")))
		  (index (get-iri (string+ base-web-id "//index.html")))
		  (robots (get-iri (string+ base-web-id "//robots.txt")))
		  )
	      
	      (create-triple webid "ldp:inbox" inbox :graph webid)
	      (make-type inbox (list "ldp:BasicContainer" "ldp:Container") webid)

	      (create-triple webid "space:preferencesFile" prefs :graph webid)
	      (make-type prefs "space:ConfigurationFile" webid)
	      (create-triple prefs "dcterms:title" "Preferences file" :graph webid :typed (getf (config :xsd) :string))

	      (make-type account (list "ldp:BasicContainer" "ldp:Container") webid)
	      (make-contain account
			    (list well-known
				  favicon
				  inbox
				  index
				  pro
				  pub
				  (get-iri (string+ base-web-id "//robots.txt"))
				  set)
			    webid)
	      
	      (make-type well-known (list "ldp:BasicContainer" "ldp:Container" "ldp:Resource") webid)

	      (make-type favicon (list "ldp:Resource" "<http://www.w3.org/ns/iana/media-types/image/vnd.microsoft.icon#Resource>") webid)

	      (make-type inbox (list "ldp:BasicContainer" "ldp:Container" "ldp:Resource") webid)

	      (make-type index (list "<http://www.w3.org/ns/iana/media-types/text/html#Resource>" "ldp:Resource") webid)

	      (make-type pro (list "ldp:BasicContainer" "ldp:Container" "ldp:Resource") webid)

	      (make-type pub (list "ldp:BasicContainer" "ldp:Container" "ldp:Resource") webid)

	      (make-type robots (list "<http://www.w3.org/ns/iana/media-types/text/plain#Resource>" "ldp:Resource") webid)

	      (make-type set (list "ldp:BasicContainer" "ldp:Container" "ldp:Resource") webid)

	      (create-triple webid "solid:account" account :graph webid)	    
	      (create-triple webid "space:storage" account :graph webid)
	      (create-triple webid "solid:privateTypeIndex" (get-iri (string+ base-web-id "/settings/privateTypeIndex.ttl")) :graph webid)
	      (create-triple webid "solid:publicTypeIndex" (get-iri (string+ base-web-id "/settings/publicTypeIndex.ttl")) :graph webid)

	      (when (get-iri image) (create-triple webid "foaf:img" (get-iri image) :graph webid))
	      (when (stringp nickname) (create-triple webid "foaf:nick" nickname :graph webid :typed (getf (config :xsd) :string)))
	      (when (get-iri key) (create-triple webid "cert:key" (get-iri key) :graph webid))
	      (when (stringp email) (create-triple webid "foaf:mbox" (string+ "<mailto:" email ">") :graph webid))
	      
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



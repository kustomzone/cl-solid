(in-package :cl-user)
(defpackage #:cl-solid/src/config
  (:use :cl)
  (:import-from :envy
                :config-env-var
                :defconfig)
  (:export :config
           :*application-root*
           :*static-directory*
           :*template-directory*
           :appenv
           :developmentp
           :productionp))
(in-package :cl-solid/src/config)

(setf (config-env-var) "APP_ENV")

(defparameter *application-root*   (asdf:system-source-directory :cl-solid))
(defparameter *static-directory*   (merge-pathnames #P"static/" *application-root*))
(defparameter *template-directory* (merge-pathnames #P"templates/" *application-root*))

#| Example config

:namespace  "http://example.com/node#"   ##This namespace should be the domain that your Solid service controls. cl-solid will automatically increment newly created graph node ids up from 1 using theis namespace starting with <http://example.com/node#1> in this example

   

(defconfig |development|
    `(:domain "http://example.com" 
	      :port "10035" 
	      :repository "/repositories/Solid" 
	      :user "user"
	      :password "password"
	      :namespace "http://example.com/node#" 
	      :agent-name "Solid #1"
	      :graphdb "Allegrograph"
))

|#

(defconfig :common
    `(:databases ((:maindb :sqlite3 :database-name ":memory:"))
		 
		 :e.agent "<http://xmlns.com/foaf/0.1/Agent>"
		 :e.basic-container "<http://www.w3.org/ns/ldp#BasicContainer>"
		 :e.group "<http://xmlns.com/foaf/0.1/Group>"
		 :e.organization "<http://xmlns.com/foaf/0.1/Organization>"
		 :e.person "<http://xmlns.com/foaf/0.1/Person>"
		 :e.personal-profile-document "<http://xmlns.com/foaf/0.1/PersonalProfileDocument>"
		 :e.progress-code "<http://def.seegrid.csiro.au/isotc211/iso19115/2003/metadata#ProgressCode>"
		 :p.contains "<http://www.w3.org/ns/ldp#contains>"
		 :p.id "<http://reference.data.gov.au/def/ont/iso19160-1-address#AddressableObject.id>"
		 :p.image "<http://xmlns.com/foaf/0.1/img>"
		 :p.key "<http://www.w3.org/ns/auth/cert#key>"
		 :p.knows "<http://xmlns.com/foaf/0.1/knows>"
		 :p.label "<http://www.w3.org/2000/01/rdf-schema#label>"
		 :p.name "<http://xmlns.com/foaf/0.1/name>"
		 :p.nickname "<http://xmlns.com/foaf/0.1/nick>"
		 :p.primary-topic "<http://xmlns.com/foaf/0.1/primaryTopic>"
		 :p.status "<http://def.seegrid.csiro.au/isotc211/iso19115/2003/metadata#status>"
		 :p.storage "<http://www.w3.org/ns/pim/space#storage>"
		 :p.type "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
		 :xsd-int "<http://www.w3.org/2001/XMLSchema#int>"
		 :xsd-string "<http://www.w3.org/2001/XMLSchema#string>"
		 
		 ))

(defconfig |development|
  '())

(defconfig |production|
  '())

(defconfig |test|
  '())

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))

(defun appenv ()
  (uiop:getenv (config-env-var #.(package-name *package*))))

(defun developmentp ()
  (string= (appenv) "development"))

(defun productionp ()
  (string= (appenv) "production"))

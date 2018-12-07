(in-package :cl-user)
(defpackage #:cl-solid/src/config
  (:use :cl
	:cl-solid/src/util)
  (:import-from :envy
                :config-env-var
                :defconfig
		)
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
		 :ontology (:acl "<http://www.w3.org/ns/auth/acl#>"
				 :rdf "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>"
				 :ldp "<http://www.w3.org/ns/ldp#>"
				 :rdfs "<http://www.w3.org/2000/01/rdf-schema#>"
				 :foaf "<http://xmlns.com/foaf/0.1/>"
				 :dcterms "<http://purl.org/dc/terms/>"
				 :vann "<http://purl.org/vocab/vann/>"
				 :vs "<http://www.w3.org/2003/06/sw-vocab-status/ns#>"
				 :cert "<http://www.w3.org/ns/auth/cert#>"
				 :solid "<http://www.w3.org/ns/solid/terms#>"
				 :space "<http://www.w3.org/ns/pim/space#>"
				 :schema "<http://schema.org/>"
				 :vcard "<http://www.w3.org/2006/vcard/ns#>"
				 :org "<http://www.w3.org/ns/org#>"
				 :owl "<http://www.w3.org/2002/07/owl#>"
				 :st "<http://www.w3.org/ns/posix/stat#>"
				 :gmx "<http://graphmetrix.com/node#>"
				 :dul "<http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#>"
				 :sio "<http://semanticscience.org/resource/>"
				 :ctag "<http://commontag.org/ns#>"
				 :tet "<http://www.pdflib.com/XML/TET5/TET-5.0/>"
				 :prov "<http://www.w3.org/ns/prov#>"
				 )
		 :ontology-ttl (:acl "http://www.w3.org/ns/auth/acl"
				       :rdf "https://lov.linkeddata.es/dataset/lov/vocabs/rdf/versions/2014-02-25.n3"
				       :ldp "https://lov.linkeddata.es/dataset/lov/vocabs/ldp/versions/2015-02-26.n3"
				       :rdfs "https://lov.linkeddata.es/dataset/lov/vocabs/rdfs/versions/2014-02-25.n3"
				       :foaf "https://lov.linkeddata.es/dataset/lov/vocabs/foaf/versions/2014-01-14.n3"
				       :dcterms "https://lov.linkeddata.es/dataset/lov/vocabs/dcterms/versions/2012-06-14.n3"
				       :vann "https://lov.linkeddata.es/dataset/lov/vocabs/vann/versions/2010-06-07.n3"
				       :vs "https://lov.linkeddata.es/dataset/lov/vocabs/vs/versions/2011-12-12.n3"
				       :cert "https://lov.linkeddata.es/dataset/lov/vocabs/cert/versions/2008-11-13.n3"
				       :solid "http://www.w3.org/ns/solid/terms"
				       :space "http://www.w3.org/ns/pim/space"
				       :schema "https://lov.linkeddata.es/dataset/lov/vocabs/schema/versions/2016-08-09.n3"
				       :vcard "https://lov.linkeddata.es/dataset/lov/vocabs/vcard/versions/2014-05-22.n3"
				       :org "https://lov.linkeddata.es/dataset/lov/vocabs/org/versions/2014-04-12.n3"
				       :owl "https://lov.linkeddata.es/dataset/lov/vocabs/owl/versions/2009-11-15.n3"
				       :st "https://raw.githubusercontent.com/solid/vocab/master/posix-stat.rdf"
				       :gmx "" ;;TODO add gmx via load file
				       :dul "https://lov.linkeddata.es/dataset/lov/vocabs/dul/versions/2017-04-10.n3"
				       :sio "https://lov.linkeddata.es/dataset/lov/vocabs/sio/versions/2016-09-27.n3"
				       :ctag "https://lov.linkeddata.es/dataset/lov/vocabs/ctag/versions/2009-06-08.n3"
				       :tet "" ;;TODO
				       :prov "https://lov.linkeddata.es/dataset/lov/vocabs/prov/versions/2015-01-11.n3"
				       )
		 :ontology-uri (:acl "http://www.w3.org/ns/auth/acl"
				     :rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
				     :ldp "http://www.w3.org/ns/ldp#"
				     :rdfs "http://www.w3.org/2000/01/rdf-schema#"
				     :foaf "http://xmlns.com/foaf/0.1/"
				     :dcterms "http://purl.org/dc/terms/"
				     :vann "http://purl.org/vocab/vann/"
				     :space  "http://www.w3.org/ns/pim/space"
				     :vs "http://www.w3.org/2003/06/sw-vocab-status/ns"
				     :cert "http://www.w3.org/ns/auth/cert"
				     :solid "http://www.w3.org/ns/solid/terms"
				     :schema "http://schema.org/"
				     :vcard "http://www.w3.org/2006/vcard/ns"
				     :org "http://www.w3.org/ns/org#"
				     :owl "http://www.w3.org/2002/07/owl"
				     :st "http://www.w3.org/ns/posix/stat"
				     :gmx "http://graphmetrix.com/node#7"
				     :dul "http://www.ontologydesignpatterns.org/ont/dul/DUL.owl"
				     :sio "http://semanticscience.org/ontology/sio.owl"
				     :ctag "http://commontag.org/ns#"
				     :tet "http://www.pdflib.com/XML/TET5/TET-5.0"
				     :prov "http://www.w3.org/ns/prov#"
				     )
				    
				      

		 :e.agent "<http://xmlns.com/foaf/0.1/Agent>"
		 :e.basic-container "<http://www.w3.org/ns/ldp#BasicContainer>"
		 :e.group "<http://xmlns.com/foaf/0.1/Group>"
		 :e.organization "<http://xmlns.com/foaf/0.1/Organization>"
		 :e.progress-code "<http://def.seegrid.csiro.au/isotc211/iso19115/2003/metadata#ProgressCode>"
		 :p.contains "<http://www.w3.org/ns/ldp#contains>"
		 :p.id "<http://reference.data.gov.au/def/ont/iso19160-1-address#AddressableObject.id>"
		 :p.knows "<http://xmlns.com/foaf/0.1/knows>"
		 :p.label "<http://www.w3.org/2000/01/rdf-schema#label>"
		 :p.status "<http://def.seegrid.csiro.au/isotc211/iso19115/2003/metadata#status>"
		 :p.type "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
		 :xsd (:int "<http://www.w3.org/2001/XMLSchema#int>"
			    :string "<http://www.w3.org/2001/XMLSchema#string>"
			    :boolean "<http://www.w3.org/2001/XMLSchema#boolean>"
			    :byte "<http://www.w3.org/2001/XMLSchema#byte>"
			    :date "<http://www.w3.org/2001/XMLSchema#date>"
			    :date-time "<http://www.w3.org/2001/XMLSchema#dateTime>"
			    :decimal "<http://www.w3.org/2001/XMLSchema#decimal>"
			    :double "<http://www.w3.org/2001/XMLSchema#double>"
			    :float "<http://www.w3.org/2001/XMLSchema#float>"
			    :integer "<http://www.w3.org/2001/XMLSchema#integer>"
			    :long "<http://www.w3.org/2001/XMLSchema#long>"
			    :short "<http://www.w3.org/2001/XMLSchema#short>"
			    :time "<http://www.w3.org/2001/XMLSchema#time>"
			    :unsigned-byte "<http://www.w3.org/2001/XMLSchema#unsignedByte>"
			    :unsigned-int "<http://www.w3.org/2001/XMLSchema#unsignedInt>"
			    :unsigned-long "<http://www.w3.org/2001/XMLSchema#unsignedLong>"
			    :unsigned-short "<http://www.w3.org/2001/XMLSchema#unsignedShort>"
			    )
		 :sparql-output (:sparql+xml "application/sparql-results+xml"
					     :processed-csv "application/processed-csv"
					     :sparql+json "application/sparql-results+json"
					     :sparql+json-rows "application/sparql-results+json-rows"
					     :sparql+ttl "application/sparql-results+ttl"
					     :sparql+xml "application/sparql-results+xml"
					     :sparql+xml-rows "application/sparql-results+xml-rows"
					     :x-ag-binary-rdf "application/x-ag-binary-rdf"
					     :x-direct-upis "application/x-direct-upis"
					     :lisp "application/x-lisp-structured-expression"
					     :csv "text/csv"
					     :csv-rows "text/csv-rows"
					     :integer "text/integer"
					     :simple-csv "text/simple-csv"
					     :tab "text/tab-separated-values"
					     :tab-rows "text/tab-separated-values-rows"
					     :table "text/table"
					     :json "application/json"
					     :text "text/plain"
					     )
		 :output (:rdf/xml "application/rdf+xml"
				   
				   :n-triples "text/plain"
				   :n-quads "application/rdf+xml"
				   :trix "application/trix"
				   :n3 "application/trix"
				   :integer "application/trix"
				   :json "application/json"
				   :x-quints "application/x-quints+json")
		 :urlencoded "application/x-www-form-urlencoded"
				       
		 :export-tool "/home/graphMetrix/ag6.4.1/bin/agtool"
		 
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

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
				 :vs "<http://www.w3.org/2003/06/sw-vocab-status/>"
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
				 
				 :isoadr "<http://reference.data.gov.au/def/ont/iso19160-1-address#>"
				 :skos "<http://www.w3.org/2004/02/skos/core#>"
				 :om "<http://def.seegrid.csiro.au/isotc211/iso19156/2011/observation#>"
				 :geo "<http://www.w3.org/2003/01/geo/wgs84_pos#>"
				 :opmv "<http://purl.org/net/opmv/ns#>"
				 :biotop "<http://purl.org/biotop/biotop.owl#>"
				 :gn "<http://www.geonames.org/ontology#>"
				 :event "<http://purl.org/NET/c4dm/event.owl#>"
				 :pext "<http://www.ontotext.com/proton/protonext#>"
				 :frbr "<http://purl.org/vocab/frbr/core#>"
				 :con "<http://www.w3.org/2000/10/swap/pim/contact#>"
				 :dbpedia "<http://dbpedia.org/ontology/>"
				 :md "<http://def.seegrid.csiro.au/isotc211/iso19115/2003/metadata#>"
				 :gr "<http://purl.org/goodrelations/v1>"
				 :juso "<http://rdfs.co/juso/>"
				 :sp "<http://spinrdf.org/sp#>"
				 :rdfg "<http://www.w3.org/2004/03/trix/rdfg-1/>"
				 :bibo "<http://purl.org/ontology/bibo/>"
				 :ov "<http://open.vocab.org/terms/>"
				 :unit "<http://data.nasa.gov/qudt/owl/unit#>"
				 :quantity "<http://data.nasa.gov/qudt/owl/quantity#>"
				 :qudt "<http://data.nasa.gov/qudt/owl/qudt#>"
				 :dqv "<http://www.w3.org/ns/dqv#>"
				 :bio "<http://purl.org/vocab/bio/0.1/>"
				 :rami "<http://iais.fraunhofer.de/vocabs/rami#>"
				 :seas "<https://w3id.org/seas/>"
				 :cc "<http://creativecommons.org/ns#>"
				 :temporal "<http://franz.com/ns/allegrograph/3.0/temporal/>"
				 :xsd "<http://www.w3.org/2001/XMLSchema#>"
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
				       :isoadr "https://lov.linkeddata.es/dataset/lov/vocabs/isoadr/versions/2017-11-27.n3"
				       :skos "https://lov.linkeddata.es/dataset/lov/vocabs/skos/versions/2009-08-18.n3"
				       :om "https://lov.linkeddata.es/dataset/lov/vocabs/om/versions/2014-06-19.n3"
				       :geo "https://lov.linkeddata.es/dataset/lov/vocabs/geo/versions/2009-04-20.n3"
				       :opmv "https://lov.linkeddata.es/dataset/lov/vocabs/opmv/versions/2010-10-06.n3"
				       :biotop "https://lov.linkeddata.es/dataset/lov/vocabs/biotop/versions/2012-04-24.n3"
				       :gn "https://lov.linkeddata.es/dataset/lov/vocabs/gn/versions/2012-10-29.n3"
				       :event "https://lov.linkeddata.es/dataset/lov/vocabs/event/versions/2007-10-25.n3"
				       :pext "https://lov.linkeddata.es/dataset/lov/vocabs/pext/versions/2014-01-28.n3"
				       :frbr "https://lov.linkeddata.es/dataset/lov/vocabs/frbr/versions/2009-05-16.n3"
				       :con "https://lov.linkeddata.es/dataset/lov/vocabs/con/versions/2011-02-01.n3"
				       :dbpedia "https://lov.linkeddata.es/dataset/lov/vocabs/dbpedia-owl/versions/2016-05-21.n3"
				       :md "https://lov.linkeddata.es/dataset/lov/vocabs/md/versions/2014-08-28.n3"
				       :gr "https://lov.linkeddata.es/dataset/lov/vocabs/gr/versions/2011-10-01.n3"
				       :juso "https://lov.linkeddata.es/dataset/lov/vocabs/juso/versions/2015-10-02.n3"
				       :sp "https://lov.linkeddata.es/dataset/lov/vocabs/sp/versions/2013-09-13.n3"
				       :rdfg "https://lov.linkeddata.es/dataset/lov/vocabs/rdfg/versions/2004-12-31.n3"
				       :bibo "https://lov.linkeddata.es/dataset/lov/vocabs/bibo/versions/2009-11-04.n3"
				       :ov "https://lov.linkeddata.es/dataset/lov/vocabs/ov/versions/2011-11-25.n3"
				       :unit ""
				       :quantity ""
				       :qudt ""
				       :dqv "https://lov.linkeddata.es/dataset/lov/vocabs/dqv/versions/2016-08-26.n3"
				       :bio "https://lov.linkeddata.es/dataset/lov/vocabs/bio/versions/2011-06-14.n3"
				       :rami "https://lov.linkeddata.es/dataset/lov/vocabs/rami/versions/2015-11-30.n3"
				       :seas "https://lov.linkeddata.es/dataset/lov/vocabs/seas/versions/2016-05-25.n3"
				       :cc "https://lov.linkeddata.es/dataset/lov/vocabs/cc/versions/2008-03-03.n3"
				       :temporal ""
				       :xsd ""
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
				     
				     :isoadr "http://reference.data.gov.au/def/ont/iso19160-1-address"
				     :skos "http://www.w3.org/2004/02/skos/core"
				     :om "http://def.seegrid.csiro.au/isotc211/iso19156/2011/observation"
				     :geo "http://www.w3.org/2003/01/geo/wgs84_pos"
				     :opmv "http://purl.org/net/opmv/ns#"
				     :biotop "http://purl.org/biotop/biotop.owl"
				     :gn "http://www.geonames.org/ontology"
				     :event "http://purl.org/NET/c4dm/event.owl"
				     :pext "http://www.ontotext.com/proton/protonext"
				     :frbr "http://purl.org/vocab/frbr/core"
				     :con "http://www.w3.org/2000/10/swap/pim/contact"
				     :dbpedia "http://dbpedia.org/ontology/"
				     :md "http://def.seegrid.csiro.au/isotc211/iso19115/2003/metadata"
				     :gr "http://purl.org/goodrelations/v1"
				     :juso "http://rdfs.co/juso/"
				     :sp "http://spinrdf.org/sp"
				     :rdfg "http://www.w3.org/2004/03/trix/rdfg-1/"
				     :bibo "http://purl.org/ontology/bibo/"
				     :ov "http://open.vocab.org/terms"
				     :unit "http://data.qudt.org/qudt/owl/1.0.0/unit.owl"
				     :quantity "http://data.qudt.org/qudt/owl/1.0.0/unit.owl"
				     :qudt "http://data.qudt.org/qudt/owl/1.0.0/unit.owl"
				     :dqv "http://www.w3.org/ns/dqv"
				     :bio "http://purl.org/vocab/bio/0.1/"
				     :rami "https://w3id.org/i40/rami/"
				     :seas "https://w3id.org/seas/"
				     :cc "http://creativecommons.org/ns"
				     :temporal "http://franz.com/ns/allegrograph/3.0/temporal/"
				     :xsd "http://www.w3.org/2001/XMLSchema"
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

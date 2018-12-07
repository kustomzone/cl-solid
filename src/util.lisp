(in-package :cl-user)
(defpackage #:cl-solid/src/util
  (:use :cl
	:cl-json
	:alexandria
	:babel
	:quri
	:wilbur
	:cl-graph
	:cl-ppcre
	:parse-float
	:simple-date-time
	:cl-date-time-parser
	)
  (:import-from :xmls
		:parse-to-list)
  (:import-from :cl-json-ld
		)
  (:import-from :osicat
		:environment-variable)
  (:export :plist->json
	   :json->plist
	   :json->lisp
	   :json-string->plist
	   :json-string->list
	   :split-string
	   :string+
	   :getk
	   :process-error
	   :?uri
	   :?iri
	   :get-iri
	   :get-uri
	   :get-pl-values
	   :get-pl-keys
	   :triples->nquads
	   :triples->json-ld
	   :get-keyword
	   :get-base-uri
	   :print-hash
	   :format-hash
	   :production
	   :development
	   :list->string
	   :program-stream
	   :string->literal
	   :literal->string
	   :term->string
	   :replace-all
	   )
  )

(in-package :cl-solid/src/util)

(defun getk (key conf)
  (cdr (assoc key conf)))

(defun url-encode? (item)
  (when (stringp item)
    (let* ((decode (quri:url-decode item))
	   (encode (string-encode decode)))
      (when (string= item encode)
	t))))

(defun plist->json (plist)
  (json:encode-json-plist-to-string plist))

(defun json->plist (json)
  (json-string->plist
   (map 'string 'code-char json)))

(defun json-string->plist (json-string)
  (alexandria:alist-plist
   (json:decode-json-from-string json-string)))

(defun json-string->list (json-string)
  (json:decode-json-from-string json-string))

(defun json->lisp (octets)
  (let ((response-string (babel:octets-to-string octets)))
    (json:decode-json-from-string response-string)))

(defun split-string (char string)
    "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position char string :start i)
          collect (subseq string i j)
          while j))

(defun string+ (&rest strings)
  (apply #'concatenate 'string strings))

(defmethod process-error ((err string))
  (string+ "Error:" err))

(defmethod process-error ((err t))
  err)

(defmethod ?uri ((item quri.uri:uri))
  (let* ((scheme (quri:uri-scheme item))
	 (host (quri:uri-host item))
	 (dhost (if host (split-string #\. host))))
	(when (and (member scheme '("http" "https") :test #'string=)
		   (and (> (length dhost) 1)(> (length (car (last dhost))) 1)))
	  t)))

(defmethod ?uri ((item string))
    (?uri (quri:uri item)))

(defmethod ?uri ((item t))
  nil)

(defmethod ?iri ((item string))
  (when (> (length item) 2)
    (when (and (string= (subseq item 0 1) "<")
	       (string= (subseq item (- (length item) 1) (length item)) ">")
	       (?uri (subseq item 1 (- (length item) 1))))
      t)))

(defmethod ?iri ((item t))
  nil)

(defmethod get-iri ((item string))
  (if (?iri item)
      item
      (if (?uri item)
	  (string+ "<" item ">"))))

(defmethod get-iri ((item quri.uri:uri))
  (get-iri (quri:render-uri item)))

(defmethod get-iri ((item wilbur:node))
  (get-iri (get-uri item)))

(defmethod get-iri ((item t))
  nil)

(defmethod get-uri ((item quri.uri:uri))
  (quri:render-uri item))

(defmethod get-uri ((item string))
  (cond ((?uri item)
	 item)
	((?iri item)
	 (string-trim '(#\< #\>) item))
	))

(defmethod get-uri ((item wilbur:node))
  (wilbur:node-uri item))

(defmethod get-uri ((item cons))
  (mapcar #'get-uri item))

(defmethod get-uri ((item t))
  nil)

(defmethod get-base-uri ((item string))
  (when (?iri item)
    (setf item (get-uri item)))
  (get-base-uri (quri:uri item)))

(defmethod get-base-uri ((item quri.uri:uri))
  (let ((host (quri:uri-host item))
	(scheme (quri:uri-scheme item)))
    (when (and host scheme)
      (string+ scheme "://" host))))

(defmethod get-keyword ((item string))
  (intern (string-upcase item) "KEYWORD"))

(defmethod get-keyword ((item t))
  nil)

(defmethod get-pl-values ((property-list cons))
  "Returns just the values of the property list as a list, skipping the keys"
  (let ((result))
    (dotimes (pv (/ (length property-list) 2))
      (setf result (cons (nth (+ (* pv 2) 1) property-list) result)))
    result
    ))

(defmethod get-pl-values ((property-list t))
  nil)

(defmethod get-pl-keys ((property-list cons))
  "Returns just the keys of the property list as a list, skipping the values"
  (let ((result))
    (dotimes (pv (/ (length property-list) 2))
      (setf result (cons (nth (* pv 2) property-list) result)))
    result
    ))

(defmethod get-pl-keys ((property-list t))
  nil)

(defmethod triples->nquads ((triples cons))
  (let ((result "")
	(row ""))
    (dolist (triple triples)
      (dolist (trip triple)
	(setf row (string+ row trip " ")))
      (setf result (string+ result (format nil "~a .~%" row)))
      (setf row ""))
    result
    ))

(defmethod triples->nquads ((triples t))
  nil)

(defun triples->json-ld (triples)
  "triples generated from sparql-values - need to be in nquad format - output is string-output-stream (get-output-stream-string)"
  (when triples
    (cl-json-ld:jsd-to-string (car (from-rdf (triples->nquads triples))))))

(defmethod print-hash ((hash hash-table))
  (maphash (lambda (k v) (print (list k v))) hash))

(defmethod print-hash ((hash t))
  nil)

(defun format-hash (hash &key (destination t))
  (when (eq (type-of hash) 'hash-table)
    (maphash (lambda (k v) (format destination "~%~A: ~A" k v)) hash)))

(defun development ()
  (setf (environment-variable "APP_ENV") "development"))

(defun production ()
  (setf (environment-variable "APP_ENV") "production"))

(defmethod list->string ((list cons))
  (with-output-to-string (stream)
    (let ((i 0))
      (dolist (item list)
	(incf i)
	(format stream "~A" item)
	(when (< i (length list))
	  (format stream ", "))))
    stream))

(defun program-stream (program &optional args)
  (let ((process (uiop:run-program program
                                     :input :stream
                                     :output :stream
                                     :wait nil
                                     :search t)))
    (when process
      (make-two-way-stream (sb-ext:process-output process)
                           (sb-ext:process-input process)))))

(defmethod string->literal ((item null))
  )

(defmethod string->literal ((item t))
  )

(defmethod string->literal ((item string))
  "Converts a string to a typed Wilbur literal"
  ;;check if number related
  (cond ((and (?xsd-datatyped item)(?xsd-language item))
	 (multiple-value-bind (start xsd)
	     (parse-xsd-datatype item)
	   (multiple-value-bind (lit lang)
	       (parse-xsd-language start)
	     (literal lit :datatype xsd :language lang))))
	((?xsd-datatyped item)
	 (multiple-value-bind (lit xsd)
	     (parse-xsd-datatype item)
	   (literal lit :datatype xsd)))
	((?xsd-language item)
	 (multiple-value-bind (lit lang)
	     (parse-xsd-language item)
	   (literal lit :datatype !xsd:string :language lang)))
	((?xsd-integer item)
	 (literal item :datatype !xsd:integer))
	((?xsd-float item)
	 (literal item :datatype !xsd:float))
	((?xsd-boolean item)
	 (literal item :datatype !xsd:boolean))
	((?xsd-normalized-string item)
	 (literal item :datatype !xsd:normalizedString))
	((?xsd-date item)
	 (let* ((ut (parse-date-time item))
		(date (iso8601-date-string ut t)))
	   (literal date :datatype !xsd:date)))
	((?xsd-date-time item)
	 (let* ((ut (parse-date-time item))
		(date (iso8601-date-string ut)))
	   (literal date :datatype !xsd:dateTime)))
	(t
	 (literal item :datatype !xsd:string))
	)
  )


(defmethod parse-xsd-datatype ((item string))
  (let ((split (split "\\^{2}" item)))
    (when (= (length split) 2)
      (when (or (search (node-uri !xsd:) item)
		(scan "<xsd:[^>]" item))
	(values (first split)(second split))))))

(defmethod parse-xsd-language ((item string))
  (let ((split (split "@" item)))
    (when (= (length split) 2)
      (when (= (length (second split)) 2);;TODO compare against valid languages
	(values (first split)(second split))))))

(defmethod ?xsd-language ((item string))
  (when (parse-xsd-language item)
    t))

(defmethod ?xsd-datatyped ((item string))
  (when (parse-xsd-datatype item)
t))

(defmethod ?xsd-integer ((item string))
  (let ((integer (parse-integer item :junk-allowed t)))
    (and integer (string= (write-to-string integer) item))))

(defmethod ?xsd-float ((item string))
  (let ((float (parse-float item :junk-allowed t)))
    (and float (string= (write-to-string float) item))))

(defmethod ?xsd-date ((item string))
  (?xsd-date (ignore-errors (parse-date-time item))))

(defmethod ?xsd-date ((item integer))
  (when (eq (length (write-to-string item)) 10)
    (multiple-value-bind (sec min hour day month year weekday dst-p time-zone)
	(decode-universal-time item 0)
      (declare (ignore day month weekday dst-p time-zone))
      (when year
	(when (and (= sec 0)(= min 0)(= hour 0))
	  t)))))

(defmethod ?xsd-date ((item null))
  )

(defmethod ?xsd-date ((item t))
  )

(defmethod ?xsd-date-time ((item string)) ;;TODO prescreen string to rule out obvious non-dates
  (?xsd-date-time (ignore-errors (parse-date-time item))))

(defmethod ?xsd-date-time ((item integer))
  ;;TODO - fix this so that midnight datetimes don't fail
  (when (eq (length (write-to-string item)) 10)
    (multiple-value-bind (sec min hour day month year weekday dst-p time-zone)
	(decode-universal-time item 0)
      (declare (ignore day month weekday dst-p time-zone))
      (when year
	(unless (and (= sec 0)(= min 0)(= hour 0))
	  t)))))

(defmethod ?xsd-date-time ((item null))
  )

(defmethod ?xsd-date-time ((item t))
  )

(defmethod ?xsd-normalized-string ((item string))
  (let ((chars (concatenate 'list item)))
    (when (not
	   (or
	    (member #\return chars)
	    (member #\tab chars)
	    (member #\newline chars)
	    (member #\& chars)
	    (member #\< chars)
	    (member #\" chars)
	    (member #\' chars)
	    (member #\\ chars)
	    ))
      t)))

(defmethod ?xsd-boolean ((item string))
  (or (string= (string-downcase item) "true")
      (string= (string-downcase item) "false")
      ))

(defmethod literal->string ((literal literal))
  (let ((literal (literal-string literal))
	(language (literal-language literal))
	(datatype (node-uri (literal-datatype literal)))) 
    (when literal
      ;;unicode quotes
      (setf literal (string literal))
      (setf literal (replace-all literal "\"" "&quot;"))
      (setf literal (replace-all literal "'" "&apos;"))
      (setf literal (replace-all literal "\\" "&#92;"))
	(when (or language datatype)
	  (setf literal
		(if (string= (node-uri !xsd:string) datatype)
		    (format nil "'''~A'''" literal)
		    (format nil "\"~A\"" literal))))
	(when language
	  (setf literal (string+ literal (format nil "@~A" language))))
	(when datatype
	  (setf literal (string+ literal (format nil "^^~a" (get-iri datatype)))))
	literal)))

(defmethod term->string ((term literal))
  (literal->string term))

(defmethod term->string ((term node))
  (get-iri (node-uri term)))

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 
    
    
	
  


	

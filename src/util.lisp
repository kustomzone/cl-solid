(in-package :cl-user)
(defpackage #:cl-solid/src/util
  (:use :cl
	:cl-json
	:alexandria
	:babel
	:quri
	)
  (:import-from :cl-json-ld
		)
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
	   :triples->nquads
	   :triples->json-ld
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
  (when (and (string= (subseq item 0 1) "<")
	     (string= (subseq item (- (length item) 1) (length item)) ">")
	     (?uri (subseq item 1 (- (length item) 1))))
      t))

(defmethod ?iri ((item t))
  nil)

(defmethod get-iri ((item string))
  (if (?iri item)
      item
      (if (?uri item)
	  (string+ "<" item ">"))))

(defmethod get-iri ((item quri.uri:uri))
  (get-iri (quri:render-uri item)))

(defmethod get-iri ((item t))
  nil)

(defmethod get-uri ((item quri.uri:uri))
  (quri:render-uri item))

(defmethod get-uri ((item string))
  (cond ((?uri item)
	 item)
	((?iri item)
	 (string-trim '(#\< #\>) item))))

(defmethod get-uri ((item t))
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

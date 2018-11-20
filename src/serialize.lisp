(in-package :cl-user)
(defpackage #:cl-solid/src/serialize
  (:use :cl
	:cl-solid/src/db
	:cl-solid/src/util
	)
  (:import-from :cl-solid/src/config
                :config)
  (:import-from :wilbur
		:triple
		:node
		:literal
		:db-add-triple)
  (:export :get-triples->turtle
	   :triples->turtle
	   :triples->db
	   :db->turtle
	   ))

(in-package :cl-solid/src/serialize)

(defun get-triples->turtle (&key s p o g repo)
  "This uses the Allegrograph get-triples function https://franz.com/agraph/support/documentation/current/lisp-reference.html#function.get-triples"
  (when (?allegrograph)
    (let ((s (if s s ""))
	  (p (if p p ""))
	  (o (if o o ""))
	  (g (if g g ""))
	  (repo (if repo repo "")))
      (server-script "get-triples-to-turtle" (list `("repo" . ,repo)`("s" . ,s)`("p" . ,p)`("o" . ,o)`("g" . ,g))))))

(defun triples->db (triples &key db)
  "Uses Wilbur in memory rdf system and serializes to turtle"
  (let ((db (if db
		db
		(make-instance 'wilbur:db))))
    (dolist (triple triples)
      (format nil "~%~s" triple)
      (let ((trip (wilbur:triple
		   (wilbur:node (get-uri (first triple)))
		   (wilbur:node (get-uri (second triple)))
		   (let ((object (third triple)))
		     (cond ((?iri object)
			    (wilbur:node (get-uri object)))
			   ((?typed-literal object)
			    (wilbur:literal (get-literal-string object) :datatype (wilbur:node (get-uri (get-literal-type object)))))
			   ((?language-literal object)
			    (wilbur:literal (get-literal-string object) :language (get-literal-language object)))
			   (t (wilbur:literal object)))))))
	(wilbur:db-add-triple db trip)))
    db))

(defun triples->turtle (triples &key (stream t) db)
  (let ((db (triples->db triples)))
    (when db
      (db->turtle db :stream stream))))

(defmethod db->turtle ((db wilbur:db) &key stream)
  (if stream
    (wilbur::db-dump db stream db :ntriples) ;:ntriples  :rdf/xml
    (with-output-to-string (stream)
      (wilbur::db-dump db stream db :ntriples))))
  
			  
		   


#|


;;from https://github.com/lisp/de.setf.wilbur/blob/master/src/goodies/serializer.lisp
(eval-when (:compile-toplevel :load-toplevel)

  (defun strip-attributes (tag)
    (let ((i (position #\Space tag :test #'char=)))
      (if i (subseq tag 0 i) tag)))
  
  (defmacro with-open-file-output ((stream pathname) &body body)
    `(with-open-file (,stream ,pathname
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
       ,@body)))

(defmethod db-dump ((where string) what style &optional namespaces)
  (db-dump (pathname where) what style namespaces))

(defmethod db-dump ((where pathname) what style &optional namespaces)
  (with-open-file-output (stream where)
    (db-dump stream what style namespaces)))

(defmethod db-dump ((where (eql t)) what style &optional namespaces)
  (db-dump *standard-output* what style namespaces))

(defmethod db-dump ((where stream) (thing list) (style (eql :ntriples))
		    &optional namespaces)
  (declare (ignore namespaces))
(dump-as-ntriples thing where))

(defun dump-as-ntriples (triples stream)
  (let ((bnodes (make-hash-table :test #'eq))
	(index 0))
    (flet ((dump-element (element)
	     (cond ((typep element 'literal)
		    (print-literal-for-ntriples element stream)
		    (princ #\Space stream))
		   ((node-uri element)
		    (format stream "<~A> " (node-uri element)))
		   (t
		    (format stream "~A "
			    (or (gethash element bnodes)
				(setf (gethash element bnodes)
				      (format nil "_:A~S" (incf index)))))))))
    (dolist (triple triples)
      (dump-element (triple-subject triple))
      (dump-element (triple-predicate triple))
      (dump-element (triple-object triple))
      (format stream ".~%")))))

(defun escape-ntriples-char (char)
  (cdr (assoc char '((#\\ . "\\\\")
		     (#\" . "\\\"")
		     (#\Linefeed . "\\n")
		     (#\Return . "\\r")
		     (#\Tab . "\\t"))
	      :test #'char=)))

(defun escape-ntriples-string (string)
  (escape-string string #'escape-ntriples-char))

;;; --------------------------------------------------------------------------------------
;;;
;;;   CHARACTER & STRING ESCAPING
;;;

(defun escape-xml-char (char)
  (cdr (assoc char '((#\< . "&lt;")
                     (#\> . "&gt;")
		     (#\& . "&amp;")
                     (#\' . "&apos;")
                     (#\" . "&quot;"))
              :test #'char=)))

(defun escape-xml-string (string &optional (extended-chars-p #+:sbcl t #-:sbcl nil))
  (components->string (mapcar #'(lambda (c)
				  (if (< c 128)
				    (or (escape-xml-char (code-char c))
					(code-char c))
				    (format nil "&#x~X;" c)))
			      (if extended-chars-p
				(extended-string->char-codes string)
				(utf8-string->char-codes string)))))

(defun escape-json-string (string &optional (extended-chars-p #+:sbcl t #-:sbcl nil))
  (components->string (mapcar #'(lambda (c)
				  (cond ((= c 34)  "\\\"") ; double-quote
					((> c 127) (format nil "\\u~4,'0X" c))
					(t         (code-char c))))
			      (if extended-chars-p
				(extended-string->char-codes string)
				(utf8-string->char-codes string)))))

(defun escape-string (string char-escape-function)
  ;; This tries to be clever about stuff that does not need to be escaped
  (labels ((escape (s n i parts)
             (let ((j (position-if char-escape-function s :start i)))
               (cond (j (escape s n (1+ j)
                                (list* (funcall char-escape-function (char s j))
                                       (subseq s i j)
                                       parts)))
                     (parts (components->string (nreverse (cons (subseq s i) parts))))
                     (t s)))))
    (escape string (length string) 0 nil)))

#-:allegro
(defun 8bit-char-string->octets (string)
  (let ((octets nil))
    (map nil
	 #'(lambda (char)
	     (let ((c (char-code char)))
	       (cond ((< c 128)
		      (push c octets))
		     (t
		      (push (logior (ash c -6) #b11000000) octets)
		      (push (logior (logand c #b00111111) #b10000000) octets)))))
	 string)
    (nreverse (cons 0 octets))))

#-:allegro
(defun utf8-string->octets (string)
  (mapcar #'char-code (coerce string 'list)))

(defun extended-string->char-codes (string)
  (mapcar #'char-code (coerce string 'list)))
  
(defun utf8-string->char-codes (string)
  (labels ((utf8 (octets codes)
	     (dsb (&optional octet &rest octets) octets
	       (cond ((or (null octet) (zerop octet))
		      (nreverse codes))
		     ((= (logand octet #b10000000) 0)
		      (utf8 octets (cons octet codes)))
		     ((= (logand octet #b11100000) #b11000000)
		      (dsb (octet2 &rest octets) octets
			(utf8 octets
			      (cons (logior (ash (logand octet #b00011111) 6)
					    (logand octet2 #b00111111))
				    codes))))
		     ((= (logand octet #b11110000) #b11100000)
		      (dsb (octet2 octet3 &rest octets) octets
			(utf8 octets
			      (cons (logior (ash (logand octet #b00011111) 12)
					    (ash (logand octet2 #b00111111) 6)
					    (logand octet3 #b00111111))
				    codes))))
		     ((= (logand octet #b11111000) #b11110000)
		      (dsb (octet2 octet3 octet4 &rest octets) octets
			(utf8 octets
			      (cons (logior (ash (logand octet #b00011111) 18)
					    (ash (logand octet2 #b00111111) 12)
					    (ash (logand octet3 #b00111111) 6)
					    (logand octet4 #b00111111))
				    codes))))
		     (t
		      ;; This could be a hack, and I am not sure if it is correct
		      (utf8 (list* (logior (ash octet -6) #b11000000)
				   (logior (logand octet #b00111111) #b10000000)
				   octets)
			    codes))))))
    (and string
	 (utf8 #+:allegro (coerce (excl:string-to-octets string) 'list)
	       #-:allegro (utf8-string->octets string)
	       nil))))

(defun utf8-string->extended-string (string)
  (coerce (mapcar #'code-char (utf8-string->char-codes string)) 'string))

(defun components->string (components)
  (with-output-to-string (stream)
    (dolist (component components)
(princ component stream))))

|#

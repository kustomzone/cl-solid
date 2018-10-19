(in-package #:db.agraph.user)

(defun split-string (char string)
    (loop for i = 0 then (1+ j)
          as j = (position char string :start i)
          collect (subseq string i j)
          while j))

(custom-service  
 :post "r" "get-triples-to-turtle" :string ((s :string)(p :string)(o :string)(g :string))
 (let ((s (if (string= s "") nil s))
       (p (if (string= p "") nil p))
       (o (if (string= o "") nil o))
       (g (if (string= g "") nil g)))    
   (serialize-turtle (get-triples :s s :p p :o o :g g) nil)))

(custom-service  
 :post "r" "upload-ontology" :string ((repo :string)(location :string)(base-uri :string)(graph :string))
 (when (and repo location graph)
   (open-triple-store repo)
   (let ((ext  (car (last (split-string #\. location)))))
     (when ext
       (cond ((string= ext "n3")
	      (load-turtle location :graph graph))
	     ((string= ext "rdf")
	      (load-rdf/xml location :graph graph :base-uri base-uri)))))
   (close-triple-store :commit t)
 ))



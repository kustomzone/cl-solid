(in-package #:db.agraph.user)  
 
(custom-service  
 :post "r" "get-triples-to-turtle" :string ((s :string)(p :string)(o :string)(g :string))
 (let ((s (if (string= s "") nil s))
       (p (if (string= p "") nil p))
       (o (if (string= o "") nil o))
       (g (if (string= g "") nil g)))    
   (serialize-turtle (get-triples :s s :p p :o o :g g) nil)))
;;test

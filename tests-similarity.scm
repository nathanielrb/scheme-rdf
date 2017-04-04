(load "bayes.scm")

(define (terms-o-filters terms #!key (optional #f) (union #f))
  (apply conc
	 (intersperse 
	  (map (lambda (i term)
		 (format #f "~A { ?id~A stats:hasTerm \"~A\" . ?id~A stats:isba ?isba }"
			 (if optional "OPTIONAL" "")
			 i term i))
	       (list-tabulate (length terms) values)
	       terms)
	  (if union "\n UNION " "\n"))))


(define (term-occurrence-query terms)
  (format #f "
SELECT DISTINCT ?isba, COUNT(DISTINCT ?id), ~A WHERE {
  GRAPH es: {
    ?id stats:isba ?isba .

    ~A
  }
}
ORDER BY ASC(?isba)"
	  
	  (n-exprs "COUNT(DISTINCT ?id~A)" (length terms))
	  ;(apply conc (intersperse (map reify terms) ", "))
	  (terms-o-filters terms #:union #t)))

(define (isba-prob isba)
  (car
   (match-sparql-query
    ((_ . count) (_ . total))
    (isba-total-query isba)
    (/ count total))))

(define ip (memoize isba-prob))

(define (graph-naive-bayes terms)
  (sort
   (map
    (lambda (group)
      (list (first group)
	    (second group)))
    ;;(+ (log (ip (first group))) (second group))))
    (match-sparql-query
     ((_ . isba) (_ . total) (_ . counts) ...)    
     (term-occurrence-query terms)    
     (list isba (apply + counts)))) ;; (map (lambda (c) (/ c total)) counts)))))
   (lambda (a b) (> (second a) (second b)))))

(define graph-naive-bayes-best-match
  (best-match graph-naive-bayes))

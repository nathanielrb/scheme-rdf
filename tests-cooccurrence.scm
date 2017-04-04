(load "bayes.scm")

(define (terms-filters terms #!optional (optional #t))
  (apply conc
	 (intersperse 
	  (map (lambda (i term)
		 (format #f "~A { ?id stats:hasTerm ?term~A . FILTER(?term~A = \"~A\") }"
			 (if optional "OPTIONAL" "")
			 i i term))
	       (list-tabulate (length terms) values)
	       terms)
	  "\n")))


(define (term-cooccurrence-totals terms)
  (format #f "
SELECT DISTINCT COUNT(DISTINCT ?id) WHERE {
  GRAPH es: {
    ?id stats:isba ?isba .

    ~A
  }
}"
	  (terms-filters terms #f)))

(define (terms-total-count terms)
  (car
   (match-sparql-query
    ((_ . count))
    (term-cooccurrence-totals terms)
    count)))

(define ttc (memoize terms-total-count))

;; Check if isba is URI or #!
(define (isba-total-query full-isba)
  (format #f "
SELECT DISTINCT COUNT(DISTINCT ?id), COUNT(distinct ?id2) WHERE {
  GRAPH es: {
    ?id stats:isba <~A> .
    ?id2 stats:isba ?isba .

  }
}"
	  full-isba))

(define (term-cooccurrence-query terms)
  (format #f "
SELECT DISTINCT ?isba, COUNT(DISTINCT ?id), COUNT(DISTINCT ?id2), ~A WHERE {
  GRAPH es: {
    ?id stats:isba ?isba .
    ?id2 stats:isba ?isba .

    ?id stats:hasTerm ?term . FILTER(?term in (~A))

    ~A
  }
}
ORDER BY ASC(?isba) ASC(?term)"

	  (n-vars "term" (length terms))
	  (apply conc (intersperse (map reify terms) ", "))
	  (terms-filters terms)))

(define (graph-bayes terms)
  (sort
   (map
    (lambda (group)
      (list (caar group)
	    (fold + 0 (map third group))))
    (group/key car
	       (match-sparql-query
		((_ . isba) (_ . c1) (_ . c2) (_ . terms) ...)    
		(term-cooccurrence-query terms)    
		(list isba terms (/ (/ c1 c2) (ttc terms))))))
   (lambda (a b) (> (second a) (second b )))))

(define graph-bayes-best-match
  (best-match graph-bayes))


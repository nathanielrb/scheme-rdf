(require-extension sort-combinators)

(use linear-algebra memoize)

(load "import.scm")

(define max-index
  (lambda (lst)
    (let loop ((lst lst) (i 0) (max-value (car lst)) (max-index 0))
      (if (not (null? lst))
	  (if (< max-value (car lst))
	      (loop (cdr lst) (add1 i) (car lst) i)
	      (loop (cdr lst) (add1 i) max-value max-index))
	  max-index))))

(define (run-tests test-data predictor)
  (let loop ((data test-data)	     
	     (score 0))
    (if (null? data)
	(/ score (length test-data))
	(let* ((target (caar data))
	       (terms (cadar data))
	       (prediction (predictor terms)))
	  (loop (cdr data)
		(if (substring=? prediction
				 target
				 (- (string-length prediction)
				    (string-length target)))
		    (+ score 1)
		    score))))))

;;; Maximizing matches w/ Matrix Arithmetic

(define (term-counts-query terms)
  (format #f "PREFIX eu: <http://tenforce.com/eurostat/>
PREFIX terms: <http://tenforce.com/terms/>
PREFIX stats: <http://tenforce.com/stats/>

SELECT DISTINCT ?isba, ?term, (COUNT(DISTINCT ?id) as ?count), (COUNT(DISTINCT ?id2) as ?total) WHERE {
  GRAPH eu: {
    ?id stats:hasTerm ?term .
    filter(?term in (~A)) .
    ?id stats:isba ?isba .
    ?id2 stats:isba ?isba .
    ?id2 stats:hasTerm ?term2 .
  }
}
GROUP BY ?isba ?term
ORDER BY ASC(?isba) ASC(?term)"
	  (apply conc
		 (intersperse (map reify terms) ", "))))

(define (zip-edges row-labels column-labels edges)
  (let loop ((es edges)
	     (row '())
	     (rows '())
	     (cls column-labels)
	     (rls row-labels))
    (cond ((null? rls)
	   (reverse rows))
	  ((null? cls)
	   (loop es '() (cons (reverse row) rows) column-labels (cdr rls)))
	  ((null? es)
	   (loop es (cons 0 row) rows (cdr cls) rls))
	  (else (match-let (((a b c) (car es)))
	          (if (and (equal? b (car cls)) (equal? a (car rls)))
		      (loop (cdr es) (cons c row) rows (cdr cls) rls)
		      (loop es (cons 0 row) rows (cdr cls) rls)))))))

(define (edges->matrix edges)
  (let* ((edges (map (match-lambda
		      [((_ . a) (_ . b) (_ . c) (_ . total))
		       (list a b (/ c total))])
		     edges))
	 (row-labels (delete-duplicates (map car edges)))
	 (column-labels (delete-duplicates (map cadr edges))))
    (list row-labels
	  column-labels
	  (list->matrix (zip-edges row-labels column-labels edges)))))

(define (score terms)
  (match-let (((isbas terms M) (edges->matrix
				(sparql/select
				 (term-counts-query terms)))))
	     (list isbas
		   (m*v M (make-vector (length terms) 1)))))

(define (best-match terms)
  (let ((s (score terms)))
    (list-ref (car s) (max-index (vector->list (cadr s))))))

;;; Bayes on Term Co-occurrences

(define (n-vars name i)
  (apply conc
	 (intersperse
	  (list-tabulate i (lambda (i) (conc "?" name i))) ", ")))

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

(define ttc (memoize terms-total-count))

(define (graph-bayes-best-match terms)
  (let ((scores (graph-bayes terms)))
    (if (null? scores)
	"NONE"
	(caar scores))))


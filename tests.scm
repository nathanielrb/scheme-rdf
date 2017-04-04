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

(define (n-exprs expr i)
  (apply conc
	 (intersperse
	  (list-tabulate i (lambda (i) (format #f expr i))) ", ")))

(define (best-match fn)
  (lambda (terms)
    (let ((scores (fn terms)))
      (if (null? scores)
	  "NONE"
	  (caar scores)))))

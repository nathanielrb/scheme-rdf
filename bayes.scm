(require-extension sort-combinators)

(use linear-algebra)

(load "rdf.scm")


(define query1 "PREFIX eu: <http://tenforce.com/eurostat/>
PREFIX terms: <http://tenforce.com/terms/>
PREFIX stats: <http://tenforce.com/stats/>

SELECT DISTINCT ?term, ?isba, (COUNT(?gtin) as ?count) WHERE {
  GRAPH eu: {
    ?gtin stats:hasTerm ?term .
    filter(?term in (terms:lemon, terms:lime)) .
    ?gtin stats:isba ?isba 
  }
}
ORDER BY ASC(?isba) ASC(?term)")

(define (term-counts-query terms)
  (format #f "PREFIX eu: <http://tenforce.com/eurostat/>
PREFIX terms: <http://tenforce.com/terms/>
PREFIX stats: <http://tenforce.com/stats/>

SELECT DISTINCT ?term, ?isba, (COUNT(?gtin) as ?count) WHERE {
  GRAPH eu: {
    ?gtin stats:hasTerm ?term .
    filter(?term in (~A)) .
    ?gtin stats:isba ?isba 
  }
}
ORDER BY ASC(?isba) ASC(?term)"
	  (apply conc
		 (intersperse
		  (map (lambda (term) (conc "terms:" term))
		       terms)
		  ", "))))

(define (term-counts terms)
  (map term-values
       (vector->list
	(json-get 'bindings
		  (cdr
		   (json-get
		    'results
		    (sparql/select (*sparql-endpoint*) (term-counts-query terms))))))))

(define results (sparql/select (*sparql-endpoint*) query1))

(define lines (vector->list (json-get 'bindings (cdr (json-get 'results results)))))

;; TODO: generalize, match each field, convert types

(define term-values
  (match-lambda
   [(('term _ ('value . term)) ('isba _ (_ . isba)) ('count _ _ (_ . count)))
    `(,isba . (,term . ,(string->number count)))]))

(define (assoc-m a b m)
  (let ((f (assoc a m)))
    (and f (let ((g (assoc b (cdr f))))
	     (and g (cdr g))))))

(define (matrix sorted-triples)
  (let* ((alist (map (lambda (triples) (cons (caar triples) (map cdr triples)))
		     (group/key car sorted-triples)))
	 (isbas (map car alist))
	 (terms (delete-duplicates (map car (append-map cdr alist)))))
    (list isbas
	  terms
	  (list->matrix
	   (map (lambda (isba)
		  (map (lambda (term)
			 (or (assoc-m isba term alist) 0))
		       terms))
		isbas)))))
    

(define (score terms)
  (match-let (((isbas terms M) (matrix (term-counts terms))))
	     (list isbas
		   (m*v M (make-vector (length terms) 1)))))

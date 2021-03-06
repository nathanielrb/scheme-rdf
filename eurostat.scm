(load "import.scm")

(*print-queries?* #f)

(*default-graph* "http://data.europa.eu/eurostat/")

(define-namespace skos "http://www.w3.org/2004/02/skos/core#")
(define-namespace qb "http://purl.org/linked-data/cube#")
(define-namespace dct "http://purl.org/dc/terms/")
(define-namespace schema "http://schema.org/")

(define-namespace obs "http://data.europa.eu/eurostat/id/observations/")
(define-namespace e6 "http://data.europa.eu/eurostat/id/taxonomy/ECOICOP-6/concept/")
(define-namespace gt "http://data.europa.eu/eurostat/id/taxonomy/GTIN/concept/")
(define-namespace terms "http://data.europa.eu/eurostat/id/terms/")
(define-namespace eurostat "http://data.europa.eu/eurostat/ns/")

(define (uuid label)
  (->string (gensym label)))

(define (terms-split terms)
  (string-split terms ", "))

(define (insert-term-triple term)
  (let ((id (uuid "term")))
    (sparql/update (insert-triples (list (triple (terms id) (eurostat "text") term))))
    (terms id)))
  
(define (term-triple-ids terms)
  (map (lambda (term)
	 (let ((ids (query-with-vars (id)
		     (select-triples "?s" (format #f "?s eurostat:text \"~A\"" term))
		     id)))
	   (if (null? ids)
	       (insert-term-triple term)
	       (car ids))))
       terms))

(define training-triples
  (match-lambda
   ((supermarket ecoicop ecoicop6 ecoicop6-desc
		 esba esba-desc gtin gtin-desc quantity unit)
    (let ((p (obs (uuid "observation")))
	  (term-ids (term-triple-ids (terms-split gtin-desc))))
      (append
       (list
	(triple p #:a (qb "Observation"))
	(if (< (/ (random 100) 100) 0.9)
	    (triple p (eurostat "classification") (e6 ecoicop))
	    (triple p (eurostat "target-classification") (e6 ecoicop)))
	(triple p (eurostat "gtin") (gt gtin)))       
       (map (lambda (term) (triple p (eurostat "term") term))
	    term-ids))))))

(define (load-dataset path #!optional (lines #f))
  (load-csv path (compose sparql/update insert-triples training-triples) lines))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Training

(define (training-set)
  (query-with-vars (id target)
		   (select-triples "?s, ?target"
				   "?s a qb:Observation .
                                    ?s eurostat:target-classification ?target
                                    FILTER NOT EXISTS { ?s eurostat:classification ?c }")
		   (list id target)))

(define (term-cooccurrence-query id)
  (select-triples "DISTINCT ?class, COUNT(DISTINCT ?id), COUNT(DISTINCT ?term), COUNT(DISTINCT ?term2)"
		  (format #f "OPTIONAL { ?id eurostat:term ?term . <~A> eurostat:term ?term }
                              ?id eurostat:term ?term2 .
                              ?id eurostat:classification ?class "
			  id)
		  #:order-by "ASC(?class)"))

(define (class-term-cooccurrences id)
  (query-with-vars
   (class id-count shared-term-count all-term-count)
   (term-cooccurrence-query id)
   (list class id-count shared-term-count all-term-count)))

(define (run-tests training-set)
  (map (match-lambda ((id target)
		      (let ((predictions
			     (map car
				  (sort
				   (class-term-cooccurrences id)
				   (match-lambda* (((_ x a b) (_ y c d))
						   ;;(> x y))))))
						   (> (* x (/ a b)) (* y (/ c d)))))))))
			(and (not (null? predictions))
			     (member target (take-max predictions 1))))))
			     ;;(equal? target (car predictions))))))

       training-set))

(define (take-max l n)
  (if (or (null? l) (= n 0))
      '()
      (cons (car l) (take-max (cdr l) (- n 1)))))

;; (define T (training-set))
;; (define P (run-tests T))
;; (/ (length (filter values P)) (length T))

(define (run)  
  (let* ((tset (training-set))
	(predictions (run-tests tset)))
    (/ (length (filter values predictions))
       (length tset))))

(load "import.scm")

(*print-queries?* #t)

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

(define (term-cooccurrence-query id)
  (select-triples "DISTINCT ?class, COUNT(DISTINCT ?id), COUNT(DISTINCT ?term)"
		  (format #f "OPTIONAL { ?id eurostat:term ?term . <~A> eurostat:term ?term .
                              }
                              ?id eurostat:classification ?class"
			  id)
		  #:order-by "ASC(?class)"))

(define (class-term-cooccurrences id)
  (cons id
	(query-with-vars
	 (class count term-count) (term-cooccurrence-query id) (list class count term-count))))

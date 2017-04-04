;; Usage
;;
;; Load:
;; (define TD (map test-datum (load-csv path gtin-triples)))
;;
;; Test:
;; (run-tests TD graph-bayes-best-match)

(load "import.scm")
(load "tests.scm")
(load "tests-naivebayes.scm")
(load "tests-cooccurrence.scm")

(*print-queries?* #f)

(define-namespace es "http://tenforce.com/eurostat")

(define-namespace stats "http://tenforce.com/stats")

(define-namespace terms "http://tenforce.com/terms")

(define-namespace ids "http://tenforce.com/uuids")

(define path "./data/retailer470111.csv")

(define (uuid label)
  (->string (gensym label)))

(define (terms-split terms)
  (string-split terms ", "))

(define gtin-triples
  (match-lambda
   [(supermarket ecoicop isba isba-desc esba esba-desc gtin gtin-desc quantity unit)
    (let ((esba-desc-terms (terms-split esba-desc))
	  (gtin-desc-terms (terms-split gtin-desc))
	  (id (uuid 'item)))
      `(,(make-triple (ids id) (stats "gtin") (es gtin))
	,(make-triple (ids id) (stats "isba") (es isba))
	,@(join (map (lambda (term)
		       (let ((termid (uuid 'term)))
			 (list
			  (make-triple (ids id) (stats "hasTerm") term))))
		     (join (list esba-desc-terms
				 (list gtin)
				 gtin-desc-terms))))))]
   [_ '()]))

(define test-datum
  (match-lambda
   [(supermarket ecoicop isba isba-desc esba esba-desc gtin gtin-desc quantity unit)
    (list isba (append (terms-split esba-desc) (terms-split gtin-desc)))]))  




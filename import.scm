(use csv csv-string srfi-127)

(load "rdf.scm")

(define datafile (with-input-from-file "../../data/cleaneddata1-470111.csv" read-string))

;;(define datafile (with-input-from-file "../../data/short.csv" read-string))

(define parse (csv-parser #\,))

(define data
  (cdr
   (map csv-record->list (parse datafile))))

(define-namespace es "http://tenforce.com/eurostat")

(define-namespace stats "http://tenforce.com/stats")

(define-namespace terms "http://tenforce.com/terms")

(define (triples data)
  (join
   (map (lambda (datum)
	  (let ((isba (third datum))
		(esba (fifth datum))
		(esba-desc-terms (string-split (sixth datum)))
		(gtin (seventh datum))
		(gtin-desc-terms (string-split (eighth datum))))
	    `(
	      ,(make-triple (es gtin) (stats "isba") (es isba))
	      ,@(map (lambda (term) (make-triple (es gtin) (stats "hasTerm") (terms term)))
		     (join (list esba-desc-terms
				 gtin-desc-terms))))))
	data)))

(define (run m n)
  (sparql/update (*sparql-endpoint*)
		 (insert-triples (take (drop (triples data) m)
				       n)
				 (*default-graph*))))

(define retailer-descriptions-words
  (map string-split retailer-descriptions))

(define word-bank
  (delete-duplicates
   (join retailer-descriptions-words)))

(define feature-lists
  (map (lambda (desc)
	 (map (lambda (word)
		(and (memq word desc) #t))
	      word-bank))
       retailer-descriptions-words))

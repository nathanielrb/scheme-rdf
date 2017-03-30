(use csv csv-string srfi-127)

(load "rdf.scm")

(define (load path)
  (with-input-from-file path read-string))

(define datafile (with-input-from-file "../../data/cleaneddata1-470111.csv" read-string))

(define parse (csv-parser #\,))

;; Slow!

(define (load-data)
  (cdr
   (map csv-record->list (parse datafile))))

;; Lazy - dosn't work (EOF) and what is gained?

(define (lazy-load-data path)
  (let ((port (open-input-file path)))
    (generator->lseq
     (lambda ()
       (csv-record->list
	(join
	 (parse
	  (read-line
	   port))))))))

;; (define lazy-data (load-data  "../../data/cleaneddata1-470111.csv"))

(define-namespace es "http://tenforce.com/eurostat")

(define-namespace stats "http://tenforce.com/stats")

(define-namespace terms "http://tenforce.com/terms")

(define (triple datum)
  (let ((isba (third datum))
	(esba (fifth datum))
	(esba-desc-terms (string-split (sixth datum)))
	(gtin (seventh datum))
	(gtin-desc-terms (string-split (eighth datum))))
    `(
      ,(make-triple (es gtin) #:a (stats "GTIN"))
      ,(make-triple (es isba) #:a (stats "ISBA"))
      ,(make-triple (es gtin) (stats "isba") (es isba))
      ,@(join
	 (map (lambda (term)
	       (list (make-triple (es gtin) (stats "hasTerm") (terms term))
		     (make-triple (terms term) #:a (stats "Term"))))
	      (join (list esba-desc-terms
			  gtin-desc-terms)))))))

(define (triples data)
  (join
   (map triple data)))

(define (run-triple datum)
  (sparql/update (*sparql-endpoint*)
		 (insert-triples (triple datum)
				 (*default-graph*))))

(define (run data)
  (for-each run-triple data))

;; (define word-bank
;;   (delete-duplicates
;;    (join retailer-descriptions-words)))

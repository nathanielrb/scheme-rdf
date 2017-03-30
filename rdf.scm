(use srfi-13 http-client intarweb uri-common)

(define *default-graph* (make-parameter "http://tenforce.com/eurostat/"))

(define *sparql-endpoint* (make-parameter "http://localhost:8890/sparql"))

(define-syntax define-namespace
  (syntax-rules ()
    ((define-namespace name namespace)
     (define (name elt)
       (let ((ns (if (substring=?
		      namespace "/"
		      (- (string-length namespace) 1))
		     namespace
		     (conc namespace "/"))))
	 (conc ns elt))))))

(define (reify x)
  (if (keyword? x)
      (keyword->string x)
      (conc " <" x "> ")))

(define (make-triple s p o)
  (list s p o))

(define (reify-triple triple)
  (conc (reify (car triple))
	(reify (cadr triple))
	(reify (caddr triple))))

(define (reify-triples triples)
  (apply conc
	 (intersperse (map reify-triple triples)
		      ".")))

(define (insert-triples triples graph)
  (format #f "WITH ~A~%INSERT { ~A }"
	  (reify graph)
	  (reify-triples triples)))

(define (sparql/update endpoint query)
  (format #t "~%Query:~%~A" query)
  (with-input-from-request 
   (make-request method: 'POST
		 uri: (uri-reference endpoint)
		 headers: (headers '((content-type application/sparql-update))))
   query
   read-string))

(define-namespace tf "http://www.tenforce.com/books")

(define T
  (list (make-triple (tf "#book3") #:a (tf "book"))
	(make-triple (tf "#book3") (tf "author") (tf "#jim"))))

(define (test)
  (sparql/update (*sparql-endpoint*) (insert-triples T (*default-graph*))))

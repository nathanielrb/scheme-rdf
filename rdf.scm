(use srfi-13 http-client intarweb uri-common medea)

(define *default-graph* (make-parameter "http://tenforce.com/eurostat/"))

(define *sparql-endpoint* (make-parameter "http://localhost:8890/sparql"))

(define *namespaces* (make-parameter '()))

;; A namespace is a pair '("ns:" . "http://example.com/ns")

(define (register-namespace name namespace)
  (let* ((full-namespace (if (substring=?
			      namespace "/"
			      (- (string-length namespace) 1))
			     namespace
			     (conc namespace "/")))
	 (prefix (if (last-substr? name ":")
		     name
		     (conc name ":"))))
    (*namespaces* (cons (cons prefix full-namespace) (*namespaces*)))))

(define (last-substr? str substr)
  (substring=? str substr
	       (- (string-length str)
		  (string-length substr))))

(define (conc-last str substr)
  (if (last-substr? str substr)
      str
      (conc str substr)))

(define-syntax define-namespace
  (syntax-rules ()
    ((define-namespace name namespace)
     (begin
       (register-namespace (->string (quote name)) namespace)
       (define (name elt)
	 (conc (conc-last (->string (quote name)) ":")
	       elt))))))

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


;; (define array-as-list-parser
;;   (cons 'array (lambda (x) x)))

;; (json-parsers (cons array-as-list-parser (json-parsers)))

(define (expand-namespaces namespaces)
  (apply conc
	 (map (lambda (ns)
		       (format #f "PREFIX ~A <~A>~%" (car ns) (cdr ns)))
		     namespaces)))

(define (add-prefixes query)
  (format #f "~A~%~A"
	  (expand-namespaces (*namespaces*))
	  query))

(define (json-get field object)
  (cdr (assoc field object)))

(define (sparql/update endpoint query)
  (format #t "~%Query:~%~A" query)
  (with-input-from-request 
   (make-request method: 'POST
		 uri: (uri-reference endpoint)
		 headers: (headers '((content-type application/sparql-update))))
   (add-prefixes query)
   read-string))

(define (sparql/select endpoint query)
  (format #t "~%Query:~%~A~%" query)
  (let-values (((result uri response)
		(with-input-from-request 
		 (make-request method: 'POST
			       uri: (uri-reference endpoint)
			       headers: (headers '((Content-Type application/x-www-form-urlencoded)
						   (Accept application/json))))
		 `((query . ,(add-prefixes query)))
		 read-json)))
    result))

;;    (json-get 'bindings
;;	      (json-get 'results


(define-namespace tf "http://www.tenforce.com/books")

(define T
  (list (make-triple (tf "#book3") #:a (tf "book"))
	(make-triple (tf "#book3") (tf "author") (tf "#jim"))))

(define (test)
  (sparql/update (*sparql-endpoint*) (insert-triples T (*default-graph*))))

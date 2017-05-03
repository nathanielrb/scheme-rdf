(use srfi-13 http-client intarweb uri-common medea matchable)

(load "utilities.scm")

(define *default-graph* (make-parameter "http://tenforce.com/eurostat/"))

(define *sparql-endpoint* (make-parameter "http://localhost:8890/sparql"))

(define *namespaces* (make-parameter '()))

(define *print-queries?* (make-parameter #t))

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

(define-syntax define-namespace
  (syntax-rules ()
    ((define-namespace name namespace)
     (begin
       (register-namespace (->string (quote name)) namespace)
       (define (name elt)
	 (list (conc-last (->string (quote name)) ":")
	       elt))))))

(define (reify x)
  (cond ((keyword? x) (keyword->string x))
	((list? x) (apply conc x))
	((substring=? "http://" x) (conc "<" x ">"))
	(else (conc "\"" x "\""))))

(define (triple s p o)
  (list s p o))

(define (reify-triple triple)
  (conc (reify (car triple))
	" "
	(reify (cadr triple))
	" "
	(reify (caddr triple))))

(define (uri str)
  (format #f "<~A>"))

(define (reify-triples triples)
  (apply conc
	 (intersperse (map reify-triple triples)
		      " .\n  ")))

(define (insert-triples triples  #!optional (graph (*default-graph*)))
  (format #f "WITH <~A>~%INSERT {~%  ~A ~%}"
	  graph
	  (reify-triples triples)))

(define (select-triples vars statements #!key (graph (*default-graph*)) order-by)
  (let ((order-statement (if order-by
			     (format #f "~%ORDER BY ~A" order-by)
			     "")))
    (format #f "WITH <~A>~%SELECT ~A~%WHERE {~% ~A ~%} ~A"
	    graph vars statements order-statement)))

(define (expand-namespaces namespaces)
  (apply conc
	 (map (lambda (ns)
		(format #f "PREFIX ~A <~A>~%"
			(car ns) (cdr ns)))
	      namespaces)))

(define (add-prefixes query)
  (format #f "~A~%~A"
	  (expand-namespaces (*namespaces*))
	  query))

(define (sparql/update query)
  (let ((endpoint (*sparql-endpoint*)))
    (when (*print-queries?*)
	  (format #t "~%~%Query:~%~%~A" (add-prefixes query)))
    (with-input-from-request 
     (make-request method: 'POST
		   uri: (uri-reference endpoint)
		   headers: (headers '((content-type application/sparql-update))))
     (add-prefixes query)
     read-string)))

(define (sparql/select query)
  (let ((endpoint (*sparql-endpoint*)))
    (when (*print-queries?*)
	  (format #t "~%Query:~%~A~%" (add-prefixes query)))
    (let-values (((result uri response)
		  (with-input-from-request 
		   (make-request method: 'POST
				 uri: (uri-reference endpoint)
				 headers: (headers '((Content-Type application/x-www-form-urlencoded)
						     (Accept application/json))))
		   `((query . ,(add-prefixes query)))
		   read-json)))
      (unpack-bindings result))))

(define sparql-binding
  (match-lambda
   [(var (`type . type) (`value . value))
    (cons var value)]
   [(var (`type . type) (`datatype . datatype) (`value . value))
    (if (equal? datatype "http://www.w3.org/2001/XMLSchema#integer")
	(cons var (string->number value))
	(cons var value))]))

(define (unpack-bindings results)
  (map (lambda (binding)
	 (map sparql-binding binding))
	  (vector->list
	   (assoc-get 'bindings
		     (assoc-get 'results results)))))

(define-syntax match-sparql-query
  (syntax-rules ()
    ((match-sparql (vars ...) query form)
     (map (match-lambda ((vars ...) form))
	  (sparql/select query)))))

(define-syntax query-with-vars
  (syntax-rules ()
    ((match-sparql (vars ...) query form)
     (map (match-lambda (((_ . vars) ...) form))
	  (sparql/select query)))))

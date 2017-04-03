(use csv csv-string srfi-127 matchable)

(load "rdf.scm")

(define (triples data)
  (join
   (map triple data)))

(define (run-triple datum triple-maker)
  (sparql/update (insert-triples
		  (triple-maker datum)
		  (*default-graph*))))

(define parse (csv-parser #\,))

(define (lazy-data path)
  (let ((port (open-input-file path)))
    (generator->lseq
     (lambda ()
       (let ((line (read-line port)))
	 (if (eof-object? line)
	     #f
	     (csv-record->list
	      (join
	       (parse line)))))))))

(define (load-csv path triple-maker #!optional (lines #f) (test-rate 0.9))
  (let loop ((data (lazy-data path))
	     (n 0)
	     (test-data '()))
    (if (and data
	     (lseq-car data)
	     (or (not lines)
		 (< n lines)))
	(if (< (/ (random 100) 100) test-rate)
	    (begin (run-triple (lseq-car data) triple-maker)
		   (loop (lseq-cdr data)
			 (+ n 1)
			 test-data))
	    (loop (lseq-cdr data)
		  (+ n 1)
		  (cons (lseq-car data)
			test-data)))
	test-data)))


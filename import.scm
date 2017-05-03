(use csv csv-string srfi-127 matchable)

(load "rdf.scm")

(define parse (csv-parser #\,))

(define (lazy-data path)
  (let ((port (open-input-file path)))
    (generator->lseq
     (lambda ()
       (let ((line (read-line port)))
	 (if (eof-object? line)
	     (close-input-pipe port)
	     (csv-record->list
	      (join
	       (parse line)))))))))

(define (make-run-thread thunk) (thread-start! (make-thread thunk)))

(define (load-csv path fn #!optional (lines #f))
  (let loop ((data (lazy-data path))
	     (n 0)
	     (accum '()))
    (if (and data
	     (lseq-car data)
	     (or (not lines)
		 (< n lines)))
	(loop (lseq-cdr data)
	      (+ n 1)
	      (cons (make-run-thread (lambda () (fn (lseq-car data))))
		    accum))
	accum)))

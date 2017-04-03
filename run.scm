(load "eurostat.scm")

(define TD (map test-datum (load-csv path gtin-triples 3000)))

(print (run-tests TD graph-bayes-best-match))

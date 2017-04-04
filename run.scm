(load "eurostat.scm")

(*print-queries?* #f)

(define TD (map test-datum (load-csv path gtin-triples 100)))

(print (run-tests TD graph-naive-bayes-best-match))

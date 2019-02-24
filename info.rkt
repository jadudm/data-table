#lang info

(define collection "data-table")

(define deps '("db"
               "data/gvector"
               "csv-reading"
               "gregor"
               "net/url"
               "rackunit"
               "syntax/parse"
               ))

(define scribblings '(("scribblings/manual.scrbl" 
                      (multi-page)
                      )))
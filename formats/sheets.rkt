#lang racket

(provide sheet->table)

(require net/url
         "csv.rkt"
         )

(define (sheet->table name url
                      #:sanitizers [sanitizers empty])
  (csv-port->table
   name
   (get-pure-port (string->url url))
   #:sanitizers sanitizers
   ))

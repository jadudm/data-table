#lang racket

(require "../tables.rkt")

(provide (contract-out
          [select                 (-> #:columns list? #:from table? table?)]
          ))

(define (select #:columns cols #:from T)
  (let ([tn (table-name T)])
    (unless (table? T)
      (error 'select "Not a table: [ ~a ]" T))
    (define table-name
      (apply string-append
             (add-between (map (λ (o) (format "~a" o))
                               (cons tn cols)) "-")))
    (define newT (create-table table-name))
    (for ([c cols])
      (define s (get-series-by-name T (format "~a" c)))
      (add-series newT s))
    newT))
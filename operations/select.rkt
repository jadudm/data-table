#lang racket

(require "../types.rkt"
         "../tables.rkt")

(provide (contract-out
          ; [select                 (-> #:columns list? #:from data-table? data-table?)]
          [select                 (->* (data-table?) () #:rest (listof string?) data-table?)]
          ))

;(define (select #:columns cols #:from T)
(define (select T . cols)
  (let ([tn (data-table-name T)])
    (unless (data-table? T)
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
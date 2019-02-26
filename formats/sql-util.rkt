#lang racket

(require db
         "../tables.rkt"
         "../types.rkt")

(provide (contract-out
          [->str         (-> any/c string?)]
          [clean-name    (-> string? string?)]
          [read-sql      (->* (connection? string?)
                              (#:show-table-query string?
                               #:select-all-query string?
                               #:name-column number?
                               #:type-column number?
                               #:sql-type->sanitizer procedure?)
                              data-table?)
                         ]
          
          ))



(define (->str o)
  (format "~a" o))

(define (clean-name str)
  (for ([sym '("/" "\\" "-" ":" ";" "\\." "," "\\+")])
    (set! str (regexp-replace* sym str "")))
  str)

(define (read-sql conn table
                  #:show-table-query    [STQ (format "SHOW COLUMNS FROM ~a" table)]
                  #:name-column         [nc 0]
                  #:type-column         [tc 1]
                  #:select-all-query    [SAQ (format "SELECT * FROM ~a" table)]
                  #:sql-type->sanitizer [sql-type->sanitizer (λ (o) o)])
  (define T (create-table table))
  ;; Each series comes from the CQ query.
  ;; The 0th element is the name, the 1st element is the type.
  (for ([row (query-rows conn STQ)])
    (add-series! T (create-series (vector-ref row nc)
                                  (sql-type->sanitizer (vector-ref row tc))
                                  #:values empty
                                  )))
  ;; Then, insert all the rows.
  (for ([row (query-rows conn SAQ)])
    (insert T (vector->list row)))

  T
  )
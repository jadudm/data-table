#lang racket

(require db)
(require "../types.rkt"
         (prefix-in t: "../tables.rkt")
         "../syntax-wrappers.rkt"
         "../sanitizers.rkt")

(provide read-sql)

(define (->str o)
  (format "~a" o))

(define (clean-name str)
  (for ([sym '("/" "\\" "-" ":" ";" "\\." "," "\\+")])
    (set! str (regexp-replace* sym str "")))
  str)

(define (mysql-type->sanitizer t)
  (match t
    [(regexp "int")    integer-sanitizer]
    [(regexp "float")  number-sanitizer]
    [(regexp "double") number-sanitizer]
    [(regexp "text")   string-sanitizer]
    [else
     (error 'msyql-type->sanitizer
            "Cannot find a sanitizer for type [ ~a ]~n"
            t)]))
    

(define (read-sql conn table)
  (define CQ (format "SHOW COLUMNS FROM ~a" table))
  (define Q (format "SELECT * FROM ~a" table))

  
  ;; (printf "~s~n" (query-rows conn CQ))
  
  (define T (create-table table))
  ;; Each series comes from the CQ query.
  ;; The 0th element is the name, the 1st element is the type.
  (for ([row (query-rows conn CQ)])
    (add-series T (create-series (vector-ref row 0)
                                 (mysql-type->sanitizer (vector-ref row 1))
                                 #:values empty
                                 )))
  ;; Then, insert all the rows.
  (for ([row (query-rows conn Q)])
    (insert T (vector->list row)))

  T
  )


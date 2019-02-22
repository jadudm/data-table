#lang racket

;; This file provides the interface. That means the implementation
;; lives in private/, and the syntactic wrapper lives here.

(require (for-syntax syntax/parse))
(require (prefix-in t: "tables.rkt")
         (prefix-in s: "sanitizers.rkt")
         (prefix-in op: "operations/select.rkt")
         (prefix-in op: "operations/sieve.rkt")
         )

(provide
 (rename-out [t:create-table             create-table]
             [t:create-series            create-series]
             [t:add-series               add-series]
             [s:number-sanitizer         number-sanitizer]
             [t:insert                   insert]
             [op:select                  λ:select]
             [op:sieve                   λ:sieve]
             [t:get-rows                 get-rows]
             [t:get-column               get-column]
             [t:rename-table             rename-table]
             [t:table-count              table-count]
             )
 create-numeric-table
 select
 sieve
 )

;; Where should the sqlite interface live?

(define-syntax (create-numeric-table stx)
  (syntax-parse stx
    [(cnt name fields ...)
     #`(t:create-numeric-table (quote name)
                               (quote (fields ...)))]))


(define-syntax (select stx)
  (syntax-parse stx
    [(s (~alt (~seq #:column cols:id)
              (~once (~seq #:from T))) ...)
     #`(let ()
         (op:select #:columns (quasiquote (cols ...)) #:from T))
     ]))

(define-syntax (sieve stx)
  (syntax-parse stx
    [(s T
        (~alt (~seq #:using cols:id)
              (~once (~seq #:where Q:expr))) ...)
     #`(let ()
         (op:sieve T #:using (quasiquote (cols ...)) #:where (quasiquote Q)))
     ]))

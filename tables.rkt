#lang racket

;; This file provides the interface. That means the implementation
;; lives in private/, and the syntactic wrapper lives here.

(require (for-syntax syntax/parse))
(require (prefix-in t: "private/tables.rkt")
         (prefix-in s: "sanitizers.rkt")
         (prefix-in o: "private/ops.rkt")
         )

(provide
 (rename-out [t:create-table             create-table]
             [t:create-series            create-series]
             [t:add-series               add-series]
             [s:number-sanitizer         number-sanitizer]
             [t:insert                   insert]
             [t:select                   λ:select]
             [t:sieve                    λ:sieve]
             [o:get-rows                 get-rows]
             [o:get-column               get-column]
             [o:rename-table             rename-table]
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
         (t:select #:columns (quasiquote (cols ...)) #:from T))
     ]))

(define-syntax (sieve stx)
  (syntax-parse stx
    [(s T
        (~alt (~seq #:using cols:id)
              (~once (~seq #:where Q:expr))) ...)
     #`(let ()
         (t:sieve T #:using (quasiquote (cols ...)) #:where (quasiquote Q)))
     ]))

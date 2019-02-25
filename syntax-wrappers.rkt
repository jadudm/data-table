#lang racket

;; This file provides the interface. That means the implementation
;; lives in private/, and the syntactic wrapper lives here.

(require (for-syntax syntax/parse))
(require (prefix-in op: "operations/select.rkt")
         (prefix-in op: "operations/sieve.rkt")
         )

(provide create-numeric-table
         (rename-out [op:select select])
         sieve
         )

;; Where should the sqlite interface live?

(define-syntax (create-numeric-table stx)
  (syntax-parse stx
    [(cnt name fields ...)
     #`(t:create-numeric-table (quote name)
                               (quote (fields ...)))]))


#;(define-syntax (select stx)
  (syntax-parse stx
    [(s (~alt (~seq #:column cols:id)
              (~once (~seq #:from T))) ...)
     #`(let ()
         (op:select #:columns (quasiquote (cols ...)) #:from T))
     ]))

(define-syntax (sieve stx)
  (syntax-parse stx 
    [(s T Q:expr) #`(op:sieve T (quasiquote Q) #:stx Q)]))

;; ----------------------------------------------------------------- 
;; ----------------------------- TESTS -----------------------------
;; -----------------------------------------------------------------

(module+ test
   (require rackunit
            "tables.rkt"
             "sanitizers.rkt")
  (define (create-bacon-table)
    (define baconT (create-table "bacons"))
  
    (define stripsS (create-series "strips" integer-sanitizer
                                   #:values (map (λ (n) n) (range 5))))
    (define streaksS (create-series "streaks" integer-sanitizer
                                    #:values (map (λ (n) n) (range 5 10))))
    (add-series baconT stripsS)
    (add-series baconT streaksS)
    baconT)
  
  (define testT
    (let ()
      (define T (create-table "sieve-bacons"))
      (add-series
       T (create-series
          "strips" integer-sanitizer
          #:values (range 4 5)))
      (add-series
       T (create-series
          "streaks" integer-sanitizer
          #:values (range 9 10)))
      T))

  (define baconT (create-bacon-table))
  (define sieveT (sieve baconT (> strips 3)))
  ;; It turns out, passing a quoted expression is fine.
  ;; Because, I parse it. Duh. This is much cleaner.
  ;; This allows both a syntactic approach, as well as queries
  ;; that are composed dynamically.
  (define sieveT2 (sieve baconT '(> strips 3)))
  (check-equal? testT sieveT)
  (check-equal? testT sieveT2)
  
  (check-equal? #((4 9)) (get-rows sieveT))
  )
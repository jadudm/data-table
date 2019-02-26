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
      (add-series! newT s))
    newT))

;; ----------------------------------------------------------------- 
;; ----------------------------- TESTS -----------------------------
;; -----------------------------------------------------------------

(module+ test
  (require rackunit
           "../sanitizers.rkt")
  (define (create-bacon-table)
    (define baconT (create-table "bacons"))
  
    (define stripsS (create-series "strips" integer-sanitizer
                                   #:values (map (λ (n) n) (range 5))))
    (define streaksS (create-series "streaks" integer-sanitizer
                                    #:values (map (λ (n) n) (range 5 10))))
    (add-series! baconT stripsS)
    (add-series! baconT streaksS)
    baconT)
  
  (define testT
    (let ()
      (define T (create-table "bacons-streaks"))
      (add-series!
       T (create-series
          "streaks" integer-sanitizer
          #:values (range 5 10)))
      T))

  (define baconT (create-bacon-table))
  (define selectT (select baconT "streaks"))
  (check-equal? testT selectT)
  (check-equal? #((0 5) (1 6) (2 7) (3 8) (4 9))
                (get-rows baconT))
  )
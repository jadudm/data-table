#lang racket

(require "types.rkt"
         "main.rkt")
(provide (all-defined-out))

(define (create-bacon-table)
  (define baconT (create-table "bacons"))
  
  (define stripsS (create-series "strips" integer-sanitizer
                                 #:values (map (λ (n) n) (range 5))))
  (define streaksS (create-series "streaks" integer-sanitizer
                                  #:values (map (λ (n) n) (range 5 10))))
  (add-series! baconT stripsS)
  (add-series! baconT streaksS)
  baconT)

(define (create-cities-table)
  (read-gsheet "cities" "http://bit.ly/cities-csv"))

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

(define (create-smallT)
  (create-table "small"
                `((factor value)
                  ,@(for/list ([n (range 10)])
                      (list "A" n))
                  ,@(for/list ([n (range 10)])
                      (list "B" (+ n 10)))
                  ,@(for/list ([n (range 10)])
                      (list "C" (+ n 20)))
                  )))

(define (create-sleepT)
  (create-table "sleep"
                '((extra group ID)
                  (0.7   1     1)
                  (-1.6  1     2)
                  (-0.2  1     3)
                  (-1.2  1     4)
                  (-0.1  1     5)
                  (3.4   1     6)
                  (3.7   1     7)
                  (0.8   1     8)
                  (0.0   1     9)
                  (2.0   1    10)
                  (1.9   2     1)
                  (0.8   2     2)
                  (1.1   2     3)
                  (0.1   2     4)
                  (-0.1  2     5)
                  (4.4   2     6)
                  (5.5   2     7)
                  (1.6   2     8)
                  (4.6   2     9)
                  (3.4   2    10))))
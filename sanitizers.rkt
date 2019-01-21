#lang racket

(provide (all-defined-out))


;; SANITIZERS
(define (number-sanitizer lon)
  (cond
    [(not (list? lon))
     (error 'number-sanitizer "Given something that is not a list of numbers: ~n~a" lon)]
    )
  (define sanitized
    (for/list ([ndx  (length lon)]
               [elem lon])
      (cond
        [(number? elem) elem]
        [(boolean? elem) (if elem 1 0)]
        [(and (string? elem) (string->number elem))
         (string->number elem)]
        [(not (number? elem))
         (error 'number-sanitizer "Element at index [ ~a ] is not a number: [ ~a ]" ndx elem)]
        )))
  sanitized)


(define (string-sanitizer ls)
  (map (λ (o) (format "~a" o)) ls))
#lang racket

(provide (contract-out
          [number-sanitizer             (-> list? (listof number?))]
          [integer-sanitizer            (-> list? (listof integer?))]
          [string-sanitizer             (-> list? (listof string?))]
          [make-number-sanitizer        (-> number? (-> list? (listof number?)))]
          [make-datetime-sanitizer      (-> string? (-> list? (listof datetime?)))]
          ))

(require gregor)

(define (make-number-sanitizer default-value)
  (define (inner-sanitizer lon)
    (define sanitized
      (for/list ([ndx  (length lon)]
                 [elem lon])
        (cond
          [(number? elem) elem]
          [(boolean? elem) (if elem 1 0)]
          [(and (string? elem) (string->number elem))
           (string->number elem)]
          [(not (number? elem)) default-value]
          [else default-value]
          )))
    sanitized)
  inner-sanitizer)

(define (make-datetime-sanitizer pattern)
  (define (inner-datetime-sanitizer los)
    (define sanitized
      (for/list ([ndx (length los)]
                 [elem los])
        (cond
          [(with-handlers ([exn? (λ (e) (raise e) false)]) (parse-datetime elem pattern))
           (parse-datetime elem pattern)]
          [else
           (error 'datetime-sanitizer "Date poorly formed [ ~a ] at index [ ~a ]~n\t~a"
                  elem ndx)
           ])))
    sanitized)
  inner-datetime-sanitizer)
      

;; PURPOSE
;; This sanitizer attempts to turn everything into numbers.
(define (number-sanitizer lon)
  (when (not (list? lon))
    (error 'number-sanitizer "Given something that is not a list of numbers: ~n~a" lon))
  
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

;; PURPOSE
;; This sanitizer turns everything into numbers, and rounds them to the nearest integer.
(define (integer-sanitizer lon)
  (when (not (list? lon))
    (error 'integer-sanitizer "Given something that is not a list of integers ~n~a" lon))

  (define sanitized
    (for/list ([ndx  (length lon)]
               [elem lon])
      (cond
        [(number? elem) elem]
        [(boolean? elem) (if elem 1 0)]
        [(and (string? elem) (string->number elem))
         (exact-round (string->number elem))]
        [(not (and (number? elem) (integer? elem)))
         (error 'integer-sanitizer "Element at index [ ~a ] is not an integer: [ ~a ]" ndx elem)]
        )))
  sanitized)

;; This should always work.
(define (string-sanitizer ls)
  (map (λ (o) (format "~a" o)) ls))
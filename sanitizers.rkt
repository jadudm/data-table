#lang racket

(provide (contract-out
          [identity-sanitizer           (-> list? list?)]
          [number-sanitizer             (-> list? (listof (or/c number? sql-null?)))]
          [integer-sanitizer            (-> list? (listof (or/c integer? sql-null?)))]
          [string-sanitizer             (-> list? (listof (or/c string? sql-null?)))]
          [guess-sanitizer              (-> (or/c list? gvector?) (-> list? any/c))]
          [make-number-sanitizer        (-> number? (-> list? (listof number?)))]
          [make-datetime-sanitizer      (-> string? (-> list? (listof datetime?)))]
          ))

(require db
         data/gvector
         gregor
         )



;; By allowing a 90% threshold, it means that we
;; recognize when a column is *mostly* something, but some of the
;; data is bad. Therefore, we will die later in the table construction process.
(define (mostly threshold pred? ls)
  (define sum (apply + (map (λ (b) (if b 1 0)) (map pred? ls))))
  (> (/ sum (length ls)) threshold))

(define (entirely pred? ls)
  (define sum (apply + (map (λ (b) (if b 1 0)) (map pred? ls))))
  (= (/ sum (length ls)) 1))
      
(define (guess-sanitizer data)
  (cond
    [(list? data)
     (define sanitizer identity-sanitizer)
     (for ([p predicates]
           [s sanitizers])
       (when (entirely (second p) data)
         (set! sanitizer (second s))))
     sanitizer]
    [(gvector? data)
     (guess-sanitizer (gvector->list data))]
    [else
     (error 'guess-sanitizer "Bad input data format: ~a" data)]
    ))

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
;; This sanitizer passes everything through.
(define (identity-sanitizer ls)
  ls)

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
        ;; FIXME
        ;; Should I allow SQL NULL to remain?
        ;; This will only show up when importing MySQL tables.
        [(sql-null? elem) elem]
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
        [(integer? elem) elem]
        [(boolean? elem) (if elem 1 0)]
        [(and (string? elem) (string->number elem))
         (exact-round (string->number elem))]
        ;; FIXME
        ;; Should I allow SQL NULL to remain?
        ;; This will only show up when importing MySQL tables.
        [(sql-null? elem) elem]
        [(not (and (number? elem) (integer? elem)))
         (error 'integer-sanitizer "Element at index [ ~a ] is not an integer: [ ~a ]" ndx elem)]
        )))
  sanitized)

;; This should always work.
(define (string-sanitizer ls)
  (for/list ([elem ls])
    (cond
      ;; FIXME
        ;; Should I allow SQL NULL to remain?
        ;; This will only show up when importing MySQL tables.
      [(sql-null? elem) elem]
      [else (format "~a" elem)])))


;; WARNING
;; These need to be in the most general to the most specific order...
;; This may be a bad idea, but could be fixed by getting rid of the (for ...)
;; up in (guess-sanitizer ...).
;;
;; In the meantime, these get matched one at a time, and at some point,
;; the last one matched is kept.

(define (any-value? o) true)

(define predicates
  `((any-value?  ,any-value?)
    (string?     ,string?)
    (number?     ,(λ (o) (or (number? o)   (and (string? o) (string->number o)))))
    (integer?    ,(λ (o) (or (integer? o)  (and (string? o) (string->number o)))))
    ))

(define sanitizers
  `((any-value?   ,identity-sanitizer)
    (string?      ,string-sanitizer)
    (number?      ,number-sanitizer)
    (integer?     ,integer-sanitizer)
    ))
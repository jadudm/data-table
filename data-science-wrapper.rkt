#lang racket

(require data/gvector
         racket/set
         "types.rkt"
         "main.rkt"
         "operations/select.rkt")
(require (prefix-in ds: "data-science/main.rkt"))

;; Wraps $ from data-science. Expands it to allow symbols and strings.
(define ($ T ndx)
  ;; gvector
  (define s* (data-table-serieses T))
  (cond
    [(or (symbol? ndx) (string? ndx))
     (define the-ndx false)
     (for ([s s*]
           [count (range (gvector-count s*))])
       (when (equal? (format "~a" (series-name s))
                     (format "~a" ndx))
         (set! the-ndx count)))
     (cond
       [(number? the-ndx) ($ T the-ndx)]
       [else
        (error '$ "No column with name '~a'" ndx)]
     )]
    [(number? ndx)
     ;; Make sure we're within bounds
     (when (>= ndx (gvector-count s*))
       (error '$ "Index ~a larger than number of columns (~a)"
              ndx (gvector-count s*)))
     (define name
       (series-name (gvector-ref s* ndx)))
     ;; Ultimately, we use the base function 'pull.'
     (pull T name)]
    ))

(require "test-support.rkt")
;; (define citiesT (create-cities-table))

(define (->symbol o)
  (string->symbol (~a o)))

(define group-with
  (match-lambda*
    [(list (? data-table? T) (? string? factor-col) (? string? column))
     (define factors (pull T (~a factor-col)))
     (group-with T factor-col factors column)]
    [(list (? data-table? T) (? string? factor-col) (? list? factors) (? string? column))
     (define uniq (list->set factors))
     ;; Now, sieve the table on each factor.
     (define res (make-hash))
     (define fc (string->symbol (~a factor-col)))
     (for ([f uniq])
       (define Q `(equal? ,fc ,(~a f)))
       (define tempT (sieve T ,Q))
       (hash-set! res f (pull tempT (~a column)))
       )
     (for/list ([(k v) res])
       (cons (->symbol k) v))
     ]))


;; ----------------------------------------------------------------- 
;; ----------------------------- TESTS -----------------------------
;; -----------------------------------------------------------------

#;(module+ test
  (require rackunit/chk racket/set)
  (require "test-support.rkt")
  
  (define baconT  (create-bacon-table))
  (define citiesT (create-cities-table))
  (define smallT
  (create-table "small"
                `((factor value)
                  ,@(for/list ([n (range 10)])
                      (list "A" n))
                  ,@(for/list ([n (range 10)])
                      (list "B" (+ n 10)))
                  ,@(for/list ([n (range 10)])
                      (list "C" (+ n 20)))
                  )))
    
  (chk
   ;; Correctness
   ($ baconT 0)           '(0 1 2 3 4)
   ($ baconT 'streaks)    '(5 6 7 8 9)
   (length ($ citiesT 0)) 128
   ($ citiesT 0) '(41 42 46 42 43 36 49 39 34 39 48 41 37 33 37 40 26 47 41 31 44 42 44 43 42 41 38 41 46 31 38 28 32 49 46 30 43 39 32 42 33 34 36 32 37 40 44 43 39 41 33 39 27 30 47 43 32 33 40 37 44 40 40 38 39 37 42 39 47 41 43 42 32 33 44 35 32 38 47 41 41 42 32 46 27 38 35 34 33 37 37 41 32 34 29 31 40 38 36 38 38 44 44 38 39 42 44 45 29 43 38 43 33 35 41 42 43 44 37 37 39 38 45 39 50 40 40 41)
   (list->set ($ citiesT 'NS)) (list->set '("N"))

   (group-with smallT "factor" "value")
   '((B 19 18 17 16 15 14 13 12 11 10) (A 9 8 7 6 5 4 3 2 1 0) (C 29 28 27 26 25 24 23 22 21 20))
   
   (group-with smallT "factor" '("A" "B") "value")
   '((B 19 18 17 16 15 14 13 12 11 10) (A 9 8 7 6 5 4 3 2 1 0))
   
   (group-with smallT "factor" '(B C) "value")
   '((B 19 18 17 16 15 14 13 12 11 10) (C 29 28 27 26 25 24 23 22 21 20))
   
   ;; Exceptions
   #:exn ($ baconT 'not-a-column)   #rx"No column"
   #:exn ($ baconT 42)              #rx"larger than"
   )
  
  
  )
#lang racket

(require data/gvector)
(require "types.rkt")
(provide (all-defined-out))


(define (rename-table T name)
  (table name (table-serieses T)))


(define (round-to-nearest v n)
  (* (add1 (modulo v n)) n ))


(define (get-series-by-name T sname)
  (define (finder gv ndx)
    (cond
      [(>= ndx (gvector-count gv))
       (error 'get-series-by-name "No series with name [ ~a ]" sname)]
      [(equal? (series-name (gvector-ref gv ndx)) sname)
       (gvector-ref gv ndx)]
      [else
       (finder gv (add1 ndx))]))
  (finder (table-serieses T) 0))

(define (get-rows T)
  (define lor empty)
  ;; This sets up the indicies to march down the vectors
  (for/vector ([n (gvector-count (series-values (gvector-ref (table-serieses T) 0)))])
    ;; This is so I can go through each of the serieses
    (for/list ([s (table-serieses T)])
      (gvector-ref (series-values s) n))))
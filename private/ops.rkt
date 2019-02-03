#lang racket

(require data/gvector)
(require "types.rkt")
(provide (contract-out
          [rename-table            (-> table? string? table?)]
          [get-series-by-name      (-> table? string? series?)]
          [get-rows                (-> table? vector?)]
          [get-column              (-> table? string? vector?)]
          [get-series-names        (-> table? (listof string?))]
          [table-count             (-> table? number?)]
          ))


(define (rename-table T name)
  (table name (table-serieses T)))

(define (table-count T)
  (gvector-count (series-values (gvector-ref (table-serieses T) 0))))

(define (round-to-nearest v n)
  (* (add1 (modulo v n)) n ))

(define (get-series-by-name T sname)
  (define (finder gv ndx)
    (cond
      [(>= ndx (gvector-count gv))
       (error 'get-series-by-name "No series with name [ ~a ]" sname)]
      [(equal? (series-name (gvector-ref gv ndx)) sname)
       ;;(printf "Found:~n~a~n"  (gvector-ref gv ndx))
       (gvector-ref gv ndx)]
      [else
       (finder gv (add1 ndx))]))
  (finder (table-serieses T) 0))

(define (get-series-names T)
  (map series-name (gvector->list (table-serieses T))))

(define (get-rows T)
  (define lor empty)
  ;; This sets up the indicies to march down the vectors
  (for/vector ([n (gvector-count (series-values (gvector-ref (table-serieses T) 0)))])
    ;; This is so I can go through each of the serieses
    (for/list ([s (table-serieses T)])
      (gvector-ref (series-values s) n))))

(define (get-column T col)
  (define sbn (get-series-by-name T col))
  ;; (printf "Requested ~a~nGot: ~a~n" col sbn)
  (gvector->vector (series-values sbn)))

#lang racket

(provide (contract-out
          [pull           (-> data-table? string? vector?)]))

(require data/gvector
         "../types.rkt"
         "../tables.rkt")

(define (pull-helper s* name)
  (cond
    [(empty? s*)
     (error 'pull "Cannot find a column named ~a" name)]
    [(equal? (series-name (first s*)) name)
     (gvector->vector (series-values (first s*)))]
    [else
     (pull-helper (rest s*) name)]))

(define (pull T s)
  (pull-helper (gvector->list (data-table-serieses T)) s))

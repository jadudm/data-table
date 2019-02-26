#lang racket

(require data/gvector
         racket/sandbox
         "../types.rkt"
         "../tables.rkt")

(provide (contract-out
          [sieve                  (-> data-table? list? data-table?)]
          ))

(define (sieve T Q)
  (define col-ndx-map (make-hash))

  ;; This gives me the index for a given column name into the
  ;; full row of the source table. This is then used when the
  ;; query is parsed, so that the value is looked up in the data table.
  (for ([c (map string->symbol (for/list ([s (data-table-serieses T)]) (series-name s)))]
        [ndx (range (gvector-count (data-table-serieses T)))])
    (hash-set! col-ndx-map c ndx))

  (define ids (map string->symbol (for/list ([s (data-table-serieses T)]) (series-name s))))
  (define ndxs (range (gvector-count (data-table-serieses T))))
 
  ;; Now, go through each row.
  (define keep '())
  (for ([row (get-rows T)])
    ;; This seems like a bizzare way to do this.
    ;; However, I'm not thinking past the quoting. So, I'm going
    ;; to simply load the env with the values and then do the evaluation
    ;; every time. It's... hokey, but better than using eval?
    ;; It also allows actual Racket expressions to be passed as the
    ;; query, instead of just something I parse.
    (define query-expression
      `(define result
         (let (
               ,@(for/list ([c (map string->symbol
                                    (for/list ([s (data-table-serieses T)]) (series-name s)))]
                            [ndx (range (gvector-count (data-table-serieses T)))])
                   `(,c ,(list-ref row ndx)))
               )
           ,Q
           )))
    
    ;; (printf "~s~n" bindings)
    
    (define racket-evaluator
      (make-evaluator 'racket/base query-expression))
    ;; Run the query against the row. The evaluation environment
    ;; has all of the bindings in it... which, it turns out, doesn't matter.
    ;; Because I don't know how to use the raw query to 
    (when (racket-evaluator `result)
      ;; (printf "Keeping: ~a~n" row)
      (set! keep (cons row keep)))
    )

  ;; (printf "Kept: ~a~n" keep)
  ;; Add the kept data to the newT
  (define newT (create-table (format "sieve-~a" (data-table-name T))))
  (for ([s (data-table-serieses T)]
        [ndx (gvector-count (data-table-serieses T))]
        )
    ;;(define s (get-series-by-name T (format "~a" c)))
    ;; (printf "Sieved: ~a~n" s)
    (add-series! newT (create-series (series-name s)
                                          (series-sanitizer s)
                                          #:values
                                          (map (λ (r)
                                                 (list-ref r (hash-ref col-ndx-map
                                                                       (string->symbol
                                                                        (series-name s)))))
                                               (reverse keep))))
    )
  newT
  )

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
      (define T (create-table "sieve-bacons"))
      (add-series!
       T (create-series
          "strips" integer-sanitizer
          #:values (range 4 5)))
      (add-series!
       T (create-series
          "streaks" integer-sanitizer
          #:values (range 9 10)))
      T))

  (define baconT (create-bacon-table))
  (define sieveT (sieve baconT '(let ([n 3]) (> strips n))))
  (check-equal? testT sieveT)
  
  (check-equal? #((4 9)) (get-rows sieveT))
  )

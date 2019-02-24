#lang racket

(require data/gvector
         "../types.rkt"
         "../tables.rkt")

(provide (contract-out
          [sieve                  (-> data-table? #:using list? #:where list? data-table?)]
          ))

(define (parse-query Q h row)
  (match Q
    [(list) '()]
    ;; FIXME This assumes rows are lists.
    [(? symbol? o) (list-ref row (hash-ref h o false))]
    [(? string? o) o]
    [(? number? o) o]
    [(list op lhs rhs)
     (let ([plhs (parse-query lhs h row)]
           [prhs (parse-query rhs h row)])
       #`(#,op #,plhs #,prhs))]
    ))
    
(define (sieve T #:using cols #:where Q)
  (define newT (create-table (format "sieve-~a" (data-table-name T))))
  (define col-ndx-map (make-hash))

  ;; This gives me the index for a given column name into the
  ;; full row of the source table.
  (for ([c (map string->symbol (for/list ([s (data-table-serieses T)]) (series-name s)))]
        [ndx (range (gvector-count (data-table-serieses T)))])
    (hash-set! col-ndx-map c ndx))

  ;; Now, go through each row.
  (define keep '())
  (for ([row (get-rows T)])
    (define newQ (parse-query Q col-ndx-map row))
    ;; (printf "newQ: ~a~n" newQ)
    (when (eval newQ)
      ;; (printf "Keeping: ~a~n" row)
      (set! keep (cons row keep)))
    )

  ;; (printf "Kept: ~a~n" keep)
  ;; Add the kept data to the newT
  (for ([s (data-table-serieses T)]
        [ndx (gvector-count (data-table-serieses T))]
        )
    ;;(define s (get-series-by-name T (format "~a" c)))
    ;; (printf "Sieved: ~a~n" s)
    (add-series newT (create-series (series-name s)
                                    (series-sanitizer s)
                                    #:values
                                    (map (λ (r) (list-ref r (hash-ref col-ndx-map
                                                                      (string->symbol
                                                                       (series-name s)))))
                                         (reverse keep))))
    )
  newT
  )
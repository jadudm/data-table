#lang racket


(provide (contract-out
          [create-table          (-> (or/c symbol? string?) any/c)]
          [create-numeric-table  (-> (or/c symbol? string?) (list/c (or/c symbol? string?)) any)]
          [create-series         (-> (or/c symbol? string?)
                                     (-> any/c any)
                                     #:values list?
                                     series?)]
          [add-series            (-> table? series? table?)]
          [insert                (case->
                                  (-> table? series? list? any)
                                  (-> table? list? any)
                                  (-> table? series? any)
                                  (-> table? #:rest any/c any))]
          [select                 (-> #:columns list? #:from table? table?)]
          [sieve                  (-> table? #:using list? #:where list? table?)]
          ))

(require data/gvector
         "ops.rkt"
         "../sanitizers.rkt"
         (for-syntax syntax/parse)
         "types.rkt")

;; CONSTANTS
(define default-gvector-length 100)

(define (valid-table-name? fun name)
  (unless (regexp-match "[a-zA-Z0-9_]" name)
    (error fun
           (string-append "Valid names for tables contain letters, numbers, and the underscore (the _ symbol). Nothing else.~n"
                          "\tYou provided: ~a" name))))

(define (valid-field-name? fun name)
  (unless (regexp-match "[a-zA-Z0-9_]" name)
    (error fun
           (string-append "Valid column names for tables contain letters, numbers, and the underscore (the _ symbol). Nothing else.~n"
                          "\tYou provided: ~a" name))))
;; PURPOSE
;; Creates a table structure. Makes sure the name conforms
;; to SQL naming conventions, so that we can save to an SQLite file.
(define (create-table name)
  (define name-string (~a name))
  (valid-table-name? 'create-table name-string)
  (table name-string (make-gvector)))

;; PURPOSE
;; Create a table that only contains numeric data. Saves having to specify everything.
(define (create-numeric-table name fields)
  (define field-strings (map ~a fields))
  (valid-table-name? 'create-numeric-table name)
  (for ([f field-strings])
    (valid-field-name? 'create-numeric-table f))
  (let ([T (create-table (format "~a" name))])
    (for ([col (map (λ (f) (format "~a" f)) field-strings)])
      (add-series T (create-series col number-sanitizer)))
    T))

(define (add-series T S)
  ;; FIXME Check to see that a series with this name does
  ;; not already exist.
  ;; ADDITION May want a replace-series interface.
  (gvector-add! (table-serieses T) S)
  T)
                           
;; FIXME For now, we're consuming lists. It would be nice
;; to consume vectors and... whatever else might come in.
(define (create-series name sanitizer
                       #:values [values empty])
  (define sanitized (sanitizer values))
  (define gv (list->gvector sanitized))
  (series name sanitizer gv))

;; Insert value into a series in a table.
(define insert
  (match-lambda*
    [(list (? table? T) (? series? S) (? list? v*))
     (for ([v v*])
       (insert T S v))]
    
    [(list (? table? T) (? series? S) v)
     (define the-series (get-series-by-name T (series-name S)))
     (define sanitized ((series-sanitizer the-series) (list v)))
     ;; (display sanitized) (newline)
     (for ([v sanitized])
       (gvector-add! (series-values the-series) v))]

    ;; As a list
    [(list (? table? T) (? list? v*))
     (apply insert (cons T v*))]
    
    ;; Insert values... must be same as number of serieses in the T
    [(list (? table? T) v* ...)
     (cond
       [(not (= (gvector-count (table-serieses T)) (length v*)))
        (error 'insert "Need to insert [ ~a ] values to match columns [ ~a ]~nYou tried to insert [ ~a ]"
               (gvector-count (table-serieses T))
               (apply string-append
                      (add-between (for/list ([s (table-serieses T)]) (series-name s)) ", "))
               v*
               )]
       [else
        (for ([s (table-serieses T)]
              [v v*])
          (insert T s v))]
       )]
    ))


(define (select #:columns cols #:from T)
  (let ([tn (table-name T)])
    (unless (table? T)
      (error 'select "Not a table: [ ~a ]" T))
    (define table-name
      (apply string-append
             (add-between (map (λ (o) (format "~a" o))
                               (cons tn cols)) "-")))
    (define newT (create-table table-name))
    (for ([c cols])
      (define s (get-series-by-name T (format "~a" c)))
      (add-series newT s))
    newT))


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
  (define newT (create-table (format "sieve-~a" (table-name T))))
  (define col-ndx-map (make-hash))
  ;; This gives me the index for a given column name into the
  ;; full row of the source table.
  (for ([c (map string->symbol (for/list ([s (table-serieses T)]) (series-name s)))]
        [ndx (range (gvector-count (table-serieses T)))])
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
  (for ([c cols]
        [ndx (length cols)]
        )
    (define s (get-series-by-name T (format "~a" c)))
    (add-series newT (create-series (series-name s)
                                    (series-sanitizer s)
                                    #:values
                                    (map (λ (r) (list-ref r ndx)) keep)))
    )
  newT
  )

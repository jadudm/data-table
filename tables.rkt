#lang racket

(require data/gvector db)

(require (for-syntax syntax/parse))

(require "types.rkt"
         "table-ops.rkt"
         "sqlite.rkt"
         "sanitizers.rkt")

(provide (all-defined-out))

;; CONSTANTS
(define default-gvector-length 100)

;; Should the serieses be a hash table?
(define (create-table name)
  (table name (make-gvector)))

(define-syntax (create-numeric-table stx)
  (syntax-parse stx
    [(_ name fields ...)
     #`(let ([T (create-table (format "~a" (quote name)))])
         (for ([col (map (λ (f) (format "~a" f)) (quote (fields ...)))])
           (add-series T (create-series col number-sanitizer)))
         T)]))

(define (add-series T S)
  ;; FIXME Check to see that a series with this name does
  ;; not already exist.
  ;; ADDITION May want a replace-series interface.
  (gvector-add! (table-serieses T) S))
                           
;; FIXME For now, we're consuming lists. It would be nice
;; to consume vectors and... whatever else might come in.
(define (create-series name sanitizer
                       #:values [values empty])
  (define sanitized (sanitizer values))
  (define gv (list->gvector values))
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

(define-syntax (select stx)
  (syntax-parse stx
    [(s (~alt (~seq #:column cols:id)
              (~once (~seq #:from T))) ...)
     #`(let ([tn (table-name T)])
         (unless (table? T)
           (error 'select "Not a table: [ ~a ]" T))
         (define table-name
           (apply string-append
                  (add-between (map (λ (o) (format "~a" o))
                                    (cons tn (quote (cols ...)))) "-")))
           (define newT (create-table table-name))
         (for ([c (quote (cols ...))])
           (define s (get-series-by-name T (format "~a" c)))
           (add-series newT s))
         newT)]))

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
    

;; FIXME
;; These should be thin wrappers around functions. Just enough
;; here to do the quoting, then pull it out to a function.
;; This will require rethinking things as a functional interface,
;; and then wrapping the language around it.
(define-syntax (sieve stx)
  (syntax-parse stx
    [(s T
        (~alt (~seq #:using cols:id)
              (~once (~seq #:where Q:expr))) ...)
        
     #`(let ()
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
           (define newQ (parse-query (quasiquote Q) col-ndx-map row))
           ;; (printf "newQ: ~a~n" newQ)
           (when (eval newQ)
             ;; (printf "Keeping: ~a~n" row)
             (set! keep (cons row keep)))
           )

         ;; (printf "Kept: ~a~n" keep)
         ;; Add the kept data to the newT
         (for ([c (quote (cols ...))]
               [ndx (length (quote (cols ...)))]
               )
           (define s (get-series-by-name T (format "~a" c)))
           (add-series newT (create-series (series-name s)
                                           (series-sanitizer s)
                                           #:values
                                           (map (λ (r) (list-ref r ndx)) keep)))
           )
         newT
         )]))
         
(module+ test
  (require rackunit)

  ;; Create a table with two series.
  (define baconT (create-table "bacons"))

  (define stripsS (create-series "strips" number-sanitizer
                                 #:values (map (λ (n) n) (range 5))))
  (define streaksS (create-series "streaks" number-sanitizer
                                  #:values (map (λ (n) n) (range 5 10))))
  (add-series baconT stripsS)
  (add-series baconT streaksS)
  (insert baconT '(3 5))
  ;; Could also be
  ;; (insert baconT 3 5)

  ;; TEST
  ;; Does the select statement return a new table containing the
  ;; correct data?
  (define test-1
    (let ()
      (define T (create-table "bacons-streaks"))
      (add-series T (create-series "streaks" number-sanitizer
                                   #:values (append (range 5 10) (list 5))))
      T))
  (define select-1
    (select #:from baconT
            #:column streaks
            ))
  ;; Should be a deep equality test.
  (check-equal? test-1 select-1)

  ;; TEST
  ;; Does get-rows return the rows of the table?
  (check-equal? #((0 5) (1 6) (2 7) (3 8) (4 9) (3 5))
                (get-rows baconT))

  ;; TEST
  ;; Does sieve return the properly filtered data?
  (define test-2
    (let ()
      (define T (create-table "big-bacons"))
      (add-series T (create-series "strips" number-sanitizer
                                   #:values '(1 0)))
      (add-series T (create-series "streaks" number-sanitizer
                                   #:values '(6 5)))
      T))   
  (define sieved-2
    (rename-table
     (sieve baconT
            #:using strips
            #:using streaks
            #:where (and (> streaks 3) (< strips 2))
            
           )
     "big-bacons"))
  (check-equal? test-2 sieved-2)
  (save sieved-2)
  )
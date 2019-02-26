#lang racket

(provide (contract-out
          [create-table          (case-> (-> (or/c symbol? string?)
                                             any/c)
                                         (-> (or/c symbol? string?)
                                             (cons/c (listof (or/c symbol? string?))
                                                     (listof any/c))
                                             any/c))]
          [create-numeric-table  (-> (or/c symbol? string?) (list/c (or/c symbol? string?)) any)]
          [create-series         (-> (or/c symbol? string?)
                                     (-> any/c any)
                                     #:values list?
                                     series?)]
          [add-series!            (-> data-table? series? data-table?)]
          [insert                (case->
                                  (-> data-table? series? list? any)
                                  (-> data-table? list? any)
                                  (-> data-table? series? any)
                                  (-> data-table? #:rest any/c any))]

          [rename-table            (-> data-table? string? data-table?)]
          [get-series-by-name      (-> data-table? string? series?)]
          [get-rows                (-> data-table? vector?)]
          [get-column              (-> data-table? string? vector?)]
          [get-series-names        (-> data-table? (listof string?))]
          [table-count             (-> data-table? number?)]
          [table-series-count      (-> data-table? number?)]
          )
         (rename-out [table-series-count table-column-count])
         )

(require data/gvector
         "sanitizers.rkt"
         "types.rkt")

;; CONSTANTS
(define default-gvector-length 100)

;; FIXME : These should probably be exceptions.
(define (valid-table-name? fun name)
  (unless (regexp-match "[a-zA-Z0-9_]" name)
    (error fun
           (string-append "Valid names for tables contain letters, numbers, and the underscore (the _ symbol). Nothing else.~n"
                          "\tYou provided: ~a" name))))

;; FIXME : These should probably be exceptions.
(define (valid-field-name? fun name)
  (unless (regexp-match "[a-zA-Z0-9_]" name)
    (error fun
           (string-append "Valid column names for tables contain letters, numbers, and the underscore (the _ symbol). Nothing else.~n"
                          "\tYou provided: ~a" name))))
;; PURPOSE
;; Creates a table structure. Makes sure the name conforms
;; to SQL naming conventions, so that we can save to an SQLite file.
(define create-table
  (match-lambda*
    [(list (? string? name))
     (define name-string (~a name))
     (valid-table-name? 'create-table name-string)
     (data-table name-string (make-gvector))]
    ;; Matches "a-table" '((a b c) (1 2 3) (4 5 6) ...)
    [(list (? string? name)
           (list (list (? symbol? s*) ...)
                 (list data-row* ...) ...))
     (define name-string (~a name))
     (valid-table-name? 'create-table name-string)
     (define T (data-table name-string (make-gvector)))
     (for ([name s*]
           [ndx  (range (length s*))])
       (define data (map (λ (row)
                           (list-ref row ndx)) data-row*))
       (define S (create-series (~a name)
                                (guess-sanitizer data)
                                #:values data))
       (add-series! T S))
     T]
    ))

;; PURPOSE
;; Create a table that only contains numeric data. Saves having to specify everything.
(define (create-numeric-table name fields)
  (define field-strings (map ~a fields))
  (valid-table-name? 'create-numeric-table name)
  (for ([f field-strings])
    (valid-field-name? 'create-numeric-table f))
  (let ([T (create-table (format "~a" name))])
    (for ([col (map (λ (f) (format "~a" f)) field-strings)])
      (add-series! T (create-series col number-sanitizer)))
    T))

;; CONTRACT
;; add-series : table series -> table
;; Returns a new table.
(define (add-series! T S)
  (when (member (series-name S)
                (map series-name (gvector->list (data-table-serieses T))))
    (error 'add-series "Series [ ~a ] already exists in table [ ~a ]~n"
           (series-name S) (data-table-name T)))
  (gvector-add! (data-table-serieses T) S)
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
    [(list (? data-table? T) (? series? S) (? list? v*))
     (for ([v v*])
       (insert T S v))]
    
    [(list (? data-table? T) (? series? S) v)
     (define the-series (get-series-by-name T (series-name S)))
     (define sanitized ((series-sanitizer the-series) (list v)))
     ;; (display sanitized) (newline)
     (for ([v sanitized])
       (gvector-add! (series-values the-series) v))]

    ;; As a list
    [(list (? data-table? T) (? list? v*))
     (apply insert (cons T v*))]
    
    ;; Insert values... must be same as number of serieses in the T
    [(list (? data-table? T) v* ...)
     (cond
       [(not (= (gvector-count (data-table-serieses T)) (length v*)))
        (error 'insert "Need to insert [ ~a ] values to match columns [ ~a ]~nYou tried to insert [ ~a ]"
               (gvector-count (data-table-serieses T))
               (apply string-append
                      (add-between (for/list ([s (data-table-serieses T)]) (series-name s)) ", "))
               v*
               )]
       [else
        (for ([s (data-table-serieses T)]
              [v v*])
          (insert T s v))]
       )]
    ))


(define (rename-table T name)
  (data-table name (data-table-serieses T)))

(define (table-count T)
  (gvector-count (series-values (gvector-ref (data-table-serieses T) 0))))

(define (table-series-count T)
  (gvector-count (data-table-serieses T)))
  

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
  (finder (data-table-serieses T) 0))

(define (get-series-names T)
  (map series-name (gvector->list (data-table-serieses T))))

(define (get-rows T)
  (define lor empty)
  ;; This sets up the indicies to march down the vectors
  (for/vector ([n (gvector-count (series-values (gvector-ref (data-table-serieses T) 0)))])
    ;; This is so I can go through each of the serieses
    (for/list ([s (data-table-serieses T)])
      (gvector-ref (series-values s) n))))

(define (get-column T col)
  (define sbn (get-series-by-name T col))
  ;; (printf "Requested ~a~nGot: ~a~n" col sbn)
  (gvector->vector (series-values sbn)))


;; ----------------------------------------------------------------- 
;; ----------------------------- TESTS -----------------------------
;; -----------------------------------------------------------------

(module+ test
  (require rackunit)
  (define (create-bacon-table)
    (define baconT (create-table "bacons"))
  
    (define stripsS (create-series "strips" integer-sanitizer
                                   #:values (map (λ (n) n) (range 5))))
    (define streaksS (create-series "streaks" integer-sanitizer
                                    #:values (map (λ (n) n) (range 5 10))))

    (add-series! (add-series! baconT stripsS) streaksS)
    )

  (define baconT (create-bacon-table))
  (define otherBaconT (create-table "bacons"
                                    '((strips streaks)
                                      (0 5)
                                      (1 6)
                                      (2 7)
                                      (3 8)
                                      (4 9))))
   
  (define comparison-table
    (data-table "bacons"
                (gvector
                 (series "strips"  integer-sanitizer (gvector 0 1 2 3 4))
                 (series "streaks" integer-sanitizer (gvector 5 6 7 8 9))
                 )))

  (check-equal? baconT comparison-table )
  (check-equal? baconT otherBaconT)

  ;; This should fail; it adds a series that already exists.
  (check-exn
   exn:fail?
   (λ ()
     (add-series! baconT (series "strips"  integer-sanitizer (gvector 0 1 2 3 4)))))
  )

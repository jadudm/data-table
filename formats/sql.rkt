#lang racket

(require db)
(require "../tables.rkt"
         "../sanitizers.rkt"
         "sql-util.rkt")

(provide read-mysql read-sqlite
         table->scribble
         (rename-out
          [mysql lookup-table:mysql]
          [sqlite lookup-table:sqlite])
         )

(define (table->scribble t)
  (cond
    [(empty? t) empty]
    [else
     (define sanitizer (second (first t)))
     (append (map (λ (type) (list (format "~a" type)
                                  (regexp-replace ">" (regexp-replace "#<procedure:" (format "~a" sanitizer) "") "")))
                  (first (first t)))
             (table->scribble (rest t)))]))

(define mysql
  `( ((int integer tinyint smallint mediumint bigint)
      ,integer-sanitizer)
     ((real double decimal)
      ,number-sanitizer)
     ((varchar text var-string)
      ,string-sanitizer)
     ((datetime timestamp date time)
      ,identity-sanitizer)
     ((blob)
      ,identity-sanitizer)
     )
  )

(define sqlite
  `( ((integer)
      ,integer-sanitizer)
     ((real)
      ,number-sanitizer)
     ((text)
      ,string-sanitizer)
     ;; FIXME: blob should result in bytes?
     ((blob)
      ,identity-sanitizer
      ) )
  )

(define (make-sql-sanitizer table)
  (λ (t)
    (define (helper table t)
      (cond
        [(empty? table)
         (error 'sql-type->sanitizer
                "Cannot find a sanitizer for type [ ~a ]~n"
                t)]
        [(ormap (λ (type)
                  (regexp-match type (format "~a" t)))
                (map ~a (first (first table))))
         (second (first table))]
        [else
         (helper (rest table) t)]))
    (helper table t)))

(define (mysql-type->sanitizer t)
  ((make-sql-sanitizer mysql) t))

(define (sqlite-type->sanitizer t)
  ((make-sql-sanitizer sqlite) t))    

(define (read-mysql conn table)
  (read-sql conn table #:sql-type->sanitizer mysql-type->sanitizer))

(define (read-sqlite conn table)
  (read-sql conn table
            #:show-table-query (format "PRAGMA table_info(~a);" table)
            #:name-column 1
            #:type-column 2
            #:sql-type->sanitizer sqlite-type->sanitizer))

;; ----------------------------------------------------------------- 
;; ----------------------------- TESTS -----------------------------
;; -----------------------------------------------------------------

(module+ test
  (require rackunit
           "../sanitizers.rkt"
           "../syntax-wrappers.rkt")

  ;; ----- MySQL -----
  ;; A public database for testing!
  ;; https://rfam.readthedocs.io/en/latest/database.html
  (define conn
    (mysql-connect
     #:database "Rfam"
     #:server "mysql-rfam-public.ebi.ac.uk"
     #:user "rfamro"
     #:port 4497))
  (define T (read-mysql conn "family"))
  (check-equal? (table-count T) 3016)
    
  (check-equal? (table-count
                 (sieve T
                        #:using number_of_species
                        #:where (< number_of_species 5)))
                546)

  ;; ----- SQLite -----
  (define connSQLite
    (sqlite3-connect #:database "sqlite-test-file.sqlite"))
  (define T1 (read-sqlite connSQLite "error_quotients"))
  (define T2 (read-sqlite connSQLite "watwin_pairs"))
  (check-equal? (table-count T1) 3)
  (check-equal? (table-count T2) 51)
  (check-equal? (table-count (sieve T2
                                    #:using id
                                    #:where (> id 2707309142)))
                10)
  )

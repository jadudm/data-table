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
  `( ((integer tinyint smallint mediumint bigint)
      ,integer-sanitizer)
     ((real double decimal)
      ,number-sanitizer)
     ((varchar text var-string)
      ,string-sanitizer)
     ((date time datetime)
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
        [(member (format "~a" t) (map ~a (first (first table))))
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



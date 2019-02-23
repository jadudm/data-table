#lang racket

(require db)
(require "../tables.rkt"
         "../sanitizers.rkt"
         "sql-util.rkt")

(provide read-mysql read-sqlite)

(define mysql
  `( (("int" "integer")
      ,integer-sanitizer)
     (("float" "double")
      ,number-sanitizer)
     (("text"  "varchar")
      ,string-sanitizer)
     (("datetime")
      ,identity-sanitizer)
     (("timestamp")
      ,identity-sanitizer))
  )

(define sqlite
  `( (("integer")
      ,integer-sanitizer)
     (("float" "double" "real")
      ,number-sanitizer)
     (("text")
      ,string-sanitizer)
     ;; FIXME: blob should result in bytes?
     (("blob")
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
        [(member (format "~a" t) (first (first table)))
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



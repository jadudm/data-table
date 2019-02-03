#lang racket

(require db)
(require "../private/types.rkt"
         "../private/ops.rkt")

(provide save)

(define (create-empty-file name exists)
  (define fname (format "~a-~a.sqlite" name (current-seconds)))
  ;; Make sure it is an SQLite DB file.
  (when (and (equal? exists 'overwrite)
             (file-exists? fname)
             (regexp-match "sqlite$" fname))
    (delete-file fname))
  ;; Create an empty file.
  (close-output-port (open-output-file fname))
  fname)


(define (->str o)
  (format "~a" o))

(define (clean-name str)
  (for ([sym '("/" "\\" "-" ":" ";" "\\." "," "\\+")])
    (set! str (regexp-replace* sym str "")))
  str)

(define (setup-sqlite T conn)
  (define field-string (apply string-append
                              (add-between
                               (for/list ([s (table-serieses T)])
                                 (format "~a number" (clean-name (->str (series-name s)))))
                               ", ")))
  (define qstring
    (format "create table ~a (ndx integer primary key ~a"
            (clean-name (table-name T))
            (if (> (string-length field-string) 0)
                (format ", ~a)" field-string)
                ")")
            ))
  ;;(printf "qstring: ~a~n" qstring)
  (query-exec conn qstring))

(define (save T)
  ;; Create an empty file.
  (define db-fname (create-empty-file (table-name T) 'overwrite))
  (define conn (sqlite3-connect #:database db-fname))
  ;; Create the table in the SQLite file
  (setup-sqlite T conn)
  (for ([row (get-rows T)])
    (define field-names (add-between
                         (for/list ([s (table-serieses T)])
                           (clean-name (->str (series-name s))))
                         ", "))
    (define insert-statement (format "insert into ~a ~a values ~a"
                                     (clean-name (table-name T))
                                     field-names
                                     (add-between (map ->str row) ",")))
    ;; (printf "is: ~s~n" insert-statement)
    (query-exec conn insert-statement))
  (disconnect conn))
  

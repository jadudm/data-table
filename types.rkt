#lang racket

(provide (all-defined-out))


;; A base table type has the foundations. Used
;; for building tables in memory/on the fly./
(struct data-table (name serieses)
  #:transparent)

;; An sql-table reads in from an SQL connection.
(struct sql-table data-table (conn fname clean-name)
  #:transparent)

;; A column in a table is a series.
(struct series (name sanitizer values)
  #:transparent)
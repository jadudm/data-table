#lang racket

(provide (all-defined-out))

;; A base table type has the foundations. Used
;; for building tables in memory/on the fly./
(struct data-table (name serieses)
  #:transparent)

;; A column in a table is a series.
(struct series (name sanitizer values)
  #:transparent)

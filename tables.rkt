#lang racket

;; This file provides the interface. That means the implementation
;; lives in private/, and the syntactic wrapper lives here.

(require (for-syntax syntax/parse))
(require (prefix-in t: "private/tables.rkt"))

;; Where should the sqlite interface live?

(define-syntax (create-numeric-table stx)
  (syntax-parse stx
    [(cnt name fields ...)
     #`(t:create-numeric-table (quote name) (quote (fields ...)))]))

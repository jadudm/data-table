#lang racket

(provide sheet->table)

(require net/url
         "csv.rkt"
         )

;; "http://bit.ly/2E2qZoI"
;; "https://pult.us/u/flavors"
;; "https://tinyurl.com/yx8nswkz"

;; WARNING
;; This works for:
;; * bit.ly
;; * your.ls
;; * tinyurl.com
;;
;; And, it seems to work for multiple layers
;; of redirection. I mean, it should, because
;; that is how it was written. 

(define (check-for-redirect los)
  (cond
    [(empty? los) false]
    [(regexp-match "301 Moved" (first los)) true]
    [else
     (check-for-redirect (rest los))]))

(define (get-new-url los)
  (cond
    ;; FIXME
    ;; This is a bad condition to get to. An error, or something, should happen.
    ;; This is just a quiet death on bad data.
    [(empty? los) ""]
    [(regexp-match "Location: " (first los))
     (second (regexp-match "Location: (.*)" (first los)))]
    [else
     (get-new-url (rest los))]))

(require keyword-lambda/keyword-case-lambda)
(define sheet->table
  (keyword-case-lambda
   [(name url #:header-row? [header-row? true] #:sanitizers [sanitizers empty])
     ;; FIXME
     ;; This feels expensive on large remote data sets.
     ;; For the optimization bin. Creating a ticket...
     (define lines
       (port->lines (get-impure-port (string->url url))))
     
     (cond
       [(check-for-redirect lines)
        (define new-url (get-new-url lines))
        ;; Recur on the URL that we looked up in the redirect.
        (sheet->table name new-url #:sanitizers sanitizers)
        ]
       [else
        (csv-port->table
         name
         (get-pure-port (string->url url))
         #:header-row? header-row?
         #:sanitizers sanitizers
         )])
     ]))

#lang racket

(provide read-gsheet)

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
(define read-gsheet
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
       (read-gsheet name new-url #:sanitizers sanitizers)
       ]
      [else
       (csv-port->table
        name
        (get-pure-port (string->url url))
        #:header-row? header-row?
        #:sanitizers sanitizers
        )])
    ]))


;; ----------------------------------------------------------------- 
;; ----------------------------- TESTS -----------------------------
;; -----------------------------------------------------------------

(module+ test
  (require rackunit
           "../types.rkt"
           "../sanitizers.rkt"
           data/gvector)
  (define test-url-1 "https://docs.google.com/spreadsheets/d/e/2PACX-1vQUfHoMQYItWKbZHz2MbpxhiqMCvwb85D7zAJ9VPS_92nDjrm3BqZmpi9G138svgwUz6d0ZH15pPy_F/pub?output=csv")

  ;; One of these is a double-redirect, for testing.
  (define test-url-2 "http://bit.ly/2E2qZoI")
  (define test-url-3 "https://pult.us/u/flavors")
  (define test-url-4 "https://tinyurl.com/yx8nswkz")
  
  (define test-table
    (data-table
     "Testing"
     (gvector
      (series "name"   string-sanitizer (gvector "Matt" "Matthew" "Simon"))
      (series "age"    integer-sanitizer (gvector 42 9 5))
      (series "flavor" string-sanitizer (gvector "Chocolate" "Mint" "Berry")))))

  ;; Read the same sheet in from multiple URL redirects.
  (define T1 (read-gsheet "Testing" test-url-1))
  (define T2 (read-gsheet "Testing" test-url-2))
  (define T3 (read-gsheet "Testing" test-url-3))
  (define T4 (read-gsheet "Testing" test-url-4))

  ;; Check they are equal (tranisitively)
  (check-equal? test-table T1)
  (check-equal? test-table T2)
  (check-equal? test-table T3)
  (check-equal? test-table T4)
  
  )
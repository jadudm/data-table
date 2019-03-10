#lang racket
(require racket/struct
         data/gvector)

(provide (struct-out series)
         (struct-out data-table))


(define (field o width side
               #:prefix [prefix ""]
               #:suffix [suffix ""])
  (define as-string (format "~a" o))
  (define len (string-length as-string))
  (cond
    [(> len width)
     (string-append prefix (substring as-string 0 width) suffix)]
    [else
     (define pad (- width len))
     (define empt (make-string pad #\space))
     (case side
       [(r right)  (string-append prefix empt as-string suffix)]
       [(l left)   (string-append prefix as-string empt suffix)]
       [(c center) (string-append
                    prefix
                    (make-string (exact-floor (/ pad 2)) #\space)
                    as-string
                    (make-string (exact-floor (/ pad 2)) #\space)
                    suffix)])
     ]))

(define (make-hyphens str #:sub [sub 0])
  (make-string (- (string-length str) sub) #\-))

;; A base table type has the foundations. Used
;; for building tables in memory/on the fly.
(define (data-table-print obj the-port mode)
  (define COL-WIDTH 10)
  (define COLS-TO-SHOW 6)

  (define total-columns (gvector-count (data-table-serieses obj)))
  (define cols-to-take
    (if (> (gvector-count (data-table-serieses obj)) COLS-TO-SHOW)
        COLS-TO-SHOW
        (gvector-count (data-table-serieses obj))))
  
  (define col-names
    (take 
     (map (λ (s)
            (field (format "~a" s) COL-WIDTH 'right #:suffix " | "))
          (map series-name (gvector->list (data-table-serieses obj))))
     cols-to-take))
  (define num-cols (length col-names))
  
  
  (define title (format "~a" (data-table-name obj) ))
  (define num-rows
    (gvector-count (series-values
                    (gvector-ref (data-table-serieses obj) 0))))
  (define num-columns (gvector-count (data-table-serieses obj)))
  (define meta  (format "~a column~a, ~a row~a ~a"
                        num-columns
                        (if (< 1 num-columns) "s" "")
                        num-rows
                        (if (< 1 num-rows) "s" "")
                        (if (> total-columns cols-to-take)
                            (format "(only showing ~a columns)" cols-to-take)
                            "")
                        ))
  
  (newline the-port)
  (display title the-port)
  (newline the-port)
  (display meta the-port)
  (newline the-port)
  (display (make-hyphens (apply string-append col-names) #:sub 1) the-port)
  (newline the-port)

  
  
  (map (λ (s) (display s the-port)) col-names)
  (newline the-port)
  (display (make-hyphens (apply string-append (take col-names cols-to-take)) #:sub 1) the-port)
  (newline the-port)
  
  (for ([ndx (range 4)])
    (define show-newline true)
    (for ([s (take (gvector->list (data-table-serieses obj)) cols-to-take)])
      (with-handlers ([exn? (λ (e) (set! show-newline false))])
        (display (field
                  (gvector-ref (series-values s) ndx)
                  COL-WIDTH 'right #:suffix " | ")
                 the-port)))
      (when show-newline (newline the-port))
      )

  (when (> num-rows 4)
    (for ([s (take (gvector->list (data-table-serieses obj)) cols-to-take)])
      (display (field
                "..."
                COL-WIDTH 'right #:suffix " | ")
               the-port))
    (newline the-port)
    )
  )
  
(struct data-table (name serieses)
  #:transparent
  #:methods gen:custom-write     
  [(define write-proc data-table-print)])


;; A column in a table is a series.
;; https://docs.racket-lang.org/reference/Printer_Extension.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._gen~3acustom-write%29%29


(define (series-print obj the-port mode)
  (let ([out!
         (case mode
           [(#t) write]
           [(#f) display]
           [else (lambda (p port) (print p port mode))]
           )])
    
    (define NW 12)
    (define FW 8)
    (display (format "~a : "
                     (field (format "~a" (series-name obj))
                            NW 'left))
             the-port)
    (define limit 4)
    (define list-of-values
      (gvector->list (series-values obj)))
    (let loop ([ls list-of-values]
               [count 0])
      (unless (or (>= count limit)
                  (empty? ls))
        (display
         (format "~a " (field (first ls) FW 'r)) the-port)
        (loop (rest ls) (add1 count))))
    (when (> (length list-of-values) limit)
      (display (field "   ... " FW 'center) the-port)
      (display
       (field (format " ~a" (first (reverse list-of-values)))
              FW 'r)
       the-port)
      )
    ))
  

(struct series (name sanitizer values)
  #:transparent)
;;  #:methods gen:custom-write     
;;  [(define write-proc series-print)])
       
#|
(define foo
  (series "foo" (λ (o) o) (list->gvector '(1 2 3 4 5))))
(series "bar" (λ (o) o) (list->gvector '(1 2 3 4 5)))
(series "long-series-name"
        (λ (o) o) (list->gvector '(1 2 3 4 5)))

(define big
  (series "long-series-name"
          (λ (o) o) (list->gvector '(1.23456789
                                     2.34567890
                                     3.45678901
                                     4.56789012
                                     5.67890123))))
(define T
  (data-table "a-table"
            (list->gvector
              (list
               foo
               (series "bar" (λ (o) o) (list->gvector '(1 2 3 4 5)))
               big
               big
               (series "gee" (λ (o) o) (list->gvector '(11 21 31 41 51)))
               big
               big
               big
               big
               ))))

(define T2
  (data-table "a-table"
    (list->gvector 
              (list
                (series "foo" (λ (o) o) (list->gvector '(4 5 6)))
               (series "bar" (λ (o) o) (list->gvector '(1 2 3)))
               ))))
|#
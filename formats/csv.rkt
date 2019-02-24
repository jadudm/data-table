#lang racket

(provide read-csv csv-port->table)

(require csv-reading
         "../types.rkt"
         "../tables.rkt"
         "../sanitizers.rkt")

(define default-csv-reader
  (make-csv-reader-maker
   '((separator-chars            #\,)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))

(define (snoc o ls)
  (reverse (cons o (reverse ls))))


(define (read-csv file
                  #:header-row? [header-row? true]
                  #:sanitizers  [sanitizers empty]
                  )
  ;; FIXME Could name the table better.
  (define T (create-table file))
  (define seriesH (make-hash))
  
  (define next-row
    (default-csv-reader (open-input-file file)))

  ;; Read the headers
  (define headers (next-row))
  (unless (= (length headers)
             (length sanitizers))
    (error 'read-csv "Unequal columns/sanitizers [cols ~a] [sanitizers ~a]"
           (length headers)
           (length sanitizers)))
              
  
  ;; Read the CSV into separate lists. The create-series function
  ;; currently expects lists.
  (let loop ([row (next-row)])
    (unless (empty? row)
      (for ([ndx (length row)])
        (hash-set! seriesH ndx
                   (snoc (list-ref row ndx) (hash-ref seriesH ndx empty))))
      (loop (next-row))))

  ;; Now, each CSV column is in the hash. 
  ;; Each needs a series, and those will be based on the
  ;; sanitizers passed in.
  (for ([s sanitizers]
        [h headers]
        [ndx (range (length sanitizers))])
    (define new-series (create-series h s #:values (hash-ref seriesH ndx)))
    (add-series T new-series)
    )
  ;; Return the new table.
  T)

;; FIXME
;; This should be unified with the code above... I just did a copy
;; past for speed of testing something.
(define (csv-port->table
         name
         port
         #:header-row? [header-row? true]
         #:sanitizers  [sanitizers empty]
         )
  ;; FIXME Could name the table better.
  (define T (create-table name))
  (define seriesH (make-hash))
  
  (define next-row
    (default-csv-reader port))

  ;; Read the headers
  (define headers empty)
  (when header-row?
    (set! headers (next-row)))
  
  ;; Read the CSV into separate lists. The create-series function
  ;; currently expects lists.
  (let loop ([row (next-row)])
    (unless (empty? row)
      (for ([ndx (length row)])
        (hash-set! seriesH ndx
                   (snoc (list-ref row ndx) (hash-ref seriesH ndx empty))))
      (loop (next-row))))

  ;; Give us something in the header row...
  (when (not header-row?)
    (set! headers
          (map (λ (ndx) (format "row-~a" ndx))
               (range (length (hash-ref seriesH 0))))))

  (when (not (empty? sanitizers))
    (unless (= (length headers)
               (length sanitizers))
      (error 'read-csv "Unequal columns/sanitizers [cols ~a] [sanitizers ~a]"
             (length headers)
             (length sanitizers))))

  ;; Now, did we get handed sanitizers, or do we have to guess?
  (when (empty? sanitizers)
    (set! sanitizers (map (λ (ndx)
                            (guess-sanitizer (hash-ref seriesH ndx)))
                          (range (length headers)))))

  ;; Now, each CSV column is in the hash. 
  ;; Each needs a series, and those will be based on the
  ;; sanitizers passed in.
  (for ([s sanitizers]
        [h headers]
        [ndx (range (length sanitizers))])
    (define new-series (create-series h s #:values (hash-ref seriesH ndx)))
    (add-series T new-series)
    )
  ;; Return the new table.
  T
  )
  
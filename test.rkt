#lang racket

(require "main.rkt"
         "private/types.rkt"
         "formats/sheets.rkt"
         "formats/mysql.rkt"
         "sanitizers.rkt"
         data/gvector
         rackunit
         db)

(define (create-bacon-table)
  (define baconT (create-table "bacons"))
  
  (define stripsS (create-series "strips" number-sanitizer
                                 #:values (map (λ (n) n) (range 5))))
  (define streaksS (create-series "streaks" number-sanitizer
                                  #:values (map (λ (n) n) (range 5 10))))
  (add-series baconT stripsS)
  (add-series baconT streaksS)
  baconT)

(define creation-tests
  (test-suite
   "Tests for creating structures."
   (let ()
     ;; Create a table with two series.
     (define baconT (create-bacon-table))
     (define comparison-table
       (table "bacons"
              (gvector
               (series "strips"  number-sanitizer (gvector 0 1 2 3 4))
               (series "streaks" number-sanitizer (gvector 5 6 7 8 9))
               )))
     
     (check-equal? comparison-table baconT))))

(define insert-tests
  (test-suite
   "Tests for inserting into tables."
   (let ()
     (define baconT (create-bacon-table))
     
     (define comparison-table
       (table "bacons"
              (gvector
               (series "strips"  number-sanitizer (gvector 0 1 2 3 4 3))
               (series "streaks" number-sanitizer (gvector 5 6 7 8 9 5))
               )))

     (insert baconT '(3 5))

     (check-equal? comparison-table baconT)
     )))

(define select-tests
  (test-suite
   "Tests for selecting from tables."
   (let ()
     (define baconT (create-bacon-table))
     (insert baconT '(3 5))
     
     ;; TEST
     ;; Does the select statement return a new table containing the
     ;; correct data?
     (define test-1
       (let ()
         (define T (create-table "bacons-streaks"))
         
         
         (add-series
          T (create-series
             "streaks" number-sanitizer
             #:values (append (range 5 10) (list 5))))
         T))

     ;; (printf "Running select-1~n")
     (define select-1
       (select #:from baconT
               #:column streaks
               ))
     ;; Should be a deep equality test.
     (check-equal? test-1 select-1)

     ;; TEST
     ;; Does get-rows return the rows of the table?
     (check-equal? #((0 5) (1 6) (2 7) (3 8) (4 9) (3 5))
                   (get-rows baconT))

     
     ;; TEST
     ;; Does sieve return the properly filtered data?
     (define test-2
       (let ()
         (define T (create-table "big-bacons"))
         (add-series T (create-series "strips" number-sanitizer
                                      #:values '(0 1)))
         (add-series T (create-series "streaks" number-sanitizer
                                      #:values '(5 6)))
         T))   
     (define sieved-2
       (rename-table
        (sieve baconT
               #:using strips
               #:using streaks
               #:where (and (> streaks 3) (< strips 2))
            
               )
        "big-bacons"))
     (check-equal? test-2 sieved-2)     
     )))


(define sheets-tests
  (test-suite
   "Testing Google Sheets import"
   (let ()
     (define test-url "https://docs.google.com/spreadsheets/d/e/2PACX-1vQUfHoMQYItWKbZHz2MbpxhiqMCvwb85D7zAJ9VPS_92nDjrm3BqZmpi9G138svgwUz6d0ZH15pPy_F/pub?output=csv")
     
     (define fetched
       (with-handlers ([exn? (λ (e) false)])
         (sheet->table "Testing" test-url
                       #:sanitizers
                       (list string-sanitizer
                             number-sanitizer
                             string-sanitizer))))
     (define test-table
       (table
        "Testing"
        (gvector
         (series "name" string-sanitizer (gvector "Matt" "Matthew" "Simon"))
         (series "age" number-sanitizer (gvector 42 9 5))
         (series "flavor" string-sanitizer (gvector "Chocolate" "Mint" "Berry")))))
     (check-equal? fetched test-table)

     ;; Lets test a sieve operation on this table.
     (define sieved
       (sieve fetched
              #:using age
              #:where (> age 6)))

     (define test-sieve-table
       (table "sieve-Testing"
              (gvector
               (series "name"   string-sanitizer (gvector "Matt" "Matthew"))
               (series "age"    number-sanitizer (gvector 42 9))
               (series "flavor" string-sanitizer (gvector "Chocolate" "Mint")))))
     (check-equal? sieved test-sieve-table)
 
     )))

(define mysql-tests
  (test-suite
   "Testing MySQL import"
   (let ()
     (with-handlers ([exn? (λ (e) true)])
       (define conn
         (mysql-connect
          #:database   "network_v3"
          #:user       "root"
          #:password   "root"
          #:server     "localhost"
          #:port       8889
          ))
       
       (define T (read-sql conn "error_quotients"))
       (define newT (sieve T #:using watwin #:where (> watwin 0)))
       (check-equal? (table-count T) 555)
       (check-equal? (table-count newT) 537)))))
     

(require rackunit/text-ui)

(define all-suites
  (list creation-tests
        insert-tests
        select-tests
        sheets-tests
        mysql-tests
        ))

(for ([suite all-suites])
  (run-tests suite))



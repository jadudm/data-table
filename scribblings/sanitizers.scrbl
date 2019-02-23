#lang scribble/manual
@require[@for-label["../main.rkt"
                    racket]]
                    
@(title #:tag "sanitizers" "Sanitizers")

The @racket[data-table] library provides a reasonable set of sanitizers for common types of data that you might encounter in a CSV file or database.

@itemlist[
 @item{@italic{integer-sanitizer}: Guarantees that all numbers in the column are integers, as defined by the Racket predicate @racket[integer?].}
 @item{@italic{number-sanitizer}: Guarantees that all elements in the column are numbers, as defined by the Racket predicate @racket[number?].}
 @item{@italic{string-sanitizer}: Coerces everything in the column to a string. Anything that cannot be coerced will cause an error.}
 ]
  
  

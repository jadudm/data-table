#lang scribble/manual
@(require racket)
@require[@for-label[racket db]]
@(require scribble/eval
          scribble/struct
          racket/sandbox
          data-table)
@(require "drawing-support.rkt")
@interaction-eval[#:eval the-eval
                   (require data-table)]


@;{ -------------------------------- sieve ---------------------------------- }
@title[]{sieve}
@defproc[#:link-target? false
         (sieve
          [table table?]
          [query expression?]
          )
         table?]{ 
 Sieves, or filters, a table, using values from one or more columns in a boolean query.
}

@(draw-sieve "images/sieve.png")
@centered{
  @(image #:scale 0.75 "images/sieve.png")
}

The sieve operation extracts rows from the source table using criteria that are expressed in terms of one or more columns in the table. Continuing with our table of cities, we might look for only those cities that are north of Lewiston, Maine (44.1004 N).

@examples[#:eval the-eval
(define northerlyT
  (sieve T (> LatD 44)))
(define total-city-count (table-count T))
(define northerly-city-count (table-count northerlyT))
total-city-count
northerly-city-count
(pull northerlyT "City")
]

To reference a column in a query, it must be denoted with the @racket[#:using] keyword. For example, we can restrict our search to cities north of Lewiston and east of the Mississippi (89.9 W):

@examples[#:eval the-eval
(define north-easterlyT
  (sieve T (and (> LatD 44) (< LonD 89.9) (equal? EW "W"))))
(define north-easterly-city-count (table-count north-easterlyT))
north-easterly-city-count
(pull north-easterlyT "City")
]

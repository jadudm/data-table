#lang scribble/manual
@(require racket)
@require[@for-label[racket db]]
@(require scribble/eval
          scribble/struct
          racket/sandbox
          data-table)
          
@(define the-eval  (make-base-eval))
@interaction-eval[#:eval the-eval
                   (require data-table)]
                   
@title{Working With Data}

The @racket[data-table] library provides functions and syntactic forms for manipulating data tables in a variety of ways. In this section, we borrow the visual language of the @(hyperlink "https://github.com/rstudio/cheatsheets/blob/master/data-transformation.pdf" "dplyr") library for depicting these operations.

@(define cities-gsheet "http://bit.ly/cities-gsheet")

Throughout this section, we will work with a Google Spreadsheet that contains 128 records, each with 10 fields (or, it has 10 columns and 128 rows). The spreadsheet describes the locations of cities in North America. This data comes from @(hyperlink "https://people.sc.fsu.edu/~jburkardt/data/csv/csv.html" "here"), and was made available under the GPL. The spreadsheet is @(hyperlink cities-gsheet "viewable online").

Or, if you just want a sense for what it looks like, it looks a bit like this:

@centered{
  @(image #:scale 0.25 "images/cities-sheet.png")
}


@(define cities-csv "http://bit.ly/cities-csv")

@;{ -------------------------------- select ---------------------------------- }
@defproc[#:link-target? false
         (select
          [#:column column-name identifier?] 
          ...
          [#:from table-name identifier?]
          )
         table?]{ 
 Selects one or more columns from a table, returning a new table containing only those columns.
}

@(require "drawing-support.rkt")
@(draw-select "images/select.png")
@centered{
  @(image #:scale 0.75 "images/select.png")
}

The @racket[select] form is used to extract one or more columns from a table, and returns a new table as a result of that operation. The new table contains only those columns named in the @racket[select] statement.

Given the @(hyperlink cities-gsheet "spreadsheet of city locations"), we might want to extract only the columns for the City and State. With @racket[select], that looks like this:

@examples[#:eval the-eval
(define cities-csv "http://bit.ly/cities-csv")
(define T (sheet->table "Cities" cities-csv))
(define cities-statesT
  (select #:column City
          #:column State
          #:from T))
(table-column-count T)
(table-column-count cities-statesT)
]

The order of the parameters does not matter. The following @racket[select] expression below is the same as the one above, even thought the table is specified first.

@examples[#:eval the-eval
(table-column-count 
  (select #:from T
          #:column City
          #:column State
          ))
]

@;{ -------------------------------- sieve ---------------------------------- }
@defproc[#:link-target? false
         (sieve
          [table table?]
          [#:using column-name identifier?] 
          ...
          [#:where query expression?]
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
  (sieve T
         #:using LatD
         #:where (> LatD 44)))
(define total-city-count (table-count T))
(define northerly-city-count (table-count northerlyT))
total-city-count
northerly-city-count
(pull northerlyT "City")
]

To reference a column in a query, it must be denoted with the @racket[#:using] keyword. For example, we can restrict our search to cities north of Lewiston and east of the Mississippi (89.9 W):

@examples[#:eval the-eval
(define north-easterlyT
  (sieve T
         #:using LatD
         #:using LonD
         #:using EW
         #:where (and (> LatD 44) (< LonD 89.9) (equal? EW "W"))))
(define north-easterly-city-count (table-count north-easterlyT))
north-easterly-city-count
(pull north-easterlyT "City")
]

@;{ -------------------------------- pull ---------------------------------- }
@defproc[#:link-target? false
         (pull
          [table table?]
          [column string?]
          )
         vector?]{ 
Pulls the values out of a column as a vector.
}

@(draw-pull "images/pull.png")
@centered{
  @(image #:scale 0.75 "images/pull.png")
}

The previous examples have used @racket[pull] to extract values from tables. If we wanted to pull both the city and state from the north-easterly cities, and combine them into strings, it might look like:

@examples[#:eval the-eval
(for/list ([city  (pull north-easterlyT "City")]
           [state (pull north-easterlyT "State")])
  (format "~a, ~a" city state))
]

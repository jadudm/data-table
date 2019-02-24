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

@(define gsheet-cities "https://docs.google.com/spreadsheets/d/1rdCU4qlhvweP2ftcsOGNd2ma0O1H7TR6f7nh7icc_Qw/edit?usp=sharing")

Throughout this section, we will work with a Google Spreadsheet that contains 128 records, each with 10 fields. This data comes from @(hyperlink "https://people.sc.fsu.edu/~jburkardt/data/csv/csv.html" "here"), and was made available under the GPL. The spreadsheet is @(hyperlink gsheet-cities "viewable online").

@(define csv-cities "https://docs.google.com/spreadsheets/d/e/2PACX-1vSQHMOiYSxSh_BvIVQ6c3mG82kF47lPcuVihv5bV9Ufq_XyRl8DWDau2Og-l1duYoMj9yvFI2wRu7HL/pub?output=csv")

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

@(define T (sheet->table "Cities" csv-cities))

Given the @(hyperlink gsheet-cities "spreadsheet of city locations"), we might want to extract only the columns for the City and State. With @racket[select], that looks like this:

@examples[#:eval the-eval
(select #:column City
        #:column State
        #:from T)
]

The order of the parameters does not matter. The following expression is the same as the one above.

@racketblock[
(select #:from Bob
        #:column A
        #:column D
        )
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

The sieve, or filter operation, extracts rows from the source table using criteria that are expressed in terms of one or more columns in the table. Continuing with our table from before, we might look for all rows where the value in column A is greater than 8.

@racketblock[
(sieve Bob
       #:using A
       #:where (> A 8)
       )
]

To reference a column in a query, it must be denoted with the @racket[#:using] keyword. For example, to look for all rows where A is greater than 8, and B contains the string "Bobcat", you would say:

@racketblock[
(sieve Bob
       #:using A
       #:using B
       #:where (and (> A 8) (string-contains? B "Bobcat"))
       )
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

#lang scribble/manual
@(require racket)
@require[@for-label["../main.rkt"
                    racket db]]

@title{Working With Data}

The @racket[data-table] library provides functions and syntactic forms for manipulating data tables in a variety of ways. In this section, we borrow the visual language of the @(hyperlink "https://github.com/rstudio/cheatsheets/blob/master/data-transformation.pdf" "dplyr") library for depicting these operations.

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

Assuming a table caled @racket[Bob] with columns @racket[A], @racket[B], @racket[C], and @racket[D], we could @racket[select] a new table containing only @racket[A] and @racket[D] with the following expression:

@racketblock[
(select #:column A
        #:column D
        #:from Bob)
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

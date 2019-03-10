#lang scribble/manual
@(require racket racket/runtime-path)
@require[@for-label[racket db]]
@(require scribble/eval
          scribble/struct
          racket/sandbox
          data-table)

@(require "drawing-support.rkt")
@interaction-eval[#:eval the-eval
                (require data-table)]

@;{ -------------------------------- select ---------------------------------- }
@title[]{select}
@defproc[#:link-target? false
         (select
           [table data-table?]
           [column-name string?] ...)
         data-table?]{ 
 Selects one or more columns from a table, returning a new table containing only those columns.
}

@(define-runtime-path select.png (build-path "images" "select.png"))
@centered{
  @(image #:scale 0.75 select.png)
}


The @racket[select] form is used to extract one or more columns from a table, and returns a new table as a result of that operation. The new table contains only those columns named in the @racket[select] statement.

Given the @(hyperlink cities-gsheet "spreadsheet of city locations"), we might want to extract only the columns for the City and State. With @racket[select], that looks like this:

@examples[#:eval the-eval
(define cities-csv "http://bit.ly/cities-csv")
(define T (read-gsheet "Cities" cities-csv))
(define cities-statesT
  (select T "City" "State"))
(table-column-count T)
(table-column-count cities-statesT)
]


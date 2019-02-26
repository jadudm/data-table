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

The @racket[data-table] library provides functions and syntactic forms for manipulating data tables in a variety of ways. Throughout this section, we will work with a Google Spreadsheet that contains 128 records, each with 10 fields (or, it has 10 columns and 128 rows). The spreadsheet describes the locations of cities in North America. This data comes from @(hyperlink "https://people.sc.fsu.edu/~jburkardt/data/csv/csv.html" "here"), and was made available under the GPL. The spreadsheet is @(hyperlink cities-gsheet "viewable online").

@(define cities-gsheet "http://bit.ly/cities-gsheet")

Or, if you just want a sense for what it looks like, it looks a bit like this:

@centered{
  @(image #:scale 0.25 "images/cities-sheet.png")
}

Also, this section borrows the visual language of the @(hyperlink "https://github.com/rstudio/cheatsheets/blob/master/data-transformation.pdf" "dplyr") library for depicting operations on data-tables.

@include-section["operations/select.scrbl"]
@include-section["operations/sieve.scrbl"]
@include-section["operations/pull.scrbl"]

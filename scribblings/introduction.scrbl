#lang scribble/manual

@;{ https://www.writethedocs.org/guide/writing/beginners-guide-to-docs/ }

@title{Introduction}


You might have some data stored in:


@itemlist[
 @item{A Google Spreadsheet}
 @item{A comma-separated-values (CSV) file}
 @item{A MySQL database}
 @item{An SQLite database}
 ]

and want to manipulate that data in Racket. When you're done, you might want to save it back out to one of those places. Or, you might want to quickly plot something in the data. The data-table library supports (FIXME will support -- 2019-02-22) those operations.

data-table is inspired and informed by data frames (from the R statistical language), as well as Pyret's data tables.

@section{What Problem does data-table Solve?}

The data-table library is intended to provide an interface to tabular data that can be integrated into the introductory teaching of programming. This is not to say that it is feature-poor, but it does mean that some design decisions are made with the use cases of @italic{teaching} and @italic{learning} as being of critical importance.

@section{What's It Look Like?}

@#reader scribble/comment-reader
(racketblock
; Require the library
(require data-table)
 
; A remote Google Spreadsheet that we want to manipulate
(define test-url "http://bit.ly/2E2qZoI")
 
; Fetch the URL, and turn it into a table named "Testing"
(define fetched
  (sheet->table "Testing" test-url
                #:sanitizers
                (list string-sanitizer
                      number-sanitizer
                      string-sanitizer)))
 
; Select all of the rows where the "age" column is greater than 6.
(sieve fetched
       #:using age
       #:where (> age 6))
)

@section{The Source and Tickets}

The source for this library can be found at @(hyperlink "https://bitbucket.org/jadudm/data-table" "Bitbucket"). The @(hyperlink "https://bitbucket.org/jadudm/data-table/issues?status=new&status=open" "issue tracker") is used for reporting bugs and feature requests; please feel free to use it for those purposes!


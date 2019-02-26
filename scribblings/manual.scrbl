#lang scribble/manual
@require[@for-label[data-table
                    racket/base]]

@title[]{data-table}
@author{Matt Jadud}

@defmodule[data-table]

@emph{data-table provides a set of tools for working with tabular data}.

You might have some data stored in:

@itemlist[
 @item{A Google Spreadsheet}
 @item{A comma-separated-values (CSV) file}
 @item{A MySQL database}
 @item{An SQLite database}
 ]

and want to manipulate that data in Racket. When you're done, you might want to save it back out to (most any of) those places. Or, you might want to quickly plot something in the data. The data-table library supports (FIXME will support -- 2019-02-22) those operations.

data-table is inspired and informed by data frames (from the R statistical language), as well as Pyret's data tables.


@include-section["introduction.scrbl"]

@include-section["reference.scrbl"]

@include-section["guide.scrbl"]

#lang scribble/manual
@require[@for-label[data-table
                    racket/base]]

@;{Does this disable caching in Pollen? Yes, it seems to.}
@;{https://docs.racket-lang.org/pollen/Cache.html#%28part._.Disabling_the_cache%29}
@(module setup racket/base
  (provide (all-defined-out))
  (define compile-cache-active #f)
  (define render-cache-active #f))

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

and want to manipulate that data in Racket. When you're done, you might want to save it back out to one of those places. Or, you might want to quickly plot something in the data. The data-table library supports (FIXME will support -- 2019-02-22) those operations.

data-table is inspired and informed by data frames (from the R statistical language), as well as Pyret's data tables.


@include-section["introduction.scrbl"]

@include-section["reference.scrbl"]

@include-section["guide.scrbl"]

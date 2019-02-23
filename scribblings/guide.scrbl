#lang scribble/manual
@require[@for-label["../main.rkt"
                    racket/base]]
                    
@title[]{Guide}

The purpose of this section is to provide a guide to the use of @racket[data-table] for reading in, manipulating, and exploring tabular data. 


A CSV (or comma-separated-values) file is a plain text file that is often formatted as follows:

@verbatim|{
 index,chickens,eggs,whichfirst
 0,2,1,0
 1,3,3,1
 2,4,2,0
 }|

This example demonstrates a CSV file where this is a header row (which contains the names of the columns in the data file), and all of the values are encoded as integers. It might be data from a research study that investigated chickens, eggs, and which came first. The first column is an index value, the second is how many chickens we found in a study, the third the number of eggs found, and the final column encodes whether the chicken came first as a zero or one, indicating falsiness or truthiness.

Assuming this file is called "chickens.csv", and our code was in the same directory as the CSV file, we would read this into a data-table using the following call to @racket[read-csv]:

@racketblock[
 (read-csv "chickens.csv" 
           #:header-row true
           #:sanitizers (list integer-sanitizer
                              integer-sanitizer 
                              integer-sanitizer 
                              integer-sanitizer))
 ]

The call to @racket[read-csv] takes one parameter, which is a path to a file. It then has two optional keyword parameters, which begin with a hash-colon. The first of these indicates whether or not the data file has a header row; not all CSV files do. By default, @racket[read-csv] assumes that there is no header row. The second is a list of @emph{data sanitizers}.

A data sanitizer is a guard; it makes sure that no data is read into the data-table that does not conform to the programmer's expectations. In the above example, we are saying that the four columns of the CSV file---@italic{index}, @italic{chickens}, @italic{eggs}, and @italic{whichfirst}---are all going to be integers. Therefore, we've provided a list of four santizers, each of which is an @racket[integer-sanitizer]. If the CSV file contains anything other than an integer in any of these columns, our attempt to read the data in will fail.
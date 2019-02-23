#lang scribble/manual
@(require racket)
@require[@for-label["../main.rkt"
                    racket db]]

@title{Reading Data}

Data can be read into @racket[table]s from multiple sources.

@;{ -------------------------------- read-csv ---------------------------------- }
@defproc[#:link-target? false
         (read-csv
          [path path-string?]
          [#:header-row? header-row boolean? true]
          [#:sanitizers sanitizers (listof (-> any/c pred?)) empty]
          )
         table?]{ 
 Reads in a CSV file, and returns a data table.
}

@racket[read-csv] takes two keyword arguments. 

@racket[#:header-row?] is used to determine whether the CSV file has a header row; this defaults to @racket[true] if not provided. The header row will be used to set the names of the serieses in the @racket[data-table] that is created from the CSV file.

@racket[#:sanitizers] is a list of sanitizers, which are functions from @racket[(listof any/c)] to @racket[(listof A)]. These guarantee the uniformity of the data coming in from the CSV file. For example:

@racketblock[
 (read-csv "somefile.csv" 
           #:header-row true
           #:sanitizers (list integer-sanitizer
                              string-sanitizer 
                              number-sanitizer 
                              integer-sanitizer))
 ]

would read in a CSV file of four columns, guaranteeing that all values in the first column matched the predicate @racket[integer?], the second matches @racket[string?], the third @racket[number?], and the fourth @racket[integer?] again. 

If no sanitizers are provided, @racket[read-csv] will attempt to @italic{guess} what type each column is. It does this by deciding if the majority of the column conforms to a given type, beginning with the least specific possible type, and concluding with the most specific possible type. If it is not possible to match the type, the @italic{identity-sanitizer} will be used, which takes all data in as-is, possibly as strings. More about sanitizers can be found in @(secref "sanitizers").

@;{ -------------------------------- read-mysql ---------------------------------- }
@defproc[#:link-target? false
         (read-mysql
          [conn connection?]
          [table string?]
          )
         table?]{ 
 Reads in a table from a MySQL database, and returns a @racket[table].
}

Given a connection to a database, @racket[read-mysql] will read a table out of that database into a @racket[data-table]. Data types in the SQL table are mapped as follows:

@(require "../formats/sql.rkt")
@tabular[#:sep @hspace[10]
  @(cons 
    (list @bold{MySQL Type} @bold{Sanitizer})
    (table->scribble lookup-table:mysql)
    )]

Once a MySQL table is loaded into a @racket[table], all information about its original column types is lost; import and export of MySQL tables is not idempotent at this time.

@;{ -------------------------------- read-sqlite ---------------------------------- }
@defproc[#:link-target? false
         (read-sqlite
          [conn connection?]
          [table string?]
          )
         table?]{ 
 Read in a table from an SQLite database, and returns a @racket[table].
}

Like the MySQL importer, @racket[read-sqlite] takes a connection to an SQLite3 database and a table name, and returns the contents of that database table as a @racket[table].

Types are mapped using the following sanitizers:

@tabular[#:sep @hspace[10]
  @(cons 
    (list @bold{SQLite Type} @bold{Sanitizer})
    (table->scribble lookup-table:sqlite)
    )]
    
Like the MySQL type, it is not possible to precisely preserve column types if a @racket[table] is re-saved to the SQLite format.




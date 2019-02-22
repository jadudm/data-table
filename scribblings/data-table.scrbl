#lang scribble/manual
 
@title[]{data-table}

@emph{data-table is a simplified interface to data frames}.

It is common to have to process data that looks, for all intents and purposes, like a spreadsheet. Sometimes this data is actually in a spreadsheet, sometimes it is in a text file (where the data is separated by commas), and sometimes it is in a database.

This library provides an interface for reading data from some of these common formats, for cleaning, manipulating, and presenting that data, as well as saving it back to one or more formats.

data-table is inspired and informed by data frames (from the R statistical language), as well as Pyret's data tables.

@include-section["introduction.scrbl"]

@include-section["reading-data.scrbl"]
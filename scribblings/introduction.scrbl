#lang scribble/manual
@(require scribble/example
          racket/sandbox
          )
          
@require[@for-label["../main.rkt"
                    racket/base]]
 
@;{ https://www.writethedocs.org/guide/writing/beginners-guide-to-docs/ 
  https://superuser.com/questions/445907/monitor-a-folder-osx-for-file-changes-then-run-a-bash-script
  https://superuser.com/questions/431624/watch-filesystem-in-real-time-on-os-x-and-ubuntu
}

@title[]{Documentation Overview}

The introduction provides a very brief overview of the @racket[data-table] library.

In the Reference sections, the functions and structures provided by the @racket[data-table] library are described. These are concise descriptions of what the provided code in the library does.

In the Guide, common use-cases for those functions are explored. 

If you are new to the @racket[data-table] library, you may find starting with the guide to be helpful for orienting you towards what is possible. If you are familiar with @racket[data-table], then you are probably interested in the reference.

@section{What Problem does data-table Solve?}

The data-table library is intended to provide an interface to tabular data that can be integrated into the introductory teaching of programming. This is not to say that it is feature-poor, but it does mean that some design decisions are made with the use cases of @italic{teaching} and @italic{learning} as being of critical importance.

@section{What's It Look Like?}

@(define my-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50])
     (make-evaluator 'racket/base
                     #:requires ' (data-table))))

@examples[#:eval my-evaluator
          ; A remote Google Spreadsheet that we want to manipulate
          (define test-url "http://bit.ly/2E2qZoI")
          
          ; Fetch the URL, and turn it into a table named "Testing"
          (define fetched (read-gsheet "Testing" test-url))
          
          ; Select all of the rows where the "age" column is greater than 6.
          (sieve fetched (> age 6))
          ]


@section{The Source and Tickets}

The source for this library can be found at @(hyperlink "https://bitbucket.org/jadudm/data-table" "Bitbucket"). The @(hyperlink "https://bitbucket.org/jadudm/data-table/issues?status=new&status=open" "issue tracker") is used for reporting bugs and feature requests; please feel free to use it for those purposes!


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

@;{ -------------------------------- pull ---------------------------------- }
@title[]{pull}
@defproc[#:link-target? false
         (pull
          [table table?]
          [column string?]
          )
         vector?]{ 
Pulls the values out of a column as a vector.
}

@(define-runtime-path pull.png (build-path "images" "pull.png"))
@centered{
  @(image #:scale 0.75 pull.png)
}


The previous examples have used @racket[pull] to extract values from tables. If we wanted to pull both the city and state from the north-easterly cities, and combine them into strings, it might look like:

@examples[#:eval the-eval
(for/list ([city  (pull north-easterlyT "City")]
           [state (pull north-easterlyT "State")])
  (format "~a, ~a" city state))
]

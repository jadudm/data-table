#lang racket
(require racket/draw)

(provide draw-select
         draw-sieve)

(define W 600)
(define H 300)

(define div 12)

(define (draw-row dc row col num fill)
  (send dc set-brush fill 'solid)
  (for ([n (range num)])
    (define x (* (* (/ 1 div) W) (+ row n)))
    ;; Offset?
    (when (> n 0)
      (set! x (- x n)))
    (define y (* (* (/ 1 div) col) W))
    (when (> col 0)
      (set! y (- y col)))
    (send dc draw-rectangle
          x y 
          (* (/ 1 div) W) (* (/ 1 div) W)))
  )

(define (draw-col dc row col num fill)
  (send dc set-brush fill 'solid)
  (for ([n (range num)])
    (define y (* (* (/ 1 div) W) (+ row n)))
    ;; Offset?
    (set! y (- y n 1))
    (define x (* (* (/ 1 div) col) W))
    (when (> col 0)
      (set! x (- x col)))
    (send dc draw-rectangle
          x y 
          (* (/ 1 div) W) (* (/ 1 div) W)))
  )

(define unit (* (/ 1 5) W))
(define (draw-arrow dc row col fill)
  (send dc set-brush fill 'solid)
  (define arrow (new dc-path%))
  (define start-x (+ 0 (* (/ 1 div) W row)))
  (define start-y (* (/ 1 div) W col))
  (send arrow move-to start-x start-y )
  (send arrow line-to (+ start-x (* unit 0.5)) start-y)
  (send arrow line-to (+ start-x (* unit 0.5)) (- start-y (* unit 0.25)))
  (send arrow line-to (+ start-x (* unit 1)) (+ start-y (* unit 0.125)))
  (send arrow line-to (+ start-x (* unit 0.5)) (+ start-y (* unit 0.5)))
  (send arrow line-to (+ start-x (* unit 0.5)) (+ start-y (* unit 0.25)))
  (send arrow line-to start-x (+ start-y (* unit 0.25)))
  (send arrow close)

  (send dc draw-path arrow)
  )

(define (draw-text dc text row col)
  (send dc set-font (make-font #:size 24))
  (define-values (w h d a) (send dc get-text-extent text))
  (define start-x (- (+ (* unit 0.5) (* (/ 1 div) W row)) w 5))
  (define start-y (+ (* (/ 1 div) W col) (* unit 0.26)))
  (send dc draw-text text start-x start-y))

(define (draw-select path)
  (define target (make-bitmap W H))
  (define dc (new bitmap-dc% [bitmap target]))
  (send dc set-pen "white" 3 'solid)
  (draw-row dc 0 0 3 "dimgray")
  (draw-row dc 0 1 3 "silver")
  (draw-row dc 0 2 3 "silver")
  (draw-row dc 0 3 3 "silver")
  (draw-col dc 1 0 3 "firebrick")

  (draw-arrow dc 4 2 "gold")

  (send dc set-pen "white" 3 'solid)
  (draw-row dc 7 0 1 "dimgray")
  (draw-row dc 7 1 1 "firebrick")
  (draw-row dc 7 2 1 "firebrick")
  (draw-row dc 7 3 1 "firebrick")

  (draw-text dc "select" 4 2)
  (send target save-file path 'png)

  ""
  )

(define (draw-sieve path)
  (define target (make-bitmap W H))
  (define dc (new bitmap-dc% [bitmap target]))
  (send dc set-pen "white" 3 'solid)
  (draw-row dc 0 0 3 "dimgray")
  (draw-row dc 0 1 3 "silver")
  (draw-row dc 0 2 3 "firebrick")
  (draw-row dc 0 3 3 "silver")

  (draw-arrow dc 4 2 "gold")

  (send dc set-pen "white" 3 'solid)
  (draw-row dc 7 0 3 "dimgray")
  (draw-row dc 7 1 3 "firebrick")
  
  (draw-text dc "sieve" 4 2)
  (send target save-file path 'png)

  ""
  )

;; (make-object image-snip% target)
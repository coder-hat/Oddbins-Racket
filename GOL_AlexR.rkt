#lang racket

; = 2019-3-05
; Racket code from Dr. Alex Rienhart's 2016-1-25 blog post
; "A flexible implementation of Conway's Game of Life"
; "the refsmmat report" - a blog at refsmmat.com


; blinker1 is part 1 of a period 2 oscillator pattern
; http://www.conwaylife.com/ref/lexicon/lex_b.htm#blinker
; useful as a starting configuration of live-cells
(define blinker1 (set '(1 1) '(2 1) '(3 1)))

; blinker2 is part 2 of a period 2 oscillator pattern
; http://www.conwaylife.com/ref/lexicon/lex_b.htm#blinker
(define blinker2 (set '(2 0) '(2 1) '(2 2))) 

; acorn is a starting configuration of live-cells
;  ##  ###
;     #
;   #
(define acorn (set '(1 1) '(2 1) '(2 3) '(4 2) '(5 1) '(6 1) '(7 1)))


(define (conway-rules neighbors alive?)
  (if alive? (or (= neighbors 2) (= neighbors 3))
      (= neighbors 3)))

(define (neighbors-rect location)
  (let ([x (first location)]
        [y (last location)])
    (for*/list ([dx '(-1 0 1)]
                [dy '(-1 0 1)]
                #:when (not (= dx dy 0)))
      (list (+ x dx) (+ y dy)))))

(define (count-occurrences neighbors)
  (for/fold ([ht (hash)])
            ([item neighbors])
    (hash-update ht item add1 0)))

; One simulation step, i.e. processes 1 generation
(define (step rules neighbors live-cells)
  (let ([num-neighbors (count-occurrences (append-map neighbors (set->list live-cells)))])
    (list->set (filter (lambda (cell)
                         (rules (hash-ref num-neighbors cell)
                                       (set-member? live-cells cell)))
                       (hash-keys num-neighbors)))))





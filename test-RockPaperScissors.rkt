#lang racket/base

(require racket/set
         rackunit
         "RockPaperScissors.rkt")

; Oscar Lopez, https://stackoverflow.com/users/201359/%c3%93scar-l%c3%b3pez
; https://stackoverflow.com/questions/25269308/how-to-implement-python-style-generator-in-scheme-racket-or-chezscheme
; Pass generator to make-grid, so test grid has a predictable sequence of distinct values.
(define (generator start stop step)
  (let ((current (- start 1)))
    (lambda ()
      (cond ((>= current stop) #f)
            (else (set! current (+ current step)) current)))
 ))

(test-case
 "Check generator."
 (let* ([g (generator 0 9 1)])
   (for ([i (in-range 9)])
     (check-equal? i (g)))
))

(test-case
 "Confirm dependency lookup table configured properly."
 (check-equal? (hash-ref id-looks-to id-rock) id-paper)
 (check-equal? (hash-ref id-looks-to id-paper) id-scissors)
 (check-equal? (hash-ref id-looks-to id-scissors) id-rock)
)

(test-case
 "Confirm random-id always produces rock, paper, or scissors id"
 (let ([valid-ids (list->set (vector->list id-options))]
       [grid (make-grid (* 250 250) random-id)])
   (for ([id grid])
     (check-true (set-member? valid-ids id)))))

(test-case
 "Check make-grid and neighbor-identities."
 (let* ([rows 3]
        [cols 3]
        [grid-length (* rows cols)]
        [initializer (generator 0 grid-length 1)]
        [grid (make-grid grid-length initializer)])
   ; Confirm the grid
   (for ([i (in-range grid-length)])
      (check-equal? i (vector-ref grid i)))
   ; Confirm neigbor-identities for center cell of grid
   ; 0 1 2
   ; 3 4 5
   ; 6 7 8
   (check-equal?
    (neighbor-identities 4 grid rows cols)
    '(0 1 2 3 5 6 7 8))
   (check-equal?
    (neighbor-identities 0 grid rows cols)
    '(8 6 7 2 1 5 3 4))
   (check-equal?
    (neighbor-identities 8 grid rows cols)
    '(4 5 3 7 6 1 2 0))
))

(test-case
 "Check cell-census."
 (check-equal?
  (cell-census (list->vector (list id-rock id-rock id-rock id-paper id-paper id-scissors)))
  (hash id-rock 3 id-paper 2 id-scissors 1))
)

(test-case
 "Check next-cell-id -> unchanged."
 (check-equal?
    (next-cell-id 4
                  (list->vector
                   (list
                    id-scissors id-scissors id-scissors
                    id-paper    id-rock     id-paper
                    id-scissors id-scissors id-scissors))
                  3 3)
    id-rock)
)

(test-case
 "Check next-cell-id -> changed."
 (check-equal?
    (next-cell-id 4
                  (list->vector
                   (list
                    id-scissors id-scissors id-scissors
                    id-paper    id-rock     id-paper
                    id-scissors id-paper    id-scissors))
                  3 3)
    id-paper)
)


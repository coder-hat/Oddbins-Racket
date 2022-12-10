#lang racket
(require math/base)
(require racket/vector)

(define id-rock 0)
(define id-paper 1)
(define id-scissors 2)

(define id-options (vector id-rock id-paper id-scissors))

(define id-looks-to (hash id-rock id-paper id-paper id-scissors id-scissors id-rock))

(define (random-id)
  (vector-ref id-options (random-integer 0 (vector-length id-options))))

(define (make-grid grid-length initializer)
  (for/vector #:length grid-length ([i (in-range grid-length)])
    (initializer)))

(define (i-val x-val y-val col-count)
  (+ x-val (* y-val col-count)))

(define (x-val i-val col-count)
  (modulo i-val col-count))

(define (y-val i-val col-count)
  (quotient i-val col-count))

(define (wrap v v-max)
  (cond [(< v 0) (sub1 v-max)]
        [(>= v v-max) 0]
        [else v]))

(define (neighbor-identities i-cell grid row-count col-count)
  (let* ([x-cell (x-val i-cell col-count)]
         [y-cell (y-val i-cell col-count)])
    (for/list ([xy-offsets `((-1 -1) (0 -1) (1 -1) (-1 0) (1 0) (-1 1) (0 1) (1 1))])
      (vector-ref grid
                  (i-val (wrap (+ x-cell (first xy-offsets)) col-count)
                         (wrap (+ y-cell (last xy-offsets)) row-count)
                         col-count)))))

(define (cell-census cell-ids)
  (for/fold ([id-counts (hash)])
            ([item cell-ids])
    (hash-update id-counts item add1 0)))

(define (next-cell-id i-cell grid row-count col-count)
  (let* ([id-counts (cell-census (neighbor-identities i-cell grid row-count col-count))]
         [cell-id (vector-ref grid i-cell)]
         [threshold-id (hash-ref id-looks-to cell-id)])
    (cond [(>= (hash-ref id-counts threshold-id 0) 3) threshold-id]
          [else cell-id])))

(define (next-grid grid row-count col-count)
  (let* ([grid-length (* row-count col-count)])
    (list->vector 
     (for/list ([i-cell (in-range (* row-count col-count))])
       (next-cell-id i-cell grid row-count col-count)))))

(provide id-rock
         id-paper
         id-scissors
         id-options
         id-looks-to
         random-id
         make-grid
         neighbor-identities
         cell-census
         next-cell-id)
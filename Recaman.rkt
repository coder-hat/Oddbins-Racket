#lang racket

; 2018-7-07
; Code to generate the Recaman number seqeuence
;
; References:
; 1. The Online Encyclopedia of Integer Sequences
; http://oeis.org/A005132
; 2. Brent Yorgey's "The Math Less Traveled" blog entry
; https://mathlesstraveled.com/2016/06/12/the-recaman-sequence/

; Computes and returns the next Recaman number that should appear in the sequence lst.
; Assumes and requries that lst contains the complete Recaman number seqeuence '(0 1 ...)
; up to the number that is returned.
(define (next-recaman-number lst)
  (if (empty? lst) 0
      (let* ([n (length lst)]
             [r-last (last lst)]
             [r-back (- r-last n)]
             [r-forward (+ r-last n)])
        (if (or (negative? r-back) (member r-back lst)) r-forward
            r-back))))

; Appends the next number in the Recaman integer sequence to lst.
; Requires and assumes that lst contains the complete, in-order, portion
; of the Recaman number sequence up to number that this function appends.
(define (append-next-recaman-number lst)
  (append lst (list (next-recaman-number lst))))

; Makes and returns a list containing the Recaman integer sequence [0, n]
; Iterative (for-loop) implementation.
(define (makitr-recaman-list n)
  (let ([lst '()])
    (for ([i n])
      (set! lst (append lst (list (next-recaman-number lst)))))
    lst))

; Makes and returns a list containing the Recaman integer sequence [0, n]
; Recursive implementation.
(define (makrec-recaman-list n)
  (let recaman-builder ([n n] [lst '()])
    (if (>= (length lst) n) lst
        (recaman-builder n (append-next-recaman-number lst)))))

    

#lang racket

; 2016-3-05 ksdj

; Returns the digits of i as a list of (single-digit) integers
; e.g. (integer->list 123) returns '(1 2 3)
(define (integer->list i)
  (if (< i 10)
      (list i)
      (append (integer->list (quotient i 10)) (list (remainder i 10)))))

; Returns the sum of the cubes of the digits of i
(define (cube-sum i)
  (for/sum ([j (map (lambda (i) (expt i 3)) (integer->list i))]) j))

; Returns the numbers that chain from i to 153, or i if not chainable
(define (chain-153 i)
  (if (or (<= i 0) (= i 153) (not (= 0 (remainder i 3))))
      (list i)
      (append (list i) (chain-153 (cube-sum i)))))

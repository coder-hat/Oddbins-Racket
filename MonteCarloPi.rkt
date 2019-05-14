#lang racket

; 2018-7-25
; Classic example of estimating Pi by Monte Carlo method.

; In quadrant I of a unit square with an inscribed quarter-circle of radius 1,
; containing a collection of randomly-generated, uniformly distributed 2-d points,
; the ratio of points within the circle to those without should equal pi / 4.

; Regarding the PRNG:
; According to Racket's documentation (4.2.2.7 Random Numbers)
; https://docs.racket-lang.org/reference/generic-numbers.html?q=random#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._random%29%29
; "The random number generator uses a 54-bit version of L’Ecuyer’s MRG32k3a algorithm [L'Ecuyer02]."

; encapsulates a 2-d point
(struct pt (x y) #:transparent)

; returns a pt with x and y values randomly chosen in range [0.0, 1.0)
(define (make-pt)
  (pt (random) (random)))

; returns magnitude of vector from origin to pt p
(define (magnitude p)
  (let ([x (pt-x p)]
        [y (pt-y p)])
    (sqrt (+ (* x x) (* y y)))))

; returns #t when vector origin->p's magnitude < 1.0
(define (in-quadrant p)
  (if (< (magnitude p) 1.0) #t #f))
  
; returns the number-of-trys randomly-generated points that lie with in the unit-quadrant.
(define (in-quadrant-count number-of-trys)
  (for/fold ([in-count 0])
            ([i number-of-trys])
    (+ in-count (if (in-quadrant (make-pt)) 1 0))))

; returns a pi estimate for the specified number-of-trys
(define (pi-estimate number-of-trys)
  (* (/ (in-quadrant-count number-of-trys) number-of-trys) 4))

; Show some estimates . . .
(define (show-estimates)
  (for ([i '(10 100 1000 10000 100000 1000000 10000000)])
    (printf "trys=~a pi-estimate=~a\n" i (exact->inexact (pi-estimate i)))))





        
        
        


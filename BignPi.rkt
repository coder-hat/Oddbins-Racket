#lang racket

; Code to compute the first 1000 digits of pi
; Original code was in Javascript, in a blog post by Andrew Jennings
; http://ajennings.net/blog/a-million-digits-of-pi-in-9-lines-of-javascript.html

(define start-x (* 3 (expt 10 1020)))

(define drop-digits (expt 10 20))

; Commented out code works fine, but recursive call computes i and x twice.
;(define (next-x i x)
;  (quotient (* x i) (* (+ i 1) 4)))

;(define (pi-nums i x pi)
;    (if (> x 0)
;      (pi-nums (+ i 2) (next-x i x) (+ pi (quotient (next-x i x) (+ i 2))))
;      pi))

; This code doesn't duplicate i and x computation in recursive call
; but requires a let*, is that "bad form"? is it harder to understand?
(define (pi-nums i x pi)
  (if (> x 0)
      (let* ([x (quotient (* x i) (* (+ i 1) 4))]
             [i (+ i 2)]
             [pi (+ pi (quotient x i))])
        (pi-nums i x pi))
      pi))

(define (compute-pi-digits start-x drop-digits)
  (quotient (pi-nums 1 start-x start-x) drop-digits))

; expected digits copy-pasted from a "first 1000 digits of pi" web search:
(define expect 31415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066470938446095505822317253594081284811174502841027019385211055596446229489549303819644288109756659334461284756482337867831652712019091456485669234603486104543266482133936072602491412737245870066063155881748815209209628292540917153643678925903600113305305488204665213841469519415116094330572703657595919530921861173819326117931051185480744623799627495673518857527248912279381830119491298336733624406566430860213949463952247371907021798609437027705392171762931767523846748184676694051320005681271452635608277857713427577896091736371787214684409012249534301465495853710507922796892589235420199561121290219608640344181598136297747713099605187072113499999983729780499510597317328160963185950244594553469083026425223082533446850352619311881710100031378387528865875332083814206171776691473035982534904287554687311595628638823537875937519577818577805321712268066130019278766111959092164201989)

(define actual (compute-pi-digits start-x drop-digits))

(displayln actual)

(= actual expect)

; ---- Tenetative prototype code from Peter Drake, 2019-10-05

;(define (f i x pi)
;  (if (<= x 0)
;      (/ pi (expt 10 20))
;      (f (+ i 2) 
;         (/ (* x i) (* (+ i 1) 4))
;         (+ pi (/ x (+ i 2))))))
;
;(define p0 (* 3 (expt 10 1020)))

;(print p0)

;(define p (f 1
;             10 ; This would also be p0
;             (* 3 (expt 10 1020))))
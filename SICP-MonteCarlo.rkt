#lang racket

; 2016-12-16
; H.Abelson, G.J.Sussman, J.Sussman
; Structure and Interpretation of Computer Programs
; pages 180-183

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ([= trials-remaining 0]
           (/ trials-passed trials))
          ([experiment]
           (iter (sub1 trials-remaining) (add1 trials-passed)))
          (else
           (iter (sub1 trials-remaining) trials-passed))))
  (iter trials 0))

;(define (gcd a b)
;  (if (= b 0) a
;      (gcd b (remainder a b))))

(define (cesaro-test)
  (= (gcd (random 1 4294967087) (random 1 4294967087)) 1))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))



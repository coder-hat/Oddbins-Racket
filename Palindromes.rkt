#lang racket

; Miscellaneous functions related to palindromic character sequences in strings.

; assumes s is length 1 or more
(define (last-char-index s)
  (- (string-length s) 1))

(define (equal-end-chars? s)
  (equal? (string-ref s 0) (string-ref s (last-char-index s))))

(define (drop-end-chars s)
  (if (> (string-length s) 1)
      (substring s 1 (last-char-index s))
      ""))

(define (is-palindrome? s)
  (cond [(or (equal? s null) (< (string-length s) 2)) #t]
        [(not (equal-end-chars? s)) #f]
        [else (is-palindrome? (drop-end-chars s))]))

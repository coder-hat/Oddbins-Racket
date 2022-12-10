#lang racket

(require racket/draw)
;(require 2htdp/image) "module: identifier already required also provided by: racket/draw in: make-color"
(require images/flomap) ; for scaling function 


; 2022-02-08
; Scaling with flomaps works, but interpolation removes aliasing, so pixels are fuzzy
; (flomap->bitmap (flomap-resize (bitmap->flomap bm) (* 86 4) (* 67 4)))

; Echo the lines of a (presumed) text file to console.
; (Included in this module as a dev convenience.)
(define (cat file-name)
  (call-with-input-file file-name
    (lambda (fi)
      (for ([line (in-lines fi)])
        (printf "~a\n" line)))))

; Return true if line starts with header key ("ncols", "nrows", ... etc)
(define (is-header-line line)
  (regexp-match? #rx"^ *[a-zA-Z_]+ " line))

; Split first and second whitespace-separated words in line
; as a key (lower-case'd string) and value (number).
(define (header-key-and-val line)
  (let ([tokens (string-split line)])
    (values (string-downcase (first tokens))    ; key
            (string->number (second tokens))))) ; value

; Load the header key and value pairs from asc-file-name into a hash table.
; All keys are lower-cased strings, all values are numbers.
(define (load-asc-header asc-file-name)
  (call-with-input-file asc-file-name
    (lambda (fi)
      (for/hash ([line (in-lines fi)]
            #:when (is-header-line line))
        (header-key-and-val line)))))

(define (asc-ncols asc-header)
  (hash-ref asc-header "ncols"))

(define (asc-nrows asc-header)
  (hash-ref asc-header "nrows"))

(define (asc-nodata asc-header)
  (hash-ref asc-header "nodata_value"))

; Split the whitespace-separated words in line into a list of numbers.
; Assumes that all the words in the line can be converted to numbers.
(define (line->numbers line)
  (for/list ([token (string-split line)])
    (string->number token)))

; Load the data from asc-file-name into a list.
; Header rows are skipped/ignored.
(define (load-asc-data asc-file-name)
  (call-with-input-file asc-file-name
    (lambda (fi)
      (for/fold ([asc-data '()])
                ([line (in-lines fi)]
                 #:unless (is-header-line line))
        (append asc-data (line->numbers line))))))

(define (asc-filter-data asc-data v-exclude)
  (for/list ([v asc-data] #:unless (= v v-exclude)) v))

(define (asc-min-max asc-data)
  (cons (apply min asc-data) (apply max asc-data)))
             
(define (asc-cell-color v no-data-value)
  (if (= v no-data-value)
      (make-color 0 0 0 1.0)
      (make-color 64 128 255 1.0)))

; https://stackoverflow.com/questions/5731863/mapping-a-numeric-range-onto-another
(define (color-norm v v-min v-max)
  (cond [(< v v-min) 64]
        [(> v v-max) 255]
        [else (inexact->exact(+ 64 (floor(* (/ (- 255 64) (- v-max v-min)) (- v v-min)))))]))

(define (asc-cell-colorist asc-header asc-data)
  (let* ([no-data (asc-nodata asc-header)]
         [min-max (asc-min-max (asc-filter-data asc-data no-data))]
         [red-val (lambda (v) (color-norm v (car min-max) (cdr min-max)))])
    (printf "red-val=~a min-max=~a" red-val min-max)
    (lambda (v) (if (= v no-data)
                    (make-color 0 0 0 1.0)
                    (make-color (red-val v) (red-val v) (red-val v) 1.0)))))
 
(define (asc-bitmap asc-header asc-data)
  (let* ([asc-width (asc-ncols asc-header)]
         [asc-height (asc-nrows asc-header)]
         [asc-total-cells (* asc-width asc-height)]
         [no-data (hash-ref asc-header "nodata_value")]
         [cell-color (asc-cell-colorist asc-header asc-data)]
         [xloc (lambda (i) (modulo i asc-width))]
         [yloc (lambda (i) (quotient i asc-width))]
         [asc-bitmap (make-bitmap asc-width asc-height)]
         [asc-dc (new bitmap-dc% (bitmap asc-bitmap))])
    (for/fold ([i 0])
              ([v asc-data])
      ;(printf "~a:~a, " i v)
      (send asc-dc set-pixel (xloc i) (yloc i) (cell-color v))
      (+ i 1))
    asc-bitmap))

(define (scale-asc-bitmap asc-bitmap scale)
  (flomap->bitmap (flomap-scale (bitmap->flomap asc-bitmap) scale)))



    
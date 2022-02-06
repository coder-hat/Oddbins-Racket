#lang racket

(require racket/draw)

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

(define (asc-cell-color v no-data-value)
  (if (= v no-data-value)
      (make-color 0 0 0 1.0)
      (make-color 0 128 255 1.0)))

(define (asc-bitmap asc-header asc-data)
  (let* ([asc-width (hash-ref asc-header "ncols")]
         [asc-height (hash-ref asc-header "nrows")]
         [asc-total-cells (* asc-width asc-height)]
         [no-data (hash-ref asc-header "nodata_value")]
         [cell-color (lambda (v) (asc-cell-color v no-data))]
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


    
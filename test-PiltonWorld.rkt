#lang racket

(require rackunit "PiltonWorld.rkt")

; ----- Helper functions

; Creates a list of particle structs from the "shorthand" list of partticle values.
; Assumes lst has the form ((x1 y1 m1) (x2 y2 m2) ... )
(define (make-particles lst)
  (for/list ([pvals (in-list lst)]) (particle (first pvals) (second pvals) (third pvals))))

; Returns true if lst1 and lst2 have exactly the same elements, disregarding element order.
; Does not consider deep equality of the elements.
(define (same-elements? lst1 lst2)
  (cond [(and (empty? lst1) (empty? lst2)) #t]
        [(or (empty? lst1) (empty? lst2)) #f]
        [else (if (member (first lst1) lst2)
                  (same-elements? (rest lst1) (remove (first lst1) lst2))
                  #f)]))

; ----- Unit Tests follow

(check-true (same-elements?
             (list (particle 1 1 1) (particle 2 2 2) (particle 3 3 3))
             (list (particle 2 2 2) (particle 3 3 3) (particle 1 1 1)))
            "check same-elements for equal lists")

(check-false (same-elements?
              (list (particle 1 1 1) (particle 2 2 2) (particle 3 4 3))
              (list (particle 2 2 2) (particle 3 3 3) (particle 1 1 1)))
             "check same-elements for unequal lists")

(check-equal? (get-location (particle 1 2 3))
              '(1 2)
              "simple get-location")

; ---- decay-particle tests assume row and column counts of 7

(check-equal? (decay-particle (particle 3 2 1) 0)
              (list (particle 3 2 1))
              "decay-particle no-decay")

(check-equal? (decay-particle (particle 3 2 1) 10)
              (list (particle 2 2 1) (particle 4 2 1))
              "decay-particle x-decay")

(check-equal? (decay-particle (particle 3 2 1) 9)
              (list (particle 3 1 1) (particle 3 3 1))
              "decay-particle x-decay")

(check-equal? (decay-particle (particle 1 1 1) 1)
              (list (particle 0 0 1) (particle 0 2 1) (particle 2 0 1) (particle 2 2 1))
              "decay-particle 4-decay")

(test-case
 "coalesce-particles test"
 (let* ([source (make-particles '((0 0 1) (4 2 1) (2 4 1) (0 0 1) (5 5 1) (2 4 1) (0 0 1) (4 2 1) (0 0 1)))]
        [expect (make-particles '((0 0 4)  (4 2 2)  (2 4 2)  (5 5 1)))]
        [actual (coalesce-particles source)])
   (check-true (same-elements? actual expect))))
   
       
   
       




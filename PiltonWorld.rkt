#lang racket

; An implementation of the "Small World" described in Barry Pilton's article:
; "A Small World", Manifold, Autumn 1969 issue, pgs 49-53.
; https://ianstewartjoat.weebly.com/manifold-5.html
;
; This code by coder-hat

; The dimensions of the (toroidal) world grid.
(define number-of-cols 7)
(define number-of-rows 7)

; Functions that get the previous or next column or row index in the toroidal world grid.
(define (col-sub1 x) (modulo (sub1 x) number-of-cols))
(define (col-add1 x) (modulo (add1 x) number-of-cols))
(define (row-sub1 y) (modulo (sub1 y) number-of-rows))
(define (row-add1 y) (modulo (add1 y) number-of-rows))

; NOTE 2018-7-08
; The Racket Guide (http://docs.racket-lang.org/guide/define-struct.html#%28part._trans-struct%29)
; says that "... opaque structure instances provide more encapsulation guarantees."
; This code currently emphasizes ease-of-analysis, so the structs are transparent.

; A particle is a location, a mass and a molecule membership ID
(struct particle (x y mass) #:transparent)

; Convenience function that gets the x,y location of particle p.
(define (get-location p)
  (list (particle-x p) (particle-y p)))
  

; A world is a moment in time and a list of all the particles in the world.
(struct world (timestep particles) #:transparent)

; Returns #t when Particles p1 and p2 share the same location, otherwise #f.
(define (location-match? p1 p2)
  (and (equal? (particle-x p1) (particle-x p2))) (equal? (particle-y p1) (particle-y p2)))

; Particle p at (x,y) with mass m decays at timestep t if and only if:
; 1. t is a multiple of m, i.e. t mod m is zero.
; 2. x = t mod cols OR y = t mod rows
(define (decay-particle p t)
  (let ([m (particle-mass p)])
    (if (equal? 0 (modulo t m))
        (let* ([x (particle-x p)]
               [y (particle-y p)]
               [x-decays (equal? x (modulo t number-of-cols))]
               [y-decays (equal? y (modulo t number-of-rows))])
          (cond [(and x-decays y-decays)
                 (list (particle (col-sub1 x) (row-sub1 y) m)
                       (particle (col-sub1 x) (row-add1 y) m)
                       (particle (col-add1 x) (row-sub1 y) m)
                       (particle (col-add1 x) (row-add1 y) m))]
                [x-decays
                 (list (particle (col-sub1 x) y m) (particle (col-add1 x) y m))]
                [y-decays
                 (list (particle x (row-sub1 y) m) (particle x (row-add1 y) m))]
                [else
                 (list p)]))
        (list p))))

; "If at any time t, two particles of masses m, n are both in the same place,
;  they coalesce to form a single particle of mass m+n.
(define (coalesce-particles lst)
  (let ([h (make-weak-hash)])
    ; Accumulate masses of co-located particles from list lst into hash map h.
    (for ([p (in-list lst)])
      (let ([p-location (get-location p)])
        (hash-set! h p-location (+ (particle-mass p) (hash-ref h p-location 0)))))
    ; Make list of unique particle locations (h keys) and accumulated masses (h values)
    (for/list ([(k v) (in-hash h)]) (particle (first k) (last k) v))))




; ----- Exposed for unit testing
(provide particle
         get-location
         decay-particle
         coalesce-particles)

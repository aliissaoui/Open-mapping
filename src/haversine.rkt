#lang racket
(require "hash-graph.rkt")
(require "make_graph.rkt")
(provide (all-defined-out))



(define (difference_latitude_deg s1 s2)                    ;d1
  (* (- (vertex-lat s2) (vertex-lat s1)) 0.017) 
 )

(define (difference_longitude_deg s1 s2)                   ;d2
 (* (- (vertex-lon s2) (vertex-lon s1)) 0.017) 
 )

(define (haversine s1 s2)
  (let*
      ([d1 (difference_latitude_deg s1 s2)]
       [d2 (difference_longitude_deg s1 s2)]
       [hav1 (+ (expt (sin (/ d1 2)) 2) (* (cos (vertex-lat s2)) (cos (vertex-lat s1)) (expt (sin (/ d2 2)) 2)) 
                                        )])
    (* 12742 (asin (sqrt hav1)))                           ;12742 = 2*le rayon de la terre=2* 6371 
  ))
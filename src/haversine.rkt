#lang racket

(struct vertex (id lat lon way))


(define (difference_latitude_deg s1 s2) ;; d1
  (* (- (vertex-lat s2) (vertex-lat s1)) 0.017) 
 )

(define (difference_longitude_deg s1 s2) ;; d2
 (* (- (vertex-lon s2) (vertex-lon s1)) 0.017) 
 )

(define (haversine s1 s2)
  (let*
      ([d1 (difference_latitude_deg s1 s2)]
       [d2 (difference_longitude_deg s1 s2)]
       [hav1 (+ (expt (sin (/ d1 2)) 2) (* (cos (vertex-lat s2)) (cos (vertex-lat s1)) (expt (sin (/ d2 2)) 2)) 
                                        )])
    (* 12742 (asin (sqrt hav1))) ;12742 = 2*le rayon de la terre=2* 6371 
  ))
    
  

#|(define v (vertex 2 5 1 '(5 6)))
(define w (vertex 3 0 9 '(2 6)))

(haversine v w)
(haversine w v)
|#
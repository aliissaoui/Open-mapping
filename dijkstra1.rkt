#lang racket

(define ht (make-hash))

(struct vertex (id lat lon way))
(struct graph (vx-ht)) 

#|(define v1 (vertex 1 3.2 4.7 '(2 3)))
(define v2 (vertex 2 2.0 8.5 '(4 5 6 1)))
(define v3 (vertex 3 1.5 9.1 '(7 8 1)))
(define v4 (vertex 4 1.0 0.5 '(2)))
(define v5 (vertex 5 5.2 8.4 '(9 2)))
(define v6 (vertex 6 4.1 9.8 '(2)))
(define v7 (vertex 7 14.0 2.4 '(3)))
(define v8 (vertex 8 12.4 7.9 '(3)))
(define v9 (vertex 9 13.8 2.1 '(5)))


(hash-set! ht 1 v1)
(hash-set! ht 2 v2)
(hash-set! ht 3 v3)
(hash-set! ht 4 v4)
(hash-set! ht 5 v5)
(hash-set! ht 6 v6)
(hash-set! ht 7 v7)
(hash-set! ht 8 v8)
(hash-set! ht 9 v9)
|#


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
    
;(vertex-way (hash-ref (graph-vx-ht g) 2))   

#|(define v (vertex 2 5 1 '(5 6)))
(define w (vertex 3 0 9 '(2 6)))

(haversine v w)
(haversine w v)
|#

(define (initialisation id_sommet liste_id_sommet l) ;d une liste initialement vide ; sommet est id ; liste_sommet est une liste des id des sommets du graph
  (match liste_id_sommet
    ['() l]
    [(cons v tail)
     (initialisation id_sommet tail (cons (cons v (if (eq? v id_sommet) 0  'infini)) l))]))

#|(define v (vertex 2 5 1 '(5 6)))
(define w (vertex 3 0 9 '(2 6)))
(define t (vertex 1 8 1 '(5 6)))
(define z (vertex 4 7 9 '(9 6)))
(define l '(v w z t))
(define d '())
(initialisation 'v l '())
|#

(define (data id gr)              
  (hash-ref (graph-vx-ht gr) id)) 

(define (min_dis liste_sommet sdep dis d) ; dis contient initialement une valeur 10**36 superieure aux distances séparant chaque deux points du graph par exe 
  (cond [(null? liste_sommet) d]
        [(<= (haversine sdep (car liste_sommet)) d)] ;recuperer toute la structure vertex
        [else (min_dis sdep (cdr liste_sommet))
        d])
  )


#|(define (maj_distance sdep s1 s2 pred dis )
  (if (> (haversine sdep s2) (+ (haversine sdep s2) (haversine s2 s1)))
      (maj_dis dis s2 (+ (haversine sdep s2) (haversine s2 s1)))
      (void)))
  
(define (maj_dis dis s d)
  (cond [(eq? s (car (car dis)))
         (set! (cdr (car dis)) 'd )]
        [else (void)]
        )
  dis)

(define (maj_pred pred s1 s2) ;le tableau des predecesseurs est un tableau
  (cond [(eq? s2 (car (car pred)))
         (set! (cdr (car pred)) s1 )]
        [else (void)]
        pred))
     

  


      ;((+ (haversine sdep s2) (haversine s2 s1)) & (cons pred 
      ;(haversine sdep s2))))


  |#    
  
  



                                         











  



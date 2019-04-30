#lang racket
(require "hash-graph.rkt")
(require "make_graph.rkt")
(define dmax (expt 10 10)) 


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

(define (initial id_sommet liste_id_sommet l) ;d une liste initialement vide ; sommet est id ; liste_sommet est une liste des id des sommets du graph
  (match liste_id_sommet
    ['() l]
    [(cons v tail)
     (initial id_sommet tail (cons (cons v (if (eq? v id_sommet) 0  1000000000000000))
                                          l))]))

(define (distance liste_double s)
 (cond [(null? liste_double) (void)]
    [(eq? (car (car liste_double)) s)
      (cadar liste_double)]
      [else (distance (cdr liste_double) s)]
      ))

;(distance '((4 . 1000000000000000) (1 . 1000000000000000) (3 . 1000000000000000) (2 . 0)) 2)      


#|(define v (vertex 2 5 1 '(5 6)))
(define w (vertex 3 0 9 '(2 6)))
(define t (vertex 1 8 1 '(5 6)))
(define z (vertex 4 7 9 '(9 6)))
(define l (list v w z t))
(define l1 '(2 3 1 4))
(define d '())
|#

;;(initial 2 l1 '())


;;(define (data id gr)              
  ;;(hash-ref (graph-vx-ht gr) id)) 

;(vertex-way (hash-ref (graph-vx-ht g) 2)) 

(define (min_dis liste_sommet sdep d smin)                                        ;d est initialement une très grande distance                                                    
  (cond [(null? liste_sommet) (cons (vertex-id smin) d)]                          ;séparant chaque deux points du graph, 10**36 par exemple et liste_sommet est une liste de vertex
        [(and (<= (haversine sdep (car liste_sommet)) d) (not(eq? (car liste_sommet) sdep)))
         (min_dis (cdr liste_sommet) sdep (haversine sdep (car liste_sommet)) (car liste_sommet))]                   
        [else (min_dis (cdr liste_sommet) sdep d smin)]
        )
  )

;;(min_dis l v 100000000000000000 w)


(define (maj_tab_dis s d liste_double)                                                ;met à jour le tableau des distances
  (if (eq? s (caar liste_double))
      (cons (list s d) (cdr liste_double)) 
      (cons (car liste_double) (maj_tab_dis s d (cdr liste_double))))         
  )

;(maj_dis 1 10 '((4 . infini) (1 . infini) (3 . infini) (2 . 0))) 

(define (maj_pred2 pred s1 s2)
  (map 
      (lambda (x) ( if ( eq? (cadr x) s2 ) (list s1 s2) x ) )pred ))



;(maj_pred2 pred 5 7)


#|(define s1 (vertex 3 0 9 '(2 6)))
(define s2 (vertex 1 8 1 '(5 6)))
(define s4 (vertex 3 0 9 '(2 6)))
(define s5 (vertex 1 8 1 '(5 6)))
|#

;;(define pred '((v1 v2) (v2 v3) (v3 v4) (v5 v6) (v6 v7) (v7 v8) (v8 v9)))

;(define pred '((1 1)(2 5)(3 7)(4 2)(5 2)(6 2)(7 3)(8 3)(9 10)(10 9)))


(define (maj_distance sdep s1 s2 pred dis)                                  ;met à jour la valeur de la distance et le predecesseur

  (if (> (distance dis s2) (+ (distance dis s1) (haversine (hash-ref (graph-vx-ht test) s2) (hash-ref (graph-vx-ht test) s1)))) ;; il faut faire en sorte que seul le vertex soit recupéré en entier
      
  ;(if (> ((distance dis s2) (+ (distance dis s1) (haversine s2 s1))))

      (cons (maj_tab_dis s2 (+ (haversine (hash-ref (graph-vx-ht test) sdep) (hash-ref (graph-vx-ht test) s2)) (haversine (hash-ref (graph-vx-ht test) s2) (hash-ref (graph-vx-ht test) s1))) dis)
            (maj_pred2 s1 s2))
      (cons dis pred))
  )

#|(define pred '((1 1)(2 5)(3 7)(4 2)(5 2)(6 2)(7 3)(8 3)(9 10)(10 9)))

(define dis '((1 0)(2 248)(3 248.6)(4 400.9)(5 314.5)(6 248.6)(7 248.6)(8 400.9)(9 400.9)(10 497.2)))

(maj_distance 1 3 4 pred dis)
|#


(define (maj_dis_voisin sdep s1 lv pred dis)
  (foldl (maj_distance sdep s1 pred dis) lv))     

(define (dijkstra-loop q dis pred sdep)                        ;q contient initialement la liste des id de tous les sommets
  (if (null? q)
      (cons dis pred)
      (let* ([s1 (min_dis q sdep dmax sdep)]
             [q (remove s1 q)]
             [lv  (vertex-way ((hash-ref (graph-vx-ht test) s1 )))])
             (maj_dis_voisin sdep s1 lv)       
             (dijkstra-loop q dis pred sdep))))



(define (dijkstra q sdep )
  (let
      ((dis (initial sdep q '()))
       (pred '())
       )
    (dijkstra-loop q dis pred sdep)))


#|(define (maj_pred_ancien pred s1 s2)                                               ;le predecesseur de s2 est s1 (s1 s2) 
  (cond [(eq? s2 (cdr (car pred)))
         (cons (list s1 s2) (cdr pred))]
        [else (maj_pred (cdr pred) s1 s2)])
        pred)

|#





  






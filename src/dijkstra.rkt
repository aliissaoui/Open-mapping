#lang racket
(require "hash-graph.rkt")
(provide (all-defined-out))

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

(define (initial id_sommet liste_id_sommet l) ;d une liste initialement vide ; sommet est id ; liste_sommet est une liste des id des sommets du graph et l est donnée initialement vide
  (match liste_id_sommet
    ['() l]
    [(cons v tail)
     (initial id_sommet tail (cons (cons v (if (eq? v id_sommet) 0  1000000000000000))
                                          l))]))

(define (distance tab_dis s)
 (cond [(null? tab_dis) (void)]
    [(eq? (car (car tab_dis)) s)
      (cdr (car tab_dis))]
      [else (distance (cdr tab_dis) s)]
      ))

;;(distance '((4 . 1000000000000000) (1 . 1000000000000000) (3 . 1000000000000000) (2 . 0)) 3)      


#|(define v (vertex 2 5 1 '(5 6)))
(define w (vertex 3 0 9 '(2 6)))
(define t (vertex 1 8 1 '(5 6)))
(define z (vertex 4 7 9 '(9 6)))
(define l (list v w z t))
(define l1 '(2 3 1 4))
(define d '())
|#
;(initial 2 l1 '())


;;(define (data id gr)              
;; (hash-ref (graph-vx-ht gr) id)) 

;(vertex-way (hash-ref (graph-vx-ht g) 2)) 

(define (min_dis liste_sommet sdep d smin)                                        ;d est initialement une très grande distance                                                    
  (cond [(null? liste_sommet) (cons (vertex-id smin) d)]                          ;séparant chaque deux points du graph, 10**36 par exemple et liste_sommet est une liste de vertex
        [(and (<= (haversine sdep (car liste_sommet)) d) (not(eq? (car liste_sommet) sdep)))
         (min_dis (cdr liste_sommet) sdep (haversine sdep (car liste_sommet)) (car liste_sommet))]                   
        [else (min_dis (cdr liste_sommet) sdep d smin)]
        )
  )

;(min_dis l v 100000000000000000 w)


(define (maj_tab_dis s d dis)                                                ;met à jour le tableau des distances
  (if (eq? s (car (car dis)))
      (cons (list s d) (cdr dis)) 
      (cons (car dis) (maj_tab_dis s d (cdr dis))))         
  )

;(maj_dis 1 10 '((4 . infini) (1 . infini) (3 . infini) (2 . 0))) 

(define (maj_pred pred s1 s2)                                               ;le predecesseur de s2 est s1 (s1 s2) 
  (cond [(eq? s2 (cdr (car pred)))
         (cons (list s1 s2) (cdr pred))]
        [else (maj_pred (cdr pred) s1 s2)])
        pred)

(define pred '((1 2)(2 4)(3 7)(4 2)(5 9)(6 2)(7 3)(8 3)(9 5)))
;(maj_pred pred 5 7)


#|(define (maj_distance sdep s1 s2 pred dis)                                  ;met à jour la valeur de la distance et le predecesseur
  (if (> ((distance dis s2) (+ (distance dis s1) (haversine (hash-ref (graph-vx-ht g) s2) (hash-ref (graph-vx-ht g) s1))))) ;; il faut faire en sorte que seul le vertex soit recupéré en entier
     (cons (maj_tab_dis s2 (+ (haversine sdep s2) (haversine s2 s1)) dis)
           (maj_pred s1 s2))
  (cons dis pred))
  )

(maj_distance 1 9 1 pred liste_double)

(define (maj_dis_voisin sdep s1 lv pred dis)
  (foldl (lambda(x) (maj_distance sdep s1 x pred dis)) lv))     


(define (dijkstra-loop q dis pred sdep)                                        ;q contient initialement la liste des id de tous les sommets
  (if (null? q)
      (cons dis pred)
      ((let ((s1 (min_dis q sdep dmax sdep))
            (q (remove s1 q))
            (lv  (vertex-way (data s1 q ))))
            (maj_dis_voisin sdep s1 lv)       
       (maj_dis_voisin sdep s1 lv)
       (dijkstra-loop q dis pred sdep)))))

(define (dijkstra q sdep )
  (let
      ((dis (initial sdep q '()))
       (pred '())
       )
    (dijkstra-loop q dis pred sdep)))




Dijkstra(G,Poids,sdeb)
1 Initialisation(G,sdeb)
2 Q := ensemble de tous les nœuds
3 tant que Q n'est pas un ensemble vide faire
4       s1 := Trouve_min(Q)
5       Q := Q privé de s1
6       pour chaque nœud s2 voisin de s1 faire
7           maj_distances(s1,s2)
8       fin pour
9 fin tant que

|#





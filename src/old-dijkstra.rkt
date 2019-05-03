#lang racket
(require "hash-graph.rkt")
(require "make_graph.rkt")
(require "haversine.rkt")
(provide (all-defined-out))
(define dmax (expt 10 10)) 


(define liste_id_sommet '(1 2 3 4 5 6 7 8 9 10))

(define (initial sdep liste_id_sommet l)                                       ;l est toujours donnée initialement vide
  (match liste_id_sommet                                                       ;liste_sommet est une liste des id des sommets du graph
    ['() l]
    [(cons v tail)
     (initial sdep tail (cons (cons v (if (eq? v sdep) 0  1000000000000000))
                                          l))]))

(write "Test de initial()")
(define liste_double (initial 1 liste_id_sommet '()))
liste_double

(define (initial_pred liste_id_sommet)
  (map (lambda (x) (cons x x)) liste_id_sommet))

(write "Test de initial_pred")
(define pred (initial_pred liste_id_sommet))
pred

(define (distance liste_double s)
 (cond [(null? liste_double) (void)]
    [(eq? (car (car liste_double)) s)
      (cdar liste_double)]
      [else (distance (cdr liste_double) s)]
      ))

(write "Test de distance()")
(distance liste_double 2)      

(define (id-to-vertex g id)              
  (hash-ref (graph-vx-ht g) id)) 

;(vertex-way (hash-ref (graph-vx-ht g) 2)) 

(define (min_dis liste_id_sommet sdep d smin)                                                ;d est initialement une très grande distance, on prend dmax dans notre cas                                                    
  (cond [(null? liste_id_sommet) (cons (vertex-id (id-to-vertex test smin)) d)]                                 
        [(and (<= (haversine (id-to-vertex test smin) (id-to-vertex test (car liste_id_sommet))) d) (not(eq? (car liste_id_sommet) sdep)))
         (min_dis (cdr liste_id_sommet) sdep (haversine (id-to-vertex test smin) (id-to-vertex test (car liste_id_sommet))) (car liste_id_sommet))]                   
        [else (min_dis (cdr liste_id_sommet) sdep d smin)]
        )
  )

(write "Test de min_dis()")
(min_dis liste_id_sommet 1 dmax 1)

(define (maj_tab_dis s d liste_double)                                                       ;met à jour la liste_double en mettant la distance 
  (if (eq? s (caar liste_double))                                                            ;correspondant au sommet s
      (cons (list s d) (cdr liste_double)) 
      (cons (car liste_double) (maj_tab_dis s d (cdr liste_double))))         
  )

(write "Test de maj_tab_dis()")
(maj_tab_dis 1 dmax liste_double) 

(define (maj_pred pred s p)                                              ;à l'issue de cette procédure le predecesseur de s doit etre p
  (if (eq? s (caar pred))                                                                    
      (cons (list s p) (cdr pred)) 
      (cons (car pred) (maj_pred (cdr pred) s p)))         
  )
  
(write "Test de maj_pred()")
(maj_pred '((5 0) (6 2)) 5 7)

;(define pred '((v1 v2) (v2 v3) (v3 v4) (v5 v6) (v6 v7) (v7 v8) (v8 v9)))
;(define pred '((1 1)(2 5)(3 7)(4 2)(5 2)(6 2)(7 3)(8 3)(9 10)(10 9)))


(define (maj_distance sdep s1 s2 pred dis)                                             ;met à jour la valeur de la distance et le predecesseur

  (if (> (distance dis s2) (+ (distance dis s1)
                              (haversine (hash-ref (graph-vx-ht test) s2)
                                         (hash-ref (graph-vx-ht test) s1))))           ;il faut faire en sorte que seul le vertex soit recupéré en entier
      (cons (maj_tab_dis s2 (+ (haversine (hash-ref (graph-vx-ht test) sdep)
                                          (hash-ref (graph-vx-ht test) s2))
                               (haversine (hash-ref (graph-vx-ht test) s2)
                                          (hash-ref (graph-vx-ht test) s1))) dis)
            (maj_pred s1 s2))
      (cons dis pred))
  )

(write "Test de maj_distance()")
(maj_distance 1 2 3 pred liste_double)



#|(define (maj_dis_voisin sdep s1 lv pred liste_double)
  (foldl (lambda (x) (maj_distance sdep s1 x pred liste_double)) lv '()))     

(write "Test de maj_dis_voisin()")
(maj_dis_voisin 1 2 (vertex-way (id-to-vertex test 2)) pred liste_double)


(define (dijkstra-loop q dis pred sdep)                        ;q contient initialement la liste des id de tous les sommets
  (if (null? q)
      (cons dis pred)
      (let* ([s1 (min_dis q sdep dmax sdep)]
             [q (remove s1 q)]
             [lv  (vertex-way ((hash-ref (graph-vx-ht test) s1 )))]
             [maj (maj_dis_voisin sdep s1 lv pred dis)])       
             (dijkstra-loop q (car maj) (cdr maj) sdep))))


(define (dijkstra q sdep )
  (let
      ((dis (initial sdep q '()))
       (pred '())
       )
    (dijkstra-loop q dis pred sdep)))
|#

  






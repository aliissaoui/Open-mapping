#lang racket
(require xml)
(require "hash-graph.rkt")
(require "dijkstra.rkt")

(provide (all-defined-out))

(define (nearest_dist vx l mini_vx mini_dist)
  (cond
    [(eq? 0 (length l)) mini_vx]
    [(< (haversine (cdr vx) (cdr (car l))) mini_dist) (nearest_dist vx (cdr l) (car l) (haversine (cdr vx) (cdar l)))] ; on calcule haversine 2 fois pour le meme vx, je devrais e stocker plutot
    [else (nearest_dist vx (cdr l) mini_vx mini_dist)]
  ))

(define (nearest_aux vx l)
  (cond
    [(eq? 0 (length l)) (list vx)]
    [else (let* ([new_l (remq vx l)] [next (nearest_dist vx l vx +inf.0)]) (list* vx (nearest_aux next (remq next new_l))))]
   ))
  

(define (nearest g) ; g is a struct graph 
  (let* ([g2 (hash->list (graph-vx-ht g))][start (car g2)])
  (map (lambda (a) (car a)) (nearest_aux start (remq start g2)))
))



(define (farthest_dist vx l maxi_vx maxi_dist)
  (cond
    [(eq? 0 (length l)) maxi_vx]
    [(> (haversine (cdr vx) (cdr (car l))) maxi_dist) (farthest_dist vx (cdr l) (car l) (haversine (cdr vx) (cdar l)))] ; on calcule haversine 2 fois pour le meme vx, je devrais e stocker plutot
    [else (farthest_dist vx (cdr l) maxi_vx maxi_dist)]
  ))

(define (farthest_aux vx l)
  (cond
    [(eq? 0 (length l)) (list vx)]
    [else (let* ([new_l (remq vx l)] [next (farthest_dist vx l vx 0)]) (list* vx (farthest_aux next (remq next new_l))))]
   ))
  

(define (farthest g) ; g is a struct graph 
  (let* ([g (hash->list (graph-vx-ht g))][start (car g)])
  (map (lambda (a) (car a)) (farthest_aux start (remq start g)))
))



(define (rand_tsp_aux vx l)
  (cond
    [(eq? 0 (length l)) (list vx)]
    [else (let* ([new_l (remq vx l)] [next (list-ref l (random 0 (length l)))]) (list* vx (rand_tsp_aux next (remq next new_l))))]
   ))
  

(define (rand_tsp g) ; g is a struct graph 
  (let* ([g (hash->list (graph-vx-ht g))][start (car g)])
  (map (lambda (a) (car a)) (rand_tsp_aux start (remq start g)))
))



(define (parcourt visited new dist index best_index )
  (cond
    [(>= index (length visited)) (let-values ([(head tail) (split-at visited best_index)]) (list head tail))]
    [ else ;(display (car new)) (display "-") (display index) (display "/")(displayln (length visited))
      (let ([new_dist (get-dist (list-ref visited (- index 1)) new (list-ref visited index))])
      (if (< new_dist dist) (parcourt visited new new_dist (+ index 1) index) (parcourt visited new dist (+ index 1) best_index)))
      ]
  ))

(define (get-dist id1 id2 id3)
  (+ (haversine (cdr id1) (cdr id2))
     (haversine (cdr id2) (cdr id3))
     ))

(define (greedy_aux visited remaining)
  (cond
    [(null? remaining) visited]
    [else (let* ([new (car remaining)] 
                 [dist_new (get-dist (last visited) new (first visited))] ; on insère le noeud à visiter en début et on a un cycle
                 [res (parcourt visited new dist_new 1 0)])
            ;(displayln visited)
            (greedy_aux (append (first res) (list new) (second res)) (remq new remaining))
          )]
  ))


(define (greedy g ids) ; g is a struct graph
  (let* ([l (hash->list (graph-vx-ht g))])
  (cond
    [(null? l) '()]
    [(eq? 1 (hash-count (graph-vx-ht g))) (list (car l))]
    [else (let* ([fst (first l)][snd (second l)])
          (map (lambda (a) (car a)) (greedy_aux (list fst snd) (remq fst (remq snd l)))))
          ]
  )))

(define (recup_vertex g ids)
  (list (map (lambda (a) (cons a (hash-set g a)))))
  )


;;(greedy g '(6270202250 6270202257 6270202272 6270277022)) ;; Voici mon test ( test c'est le simple graphe défini dans hash-graph )



;;;; test zone



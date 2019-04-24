#lang racket
(require xml)
(require "hash-graph.rkt")
(require "make_graph.rkt")
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
  (let* ([g (hash->list (graph-vx-ht g))][start (car g)])
  (list (map (lambda (a) (car a)) (nearest_aux start (remq start g))))
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
  (list (map (lambda (a) (car a)) (farthest_aux start (remq start g))))
))

;;;; test zone

(define test-list (hash->list test))
test-list


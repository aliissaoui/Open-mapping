#lang racket

(require "hash-graph.rkt")
(provide (all-defined-out))
;;Retourne la liste de toutes les clés d'un graphe

(define (keys g)
 (hash-keys (graph-vx-ht g)))
;;Retourne la liste de toutes les latitudes
(define (list-lat g)
  (map (lambda (v) (vertex-lat v)) (hash-values (graph-vx-ht g))))
;;Retourne la liste de toutes les longitudes
(define (list-lon g)
  (map (lambda (v) (vertex-lon v)) (hash-values (graph-vx-ht g))))
;; Les 4 suivantes fonctions retournent les max et les min de lattitudes et des longitudes
(define (max-lat g)
  (apply max (list-lat g)))

(define (max-lon g)
  (apply max (list-lon g)))

(define (min-lon g)
  (apply min (list-lon g)))

(define (min-lat g)
  (apply min (list-lat g)))

(define minlat (min-lat g))
(define maxlat (max-lat g))
(define minlon (min-lon g))
(define maxlon (max-lon g))


;;convertir les coordonnees en format svr

(define (convert-lat lat)
  (* (/ (- minlat lat) (- minlat maxlat)) coef-lat))


(define (convert-lon lon)
  (* (/ (- minlon lon) (- minlon maxlon)) coef-lon))


;; Creer un cercle a partir d'un vertex
(define circle-ray 5)

(define (create-circle v)
  `(circle ((cx ,(number->string (exact->inexact (convert-lon (vertex-lon v)))))
            (cy ,(number->string (exact->inexact (convert-lat (vertex-lat v)))))
            (r ,(number->string circle-ray)) (stroke "red") (fill "grey"))))

;;(create-circle v1)

;; Creer une ligne entre deux vertex
(define (create-line v w)
  `(line ((x1 ,(number->string (exact->inexact (convert-lon (vertex-lon v)))))
          (y1 ,(number->string (exact->inexact (convert-lat (vertex-lat v)))))
          (x2 ,(number->string (exact->inexact (convert-lon (vertex-lon w)))))
          (y2 ,(number->string (exact->inexact (convert-lat (vertex-lat w)))))
          (stroke "black"))))
;; Creer tous les cercles d'un graphe
(define (graph-circles g)
  (map create-circle (hash-values (graph-vx-ht g))))

;; Retourne une liste des Vertexs voisins d'un vertex passe en parametre
(define (vertex-neighbors g v)
  (map ((curry hash-ref) (graph-vx-ht g)) (vertex-way v)))

;; Creer les lignes entre un vertex et ses voisins
(define (vertex-lines g v)
  (map ((curry create-line) v) (vertex-neighbors g v)))

;; Creer toutes les lignes d'un graphe
(define (graph-lines g)
  (apply append (map ((curry vertex-lines) g) (hash-values (graph-vx-ht g)))))

;; Tous les cercles et toutes les lignes d'un graphe
(define (graph-map g)
  (append (graph-circles g) (graph-lines g)))
#|
(graph-circles g)
(vertex-neighbors g v2)
(vertex-lines g v2)
(graph-lines g)
(graph-map g)|#

;;(graph-map g)
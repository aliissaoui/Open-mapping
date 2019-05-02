#lang racket

(require "hash-graph.rkt")
(require "make_graph.rkt")
(require "voyageur_commerce.rkt")

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

(define extremums (box flattenedFullOsm))

(define maxlat (first extremums))
(define maxlon (second extremums))
(define minlat (third extremums))
(define minlon (fourth extremums))
#|

(define maxlat (max-lat g))
(define maxlon (max-lon g))
(define minlat (min-lat g))
(define minlon (min-lon g))
|#
;;(display extremums)
;;convertir les coordonnees en format svr

(define (convert-lat lat)
  (* (/ (- minlat lat) (- minlat maxlat)) coef-lat))


(define (convert-lon lon)
  (* (/ (- minlon lon) (- minlon maxlon)) coef-lon))


;; Creer un cercle a partir d'un vertex
(define circle-ray 3)

(define (create-circle color ray v)
  `(circle ((cx ,(number->string (exact->inexact (convert-lon (vertex-lon v)))))
            (cy ,(number->string (exact->inexact (convert-lat (vertex-lat v)))))
            (r ,(number->string ray)) (stroke "red") (fill ,color))))

;;(create-circle v1)

;; Creer une ligne entre deux vertex
(define (create-line color v w)
  `(line ((x1 ,(number->string (exact->inexact (convert-lon (vertex-lon v)))))
          (y1 ,(number->string (exact->inexact (convert-lat (vertex-lat v)))))
          (x2 ,(number->string (exact->inexact (convert-lon (vertex-lon w)))))
          (y2 ,(number->string (exact->inexact (convert-lat (vertex-lat w)))))
          (stroke ,color))))

(define (create-itinerary-line color v w)
  `(line ((x1 ,(number->string (exact->inexact (convert-lon (vertex-lon v)))))
          (y1 ,(number->string (exact->inexact (convert-lat (vertex-lat v)))))
          (x2 ,(number->string (exact->inexact (convert-lon (vertex-lon w)))))
          (y2 ,(number->string (exact->inexact (convert-lat (vertex-lat w)))))
          (stroke ,color)
          (stroke-width "5"))))

;;Ecrit un texte dans une position donnée
(define (display-distance msg x1 y1)
  `(text ((x ,(number->string x1))
          (y ,(number->string y1))
          (font-family "Verdana")
          (fill="brown")
          (font-size "20"))
          ,msg))

;; Creer tous les cercles d'un graphe
(define (graph-circles g)
  (map ((curry create-circle) "grey" circle-ray) (hash-values (graph-vx-ht g))))

;; Retourne une liste des Vertexs voisins d'un vertex passe en parametre
(define (vertex-neighbors g v)
  (map ((curry hash-ref) (graph-vx-ht g)) (vertex-way v)))

;; Creer les lignes entre un vertex et ses voisins
(define (vertex-lines g color v )
  (map ((curry create-line) color v) (vertex-neighbors g v)))

;; Creer toutes les lignes d'un graphe
(define (graph-lines g)
  (apply append (map ((curry vertex-lines) g "black") (hash-values (graph-vx-ht g)))))

;; Tous les cercles et toutes les lignes d'un graphe
(define (graph-map g g-not-red)
  (append (graph-circles g) (graph-lines g-not-red)))

;; Les fonctions necessaires a la representation d'un itineraire.
(define (itinerary-circles g list color)
 (map ((curry create-circle) color 10) (map ((curry hash-ref) (graph-vx-ht g)) list)))

(define (itinerary-lines g list color)
  (let ([vx-list (doublet (map ((curry hash-ref) (graph-vx-ht g)) list))])
  (map (lambda (x) (((curry create-itinerary-line) color) (first x) (second x))) vx-list)))


(define (itinerary-map g liste)
  (append (itinerary-lines g liste "green")
  ;;(itinerary-circles g liste "yellow")        
          (list (create-circle "blue" 15 (hash-ref (graph-vx-ht g) (first liste)))
                (create-circle "brown" 15 (hash-ref (graph-vx-ht g) (first (reverse liste)))))))

(define (itinerary-distances g way)
  (map (lambda (x)
         (let ([v (hash-ref (graph-vx-ht g) (car x))])
               (display-distance (real->decimal-string (cdr x) 2)
                                 (- (convert-lon (vertex-lon v)) 20)
                                 (- (convert-lat (vertex-lat v)) 20) ))) way))

;;Representation du chemin minimal
(define (dijkstra-map g way)
  (let* ([ids (map car way)]
        [first (hash-ref (graph-vx-ht g) (first ids))]
        [last (hash-ref (graph-vx-ht g) (last ids))])
    ;;(append (itinerary-circles g ids "brown") (itinerary-lines g ids "blue")
     (append (itinerary-lines g ids "blue")
             (list (create-circle "blue" 15 first)
                   (create-circle "red" 15 last))
             (itinerary-distances g way))))
            ;;(display-distance  "first" (convert-lon (vertex-lon last)) (convert-lat (vertex-lat last))))))

 
;; Representation d'un cycle

(define (cycle-map g liste)
  (let ([ids (greedy g liste)])
  (append (itinerary-circles g ids "yellow") (itinerary-lines g ids "green")
          (itinerary-circles g liste "blue"))))
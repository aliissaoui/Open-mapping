#lang racket
(require racket/trace)
(provide (all-defined-out))
;;Graphe sous la forme d'un table de hashage:

(define coef-lat 1080)
(define coef-lon 1920)

(define ht (make-hash))

(struct vertex (id lat lon way))
(struct graph (vx-ht))

(define v1 (vertex 1 0 3 '(2 3)))
(define v2 (vertex 2 1 1 '(4 5 6 1)))
(define v3 (vertex 3 1 5 '(7 8 1)))
(define v4 (vertex 4 2 0 '(2)))
(define v5 (vertex 5 2 1 '(9 2)))
(define v6 (vertex 6 2 2 '(2)))
(define v7 (vertex 7 2 4 '(3)))
(define v8 (vertex 8 2 6 '(3)))
(define v9 (vertex 9 3 1 '(5)))


(hash-set! ht 1 v1)
(hash-set! ht 2 v2)
(hash-set! ht 3 v3)
(hash-set! ht 4 v4)
(hash-set! ht 5 v5)
(hash-set! ht 6 v6)
(hash-set! ht 7 v7)
(hash-set! ht 8 v8)
(hash-set! ht 9 v9)

(define g (graph ht))
(vertex-way (hash-ref (graph-vx-ht g) 2))
;;(display (graph-vx-ht g))
(define (inverse l)
  (foldl cons '() l))

;; Parcours en profondeur d'un graphe
(define (depth-first-1 g v marked)
  (let ([marked (set-add marked (vertex-id v))])
  (foldl (lambda (actual marked)
    (cond [(not (set-member? marked actual))
               (depth-first-1 g (hash-ref (graph-vx-ht g) actual)
                           marked )]
          [else (inverse (set-add (inverse marked) actual))])) marked (vertex-way v) )))


;;Itineraire entre deux sommets
(define (itinerary g v w mark result)
  (let ([mark (set-add mark (vertex-id v))]
        [result (cons (vertex-id v) result)])
  (foldl (lambda (actual mark)
           (cond  [(equal? (vertex-id v) (vertex-id w)) result] ;; Si on a trouvé le vertex on fait mark = result
                  ;; Si ( (car mark est l'id du vertex recherché c'est fini)
                  [(and (not (null? mark)) (= (car mark) (vertex-id w))) mark]
                  ;; si le vertex n'a jamais était visité on visite ces voisins
                  [(not (set-member? mark actual))
                  (itinerary g (hash-ref (graph-vx-ht g) actual) w
                             mark result)]
                   ;; Sinon on ajoute le vertex actuel à la liste des vertex marqués
                  [else (inverse (set-add (inverse mark) actual))])) mark (vertex-way v) )))

;;(trace depth-first-1)
;;(trace itinerary)
;;(inverse (depth-first-1 g v1 '() ))
;;(inverse (itinerary g v8 v7 '() '()))




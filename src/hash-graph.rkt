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
(define v9 (vertex 9 3 1 '(5 10)))
(define v10 (vertex 10 4 1 '(9)))
(define v11 (vertex 11 5 3 '(12)))
(define v12 (vertex 12 6 7 '(11)))
(hash-set! ht 1 v1)
(hash-set! ht 2 v2)
(hash-set! ht 3 v3)
(hash-set! ht 4 v4)
(hash-set! ht 5 v5)
(hash-set! ht 6 v6)
(hash-set! ht 7 v7)
(hash-set! ht 8 v8)
(hash-set! ht 9 v9)
(hash-set! ht 10 v10)
(hash-set! ht 11 v11)
(hash-set! ht 12 v12)

(define test (graph ht))

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

(define (depth-first g v)
  (depth-first-1 g (hash-ref (graph-vx-ht g) v) '()))

;;Itineraire entre deux sommets
(define (itinerary g v w depth mark result)
  (let ([mark (set-add mark (vertex-id v))]
        [result (cons (vertex-id v) result)])
    #|(display "\nway :")
    (display (vertex-way v))
    (display "\nmark: ")
    (display mark)
    (display "\nresult")
    (display result)|#
    (foldl (lambda (actual mark)
                    ;; Si on a trouvé le vertex on fait mark = result
             (cond  [(equal? (vertex-id v) (vertex-id w)) result]
                    ;; Si pas de chemin
                    [(equal? mark depth) (list (vertex-id w) (vertex-id w))]
                    ;; Si ( (car mark est l'id du vertex recherché c'est fini)
                    [(and (not (null? mark)) (= (car mark) (vertex-id w))) mark]
                    ;; si le vertex n'a jamais était visité on visite ces voisins
                    [(not (set-member? mark actual))
                     (itinerary g (hash-ref (graph-vx-ht g) actual) w depth
                                mark result)]
                    ;; Sinon on ajoute le vertex actuel à la liste des vertex marqués
                    [else (inverse (set-add (inverse mark) actual))])) mark (vertex-way v))))


(define (id-itinerary g v-id w-id )
  (inverse (itinerary g (hash-ref (graph-vx-ht g) v-id)
                        (hash-ref (graph-vx-ht g) w-id)
                        (depth-first g v-id)
                         '() '())))

;;(trace depth-first-1)
;;(trace itinerary)
;;(inverse (depth-first-1 test v1 '() ))
;;(inverse (itinerary test v1 v12 (depth-first test v1) '() '()))


;;Creates a vertex
(define (create-vertex id lat lon way)
  (vertex id lat lon way))

;;Adds a vertex to a graph
(define (add-vertex g v)
  (hash-set! (graph-vx-ht g) (vertex-id v) v))

(add-vertex test v10)
;;(display (graph-vx-ht g))
;;(vertex-way (hash-ref (graph-vx-ht g) 10))


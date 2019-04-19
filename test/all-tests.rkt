#lang racket
(require rackunit)
(require xml)
(require rackunit/text-ui)

(require "../src/make_graph.rkt")
(require "../src/hash-graph.rkt")

(define osm (xml->xexpr (document-element
    (read-xml (open-input-file "../maps/test_map.osm")))))
(define f (flatten osm))
(define g3 (graph (make-graph (list-node f) (list-way f))))

(define ht1 (make-hash))

(define n1 (vertex 1 0 3 '(3 2)))
(define n2 (vertex 2 1 1 '(1 5 4 6)))
(define n3 (vertex 3 1 5 '(1 7 8)))
(define n4 (vertex 4 2 0 '(2)))
(define n5 (vertex 5 2 1 '(2 9)))
(define n6 (vertex 6 2 2 '(2)))
(define n7 (vertex 7 2 4 '(3)))
(define n8 (vertex 8 2 6 '(3)))
(define n9 (vertex 9 3 1 '(5)))
(define n10 (vertex 10 5 1 '(9)))

(hash-set! ht1 1 v1)
(hash-set! ht1 2 v2)
(hash-set! ht1 3 v3)
(hash-set! ht1 4 v4)
(hash-set! ht1 5 v5)
(hash-set! ht1 6 v6)
(hash-set! ht1 7 v7)
(hash-set! ht1 8 v8)
(hash-set! ht1 9 v9)

(define g1 (graph ht1))
;(define reduced-g3 (reduce (graph-vx-ht g3)))
;(graph-vx-ht g3)



(define all-tests
  (test-suite
   "Tests for hash-graph.rkt"
   (test-case
    "test cohérence du graphe donné par la fonction make-graph avec le fichier osm donné"
    (member 3 (vertex-way (hash-ref (graph-vx-ht g3) 7))))
   (test-case
    "test cohérence du graphe donné par la fonction make-graph avec le fichier osm donné"
    (member 2 (vertex-way (hash-ref (graph-vx-ht g3) 5))))
   (test-case
    "test cohérence du graphe donné par la fonction make-graph avec le fichier osm donné"
    (member 9 (vertex-way (hash-ref (graph-vx-ht g3) 5))))
   (test-case
    "test cohérence du graphe donné par la fonction make-graph avec le fichier osm donné"
    (equal? 2 (vertex-lat (hash-ref (graph-vx-ht g3) 5))))))
;(vertex-id (hash-ref (graph-vx-ht g1) 9))
(printf "Running tests\n")
(run-tests all-tests)
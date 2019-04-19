#lang racket
(require xml)
(require "hash-graph.rkt")
(provide (all-defined-out))
(require srfi/1)


;;;; STRUCT

;;;; GRAPH

(define osm (xml->xexpr (document-element
    (read-xml (open-input-file "../maps/projMapping.osm")))))

(define fullosm (xml->xexpr (document-element
    (read-xml (open-input-file "../maps/pentagon.osm")))))

(define flattenedOsm (flatten osm))
(define flattenedFullOsm (flatten fullosm))
(define (node-begin graph)
  (member 'node graph))

(define (tuple-node graph)
  (list (string->number(cadr (member 'id graph))) (string->number(cadr (member 'lat graph))) (string->number(cadr (member 'lon graph))))
 )

(define (list-node graph)
  (let ([m (member 'node graph)])
  (cond
    [(equal? #f  m) '()]
    [else (list* (tuple-node m) (list-node (cdr m))) ]
  )))

(define (tuple-way graph)
  (cond
    [(equal? 'ref (car graph)) (list* (string->number (cadr graph)) (tuple-way (cddddr graph)))]
    [else '()]
   ))

(define (list-way-aux graph)
  (let ([m (member 'way graph)])
  (cond
    [(equal? #f  m) '()]
    [else (list* (tuple-way (member 'ref m)) (list-way-aux (cdr m))) ]
  )))

(define (doublet L)
  (cond
    [(>= (length L) 2) (list* (list (first L) (second L)) (doublet (cdr L))) ]
    [else '()]
   )
 )

(define (list-way graph)
  (let* ([L (list-way-aux graph)]
        [doubled (map doublet L)])
  (foldl append '() doubled)
))

;;add-neignbours --> retourne une liste des voisins selon le lien ids
;;nd a node, ids a list of  ways,
(define (add-neighbours nd list-way)
  (list nd (delete-duplicates (remove* (list (car nd)) (flatten (map (lambda (list) (if (member (car nd) list) list '()))  list-way)))))
)

(define (cr-vertex list)
  (vertex (first (first list)) (second(first list)) (third(first list)) (second list)))

(define (make-graph list-node list-way )
  (define hash_graph (make-hash))
  (map (lambda (list-nd) (hash-set! hash_graph (car list-nd) (cr-vertex ( add-neighbours list-nd list-way)) )) list-node)
  hash_graph
  )



;;;;;;;;;;; MAXLAT - MAXLON

(define (box osm)
  (list (string->number(cadr (member 'maxlat osm)))
        (string->number(cadr (member 'maxlon osm)))
        (string->number(cadr (member 'minlat osm)))
        (string->number(cadr (member 'minlon osm)))
))


;;;;; DISTANCE

(define (distance lat lon) 1) ;; acts as a debug for now


;;;;;; REDUCE

(define (remove_id_neighbour node graph) ;; this function changes neighbours (aka vertex) : A<->B<->C => A<->C
  (let* ([fst_nd_id (first (vertex-way node))] [snd_nd_id (second (vertex-way node))]
         [fst_nd (hash-ref! graph fst_nd_id "In remove_id_neighbour : failed to get fst_nd")]
         [snd_nd (hash-ref! graph snd_nd_id "In remove_id_neighbour : failed to get snd_nd")])
    (hash-set! graph fst_nd_id (vertex (vertex-id fst_nd) (vertex-lat fst_nd) (vertex-lon fst_nd) (remove-duplicates (append (remq (vertex-id node) (vertex-way fst_nd)) (list snd_nd_id) ))))
    (hash-set! graph snd_nd_id (vertex (vertex-id snd_nd) (vertex-lat snd_nd) (vertex-lon snd_nd) (remove-duplicates (append (remq (vertex-id node) (vertex-way snd_nd)) (list fst_nd_id) ))))
    
  ))

(define (reduce_aux node graph) ;; given a node, if degree = 2 :  deletes his id from its neighbours then deletes this node in the graph
  (cond [(equal? 2 (length  (vertex-way node))) (remove_id_neighbour node graph) (hash-remove! graph (vertex-id node))]
 ))

(define (reduce graph) ;; goes through each node to delete nodes of degree 2
  (hash-map graph (lambda vert (reduce_aux (cadr vert) graph)))
  )




;;;;;;;; EXECUTION
(define test (make-graph (list-node flattenedOsm) (list-way flattenedOsm)))
test
(define g2 (graph (make-graph (list-node flattenedOsm) (list-way flattenedOsm))))


(define g3 (graph  (make-graph (list-node flattenedFullOsm) (list-way flattenedFullOsm))))
;full-graph





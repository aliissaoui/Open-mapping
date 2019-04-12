#lang racket
(require xml)

(define osm (xml->xexpr (document-element
    (read-xml (open-input-file "../maps/projMapping.osm")))))

(define graph (flatten osm))

(define (node-begin graph)
  (member 'node graph))

(define (tuple-node graph)
  (list (string->number(caddr graph)) (string->number(cadr (cdddr graph))) (string->number(caddr (cddr (cddr graph)))))
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

(define (list-way graph)
  (let ([m (member 'way graph)])
  (cond
    [(equal? #f  m) '()]
    [else (list* (tuple-way (member 'ref m)) (list-way (cdr m))) ]
  )))

;;add-neignbours --> retourne une liste des voisins selon le lien ids
;;nd a node, ids a list of  ways,
(define (add-neighbours nd list-way)
  (list nd (remove* (list (car nd)) (flatten (map (lambda (list) (if (member (car nd) list) list '()))  list-way))))
)


(define (make-graph list-node list-way )
  (define hash_graph (make-hash))
  (map (lambda (list-nd) (hash-set! hash_graph (car list-nd) (add-neighbours list-nd list-way) )) list-node)
  hash_graph
  )

(define end-graph (make-graph (list-node graph) (list-way graph)))
end-graph


(define (distance lat lon) 1) ;; acts as a debug for now

(define (remove_id_neighbour node graph) ;; this function changes neighbours (aka vertex) : A<->B<->C => A<->C
  (let* ([fst_nd_id (caadr node)] [snd_nd_id (caadr node)]
         [fst_nd (hash-set! graph fst_nd_id "In remove_id_neighbour : failed to get fst_nd")]
         [snd_nd (hash-set! graph snd_nd_id "In remove_id_neighbour : failed to get snd_nd")])
    (hash-set! graph fst_nd_id ((car fst_nd) (append (remove (caar node) (cadr fst_nd)) '(snd_nd_id) )))
    (hash-set! graph snd_nd_id ((car snd_nd) (append (remove (caar node) (cadr snd_nd)) '(fst_nd_id) )))
    
  ))

(define (reduce_aux node graph) ;; given a node, if degree = 2 :  deletes his id from its neighbours then deletes this node in the graph
  (cond [equal? 2 (length (cadr node)) ((remove_id_neighbour node graph) (hash-remove! graph (caar node)))]
  ))

(define (reduce graph) ;; goes through each node to delete nodes of degree 2
  (hash-map graph (lambda hash_node (reduce_aux (cadr hash_node) graph)))
  )












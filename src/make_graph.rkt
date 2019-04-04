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

(define (make-graph list-node list-way)
  (list* (map (lambda (list-nd) (add-neighbours list-nd list-way) ) list-node))  
  )

(define end-graph (make-graph (list-node graph) (list-way graph)))
end-graph
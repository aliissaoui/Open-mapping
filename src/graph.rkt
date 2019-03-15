#lang racket

(require)
(provide (contract-out
 ;; Predicate identifying the vertices (returns true iff the argument
 ;; is a vertex)
 ;; @param  v : any/c
 ;; @return   : boolean?
 [vertex?  (-> any/c boolean?)]

 ;; Predicate identifying the graphs (returns true iff the argument is
 ;; a graph)
 ;; @param  g : any/c
 ;; @return   : boolean?
 [graph? (-> any/c boolean?)]

 ;; Constructor of a vertex
 ;; @param  id : exact-nonnegative-integer?
 ;; @return    : vertex?
 ;;[vertex-new (-> exact-nonnegative-integer? vertex?)]
))

(struct vertex (id coord way))
(struct graph (List-of-vertex)) 


(define v (vertex 2 '(3.2 4.7) '(5 6)))
(define w (vertex 3 '(2.0 8.5) '(2 6)))
(define g (graph '(v w)))
(vertex? v)
(vertex-id v)
(vertex-id w)
(graph? g)


;;fichier graph : -> graph? vertex?
;;contract : -> graph? vertex? Mais avec des contrats 
;;fichier client : require graph

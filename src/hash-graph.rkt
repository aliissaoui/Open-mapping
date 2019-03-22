#lang racket
(require racket/trace)
;;Graphe sous la forme d'un table de hashage:


(define ht (make-hash))

(struct vertex (id lat lon way))
(struct graph (vx-ht)) 

(define v1 (vertex 1 3.2 4.7 '(2 3)))
(define v2 (vertex 2 2.0 8.5 '(4 5 6)))
(define v3 (vertex 3 1.5 9.1 '(7 8)))
(define v4 (vertex 4 1.0 0.5 '()))
(define v5 (vertex 5 5.2 8.4 '(9)))
(define v6 (vertex 6 4.1 9.8 '()))
(define v7 (vertex 7 14.0 2.4 '()))
(define v8 (vertex 8 12.4 7.9 '()))
(define v9 (vertex 9 13.8 2.1 '()))


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

;;(define (depth-first g v marked)
 ;; (display (vertex-id v))
  ;;(display "-")
  ;(for ([actual (vertex-way v)])
  ;  (cond [(not (set-member? marked actual))
  ;             (depth-first g (hash-ref (graph-vx-ht g) actual)
   ;                         (set-add marked (vertex-id v)))]
    ;      [else marked])))

(define (depth-first g v marked)
  (display (vertex-id v))
  (display "-")
  (foldl (lambda (actual marked)
    (cond [(not (set-member? marked actual))
               (depth-first g (hash-ref (graph-vx-ht g) actual)
                            (set-add marked (vertex-id v)))]
          [else (set-add marked (vertex-id actual))])) marked (vertex-way v) )  )

;;(trace depth-first)
(depth-first g v1 '())
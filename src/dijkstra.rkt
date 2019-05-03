#lang racket
(require racket/trace)
(require "hash-graph.rkt")
(require "make_graph.rkt")
(require "haversine.rkt")
(provide (all-defined-out))

(define (initialize g start)
  (define h (make-hash))
  (map (lambda (x) (hash-set! h (car x) (cons (cdr x) (car x)))) 
  (let ([keys (hash-keys (graph-vx-ht g))]
        [inf 10000])
    (map (lambda (x)
           (if (= start x)
               (cons x 0)
               (cons x inf)))
           keys)))
  h)

(define (member? l x)
  (cond [(null? l) #f]
        [(= x (car l)) #t]
        [else (member? (cdr l) x)]))

(define (find-min distances Q)
  (let* ([d-list1 (hash->list distances)]
         [d-list (filter
                  (lambda (x) (member? Q (car x))) d-list1)] 
         [min-distances (apply min (map cadr d-list))])
    (first (filter (lambda (x) (= min-distances (cadr x))) d-list))))



;;dis is a hash table : (key : distance)
(define (change-dis-pred distances w dis pred)
  ;;(display distances)
  (hash-remove! distances w)
  (hash-set! distances w (cons dis pred)))


(define (maj-distances g v w distances)
  ;;;(display distances)
  (let* ([poids (haversine (hash-ref (graph-vx-ht g) v) (hash-ref (graph-vx-ht g) w))]
         [d1 (car (hash-ref distances v))]
         [d2 (car (hash-ref distances w))])
    (cond [(>= d2 (+ d1 poids)) (change-dis-pred distances w (+ d1 poids) v)]
          [else distances]))
  distances) 


(define (maj-vertex g v q)
  (last (map (lambda (x) (maj-distances g v x q)) (vertex-way (hash-ref (graph-vx-ht g) v)))))


(define (Dijkstra-loop g start distances Q)
  (if (null? Q)
      distances
      (let ([min (car (find-min distances Q))])
        (maj-vertex g min distances)
        (Dijkstra-loop g min distances (remove min Q)))))

(define (Dijkstra g start Q)
  (let ([start-vx (hash-ref (graph-vx-ht g) start)])
    (Dijkstra-loop g (vertex-id start-vx) (initialize g (vertex-id start-vx)) Q)))

;;(Dijkstra g 392014874)
(define (find-way dis v w)
  (if (= v w)
      (cons (cons v (car (hash-ref dis w))) '())
      (let ([w2 (hash-ref dis w)])
        (cons (cons w (car w2)) (find-way dis v (cdr w2))))))

(define (dijkstra-way g v w)
  (let ([Q (depth-first-1 g (hash-ref (graph-vx-ht g) v) '())])
    (if (not (member? Q w))
        (list w w)
        (reverse (find-way (Dijkstra g v Q) v w)))))
#lang racket
(require xml)

(define osm (xml->xexpr (document-element
    (read-xml (open-input-file "../maps/projMapping.osm")))))
;(define (delete-vide-string liste)
;   (cdr (cons list)))
;(car (car (cdr (cddddr osm))))
(define (extract-champs f l type)
  (cond
    [(null? l) f]
    [(and (list? (car l)) (equal? (caar l) type))
                                                  (extract-champs  (cons (car l) f) (cdr l) type)]
    [else (extract-champs f (cdr l) type) ]))
;
;(define (delete-unuseless f l)
;  (cond
;    [(null? l) f]
;    [(string? (car l))]
;    [(list? (car l))
;     ((cons (delete-unuseless (car l) (delete-unuseless (cdr l) f))))]
;    [else (delete-unuseless f (cdr l))]))

(define node-list (extract-champs '() osm 'node))
(define way-list (extract-champs '() osm 'way))
(define way-lists (extract-champs '() way-list 'id))
;;(delete-unuseless way-list '())
way-list
;(caadar way-list);;id of the way
(define a (cdaadr (cadddr (car way-list))));;id du premièr node  
(define b (cdaadr (cadr (cddddr (car way-list)))));;id du dexième node 
;;(cadddr (cddddr (car way-list)))
(define way
         '((way ((id "199797372")) "\n    " (nd ((ref "2097959544"))) "\n    " (nd ((ref "515330686"))) "\n    " (tag ((k "highway") (v "tertiary"))) "\n  ")
           (way ((id "111")) "\n    " (nd ((ref "222"))) "\n    " (nd ((ref "11"))) "\n    " (tag ((k "highway") (v "tertiary"))) "\n  ")
))
(list a b);;la liste des deux nodes 
(caadr (list a b));;reacceder au premier node 
(define (id-filter-way l f)
  (if (null? l)
      f
      (id-filter-way (cdr l) (cons (list (cdaadr (cadddr (car l))) (cdaadr (cadr (cddddr (car l))))) f) )))
 (id-filter-way way-list '())
(id-filter-way way '())
(define (is-id? id  l)
  (equal? (car id) (car (cdaadr l))))

(define (add-neighbours l id f)
  (cond
    [(null? l) f]
    [(is-id? (car id) (car l)) (list (add-neighbours (cdr l) id (cons (car l) (cadr id))) f)]
    [(is-id? (cadr id) (car l)) (list (add-neighbours (cdr l) id (cons (car l) (car id))) f)]
    [else (add-neighbours (cdr l) id (list (car l) f))]
    )
  )
(add-neighbours node-list '(("2097959544") ("515330686")) '())
  
(define (construct-graphe file)
  (define way-list (extract-champs '() file 'way))
  (define node-list (extract-champs '() file 'node))
  (define neighbours-list (id-filter-way way-list '()))
  (add-neighbours node-list (car neighbours-list) '())
 )
(construct-graphe osm)    
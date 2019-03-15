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

(define node-list (extract-champs '() osm 'node))
(define way-list (extract-champs '() osm 'way))


      
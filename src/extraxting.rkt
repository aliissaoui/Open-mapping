#lang racket
(require xml)

(define osm (xml->xexpr (document-element
    (read-xml (open-input-file "../maps/projMapping.osm")))))

;;extraire la liste des champs de la liste l de type 'type ('node)
(define (extract-champs f l type)
  (cond
    [(null? l) f]
    [(and (list? (car l)) (equal? (caar l) type))
                                                  (extract-champs  (cons (car l) f) (cdr l) type)]
    [else (extract-champs f (cdr l) type) ]))

;(define (delete-unuseless f l)
;  (cond
;    [(null? l) f]
;    [(string? (car l))]
;    [(list? (car l))
;     ((cons (delete-unuseless (car l) (delete-unuseless (cdr l) f))))]
;    [else (delete-unuseless f (cdr l))]))


;;id-filter-way -> liste des id des nodes qui sont liés 
(define (id-filter-way l f)
  (if (null? l)
      f
      (id-filter-way (cdr l) (cons (list (cdaadr (cadddr (car l))) (cdaadr (cadr (cddddr (car l))))) f) )))



;; is-id? -> vrai si id est l'id du node l
(define (is-id? id  l)
  (equal? (car id) (car (cdaadr l))))

;;add-neignbours --> retourne une liste des voisins selon le lien ids 
(define (add-neighbours l ids f)
  (cond
    [(null? l) f]
    [(is-id? (car ids) (car l)) (list (add-neighbours (cdr l) ids (cons (car l) (cadr ids))) f)]
    [(is-id? (cadr ids) (car l)) (list (add-neighbours (cdr l) ids (cons (car l) (car ids))) f)]
    [else (add-neighbours (cdr l) ids (list (car l) f))]
    )
  )
;;construct-graphe construction du graphe à partir du fichier osm
(define (construct-graphe file)
  (define way-list (extract-champs '() file 'way))
  (define node-list (extract-champs '() file 'node))
  (define neighbours-list (id-filter-way way-list '()))
  (add-neighbours node-list (car neighbours-list) '())
 )


;;tests
(define node-list (extract-champs '() osm 'node))
(define way-list (extract-champs '() osm 'way))
(define way-lists (extract-champs '() way-list 'id))
(define a (cdaadr (cadddr (car way-list))));;id du premièr node  
(define b (cdaadr (cadr (cddddr (car way-list)))));;id du dexième node 
(define way
         '((way ((id "199797372")) "\n    " (nd ((ref "2097959544"))) "\n    " (nd ((ref "515330686"))) "\n    " (tag ((k "highway") (v "tertiary"))) "\n  ")
           (way ((id "111")) "\n    " (nd ((ref "222"))) "\n    " (nd ((ref "11"))) "\n    " (tag ((k "highway") (v "tertiary"))) "\n  ")
))
(id-filter-way way-list '())
(id-filter-way way '())
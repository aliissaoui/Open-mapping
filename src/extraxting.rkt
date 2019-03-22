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


;;id-filter-way -> liste des id des nodes qui sont liés 
(define (id-filter-way l f)
  (if (null? l)
      f
      (id-filter-way (cdr l) (cons (list (string->number (car (cdaadr (cadddr (car l))))) (string->number (car (cdaadr (cadr (cddddr (car l))))))) f) )))



;; is-id? -> vrai si id est l'id du node l
(define (is-id? id  l)
  (equal? (car id) (car l)))

;;add-neignbours --> retourne une liste des voisins selon le lien ids
;;fonction à reparer
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
  (define node-list (parse-node (extract-champs '() file 'node)))
  (define neighbours-list (id-filter-way way-list '()))
  (add-neighbours node-list neighbours-list '())
 )
;;aramettres ; cdr d un champs node -> list des 3 champs 
(define (parse1-node node)
  (if (null? node)
      '()
      (cons (string->number (cadar node)) (parse1-node (cdr node)))))
(define (parse-node nodes)
  (cond
    [(null? nodes) '()]
    [(list? (car nodes)) (cons (parse1-node (cadar nodes)) (parse-node (cdr nodes)))]
    [else (cons (cons (parse-node (cdar nodes))) (parse1-node (cdr nodes)))]))
(define node-list (extract-champs '() osm 'node))

(parse-node node-list)
;(parse1-node '((id "515330686") (lat "44.7754213") (lon "-0.8587464")))
;;tests
;(define node-list (extract-champs '() osm 'node))
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

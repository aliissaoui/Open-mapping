#lang racket

(require xml)
(require web-server/servlet
         web-server/servlet-env)

(require "hash-graph.rkt")
(require "graph-map.rkt")
(require "make_graph.rkt")
(require "dijkstra.rkt")

;; Showing only the map

(define (main-page req)
  (response/xexpr
   `(html
     (head (title "NORMAL MAP"))
     (body
      (svg
       ((viewBox "0 0 1920 1080"))
       .,(append (graph-map g g2)
                 `((text ((x ,"100")
                                    (y ,"50")
                                    (font-family "Verdana")
                                    (fill="red")
                                    (font-size "50"))
                                   "Welcome to Open Mapping Service"))))))))

;;Showing the itinerary between two vertexes given in the url 

(define (route req)
  (if (null? (request-bindings req))
      (response/xexpr
       `(html (head (title "NO IDs"))
              (body
               (h1 ," Error666: Please enter start and end ids ")
               (pre ,(format "~a" " Use : http://localhost:9000/route?start=<id>&end=<id>")))))
      
      (let* ([start (string->number (extract-binding/single 'start (request-bindings req)))]
            [end   (string->number (extract-binding/single 'end (request-bindings req)))]
            [itinerary (id-itinerary g2 start end)])
        (display itinerary)
        (if (not (and (= (length itinerary) 2) (= (first itinerary) (second itinerary))))
            (response/xexpr
             `(html
               (head (title "ITINERARY MAP"))
               (body
                (svg
                 ((viewBox "0 0 1920 1000"))
                 .,(append (graph-map g g2)
                           (itinerary-map g2 itinerary)
                           `((text ((x ,"100")
                                    (y ,"50")
                                    (font-family "Verdana")
                                    (fill="red")
                                    (font-size "50"))
                                   "Itinerary by Open Mapping Service")))))))
            (response/xexpr
             `(html (head (title " Disconnected Universe"))
                    (body
                     (h1 ," Error777:  Disconnected Universe Error ")
                     (pre ,(format "~a" " Use connected ids")))))))))
;;affiche l'itineraire fournie par dijkstra 
(define (distance req)
  (if (null? (request-bindings req))
      (response/xexpr
       `(html (head (title "NO IDs"))
              (body
               (h1 ," Error666: Please enter start and end ids ")
               (pre ,(format "~a" " Use : http://localhost:9000/distance?start=<id>&end=<id>")))))
      
      (let* ([start (string->number (extract-binding/single 'start (request-bindings req)))]
             [end   (string->number (extract-binding/single 'end (request-bindings req)))]
             [way (dijkstra-way g start end)])
        (if (not (and (= (length way) 2) (= (first way) (second way))))
            (response/xexpr
             `(html
               (head (title "OPTIMAL ITINERARY"))
               (body
                (svg
                 ((viewBox "0 0 1920 1000"))
                 .,(append (graph-map g g2)
                           (dijkstra-map g2 way)
                           `((text ((x ,"100")
                                    (y ,"50")
                                    (font-family "Verdana")
                                    (fill="red")
                                    (font-size "50"))
                                   "Optimal Itinerary by Open Mapping Service")))))))
            (response/xexpr
             `(html (head (title " Disconnected Universe"))
                    (body
                     (h1 ," Error777:  Disconnected Universe Error ")
                     (pre ,(format "~a" " Use connected ids")))))))))
  

;;Showing the cycle between a liste of nodes given in the url

(define (cycle req)
  (let ([nodes (map string->number (string-split
                (extract-binding/single 'nodes (request-bindings req)) ","))])
  (response/xexpr
   `(html
     (head (title "CYCLE MAP"))
     (body
      (svg
       ((viewBox "0 0 1920 1080"))
       .,(append (graph-map g) (cycle-map g nodes))))))))


;; An example of a page returning HTML with xexprs and macros
(define (display-page req)
  (response/xexpr
   `(html (head (title "OPEN MAPPING SERVICE DISPLAY PAGE"))
             (body
              (h1 ,(url->string (request-uri req)))
              (pre ,(format "~a" (request-bindings req)))))))


;; Routing function
;;     /display          --->   display-page
;;     everything else   --->   main-page

(define-values (server-dispatch server-url)
  (dispatch-rules
     [("display") display-page]
     [("route") route]
     [("distance") distance]
     [("cycle") cycle]
     [else main-page]))


(serve/servlet server-dispatch
               #:servlet-regexp #rx""
               #:port 9000
               #:launch-browser? #f)


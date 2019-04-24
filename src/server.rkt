#lang racket

(require xml)
(require web-server/servlet
         web-server/servlet-env)

(require "hash-graph.rkt")
(require "graph-map.rkt")
(require "make_graph.rkt")

;; Showing only the map

(define (main-page req)
  (response/xexpr
   `(html
     (head (title "NORMAL MAP"))
     (body
      (svg
       ((viewBox "0 0 1920 1000"))
       .,(graph-map g))))))

;;Showing the itinerary between two vertexes given in the url 

(define (route req)
  (let ([start (string->number (extract-binding/single 'start (request-bindings req)))]
        [end   (string->number (extract-binding/single 'end (request-bindings req)))])
  (response/xexpr
   `(html
     (head (title "ITINERARY"))
     (body
      (svg
       ((viewBox "0 0 1920 1000"))
       .,(append (graph-map g)
                 (itinerary-map g (id-itinerary g start end)))))))))


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
     [else main-page]))


(serve/servlet server-dispatch
               #:servlet-regexp #rx""
               #:port 9000
               #:launch-browser? #f)


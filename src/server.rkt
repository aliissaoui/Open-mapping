#lang racket

(require xml)
(require web-server/servlet
         web-server/servlet-env)
;;(require "hash-graph.rkt")
(require "graph-map.rkt")
(require "make_graph.rkt")

;; An example of a page returning TEXT with calls to fprintf
(define (main-page req)
  (response/xexpr
   `(html
     (head (title "TEST OPEN MAPPING"))
     (body
      (svg
       ((viewBox "0 0 1920 1000"))
       ;;.,(graph-map g)
       .,(graph-map g3)
       )))))


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
     [else main-page]))

(serve/servlet server-dispatch
               #:servlet-regexp #rx""
               #:port 9000
               #:launch-browser? #f)


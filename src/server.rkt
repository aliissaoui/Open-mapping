#lang racket

(require xml)
(require web-server/servlet
         web-server/servlet-env)

;; An example of a page returning TEXT with calls to fprintf
(define (main-page req)
  (response/output
    #:mime-type TEXT/HTML-MIME-TYPE
    (lambda (out)
      (fprintf out "HELLO FROM THE OPEN MAPPING SERVICE~n")
      )))

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
#lang racket

(require web-server/servlet
         web-server/servlet-env
         web-server/dispatch
         json
         linkeddata/json-ld
         linkeddata/rdf
         datalog)

(define knowledge (make-theory))

(define (response/string
         #:code [code 200]
         #:body [body ""])
  (response/full
   code
   (string->bytes/utf-8 "Yo")
   (current-seconds)
   (string->bytes/utf-8 "text/plain")
   empty
   (list (string->bytes/utf-8 body))))

(define (index req)
  (response/string #:body (format "~a" knowledge)))

(define (hello req name)
  (response/string
   #:body (string-append "Hello " name ".")))

(define (data->rdf data)
  (flatten
   (hash-values
    (json-ld->rdf (expand-jsonld (bytes->jsexpr data))))))

(define a-triple
  (triple "http://example.com/test#example1"
          "http://example.com/vocab#embed"
          "http://example.com/vocab#example2"))

(define-syntax-rule (knowledge-add-triple! theory t)
  (datalog theory (! (triple
                      (triple-subject t)
                      (triple-predicate t)
                      (triple-object t)))))

(define (post! req)
  (let* ([data (request-post-data/raw req)]
         [rdf (data->rdf data)])
    (for ([triple rdf])
      (knowledge-add-triple! knowledge triple))
    (response/string #:body "Ok")))

(define-values (go _)
    (dispatch-rules
     [("") index]
     [("post") #:method "post" post!]
     [ ("hello" (string-arg)) hello]
     [else index]))


(serve/servlet go
               #:servlet-regexp #rx""
               #:launch-browser? #f
               )

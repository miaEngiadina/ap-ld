;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
#lang racket

(require web-server/servlet
         web-server/servlet-env
         web-server/dispatch
         net/url
         json
         linkeddata/json-ld
         linkeddata/rdf
         datalog)

;; Database
;; ========

;; The database is a datalog theory.
(define database
  (make-parameter (make-theory)))

;; Add a triple to the database.
;; TODO: this is my first syntax-rule. Read more of "Fear of Macros" (http://www.greghendershott.com/fear-of-macros/index.html) and make this nicer.
(define-syntax-rule (database-add-triple! t)
  (datalog (database) (! (triple
                      (triple-subject t)
                      (triple-predicate t)
                      (triple-object t)))))

;; TODO: just have a macro that adds multiple triples at the same time.
(define (database-add-triples! ts)
  (for ([t ts])
    (database-add-triple! t)))

;; Parse data to RDF triples
(define (data->triples data)
  (flatten
   (hash-values
    (json-ld->rdf (expand-jsonld (bytes->jsexpr data))))))

;; get all triples with subject id and format as jsexpr
(define (database-get-subject id)
  (define (query->hash q)
    (foldl (lambda (triple-as-hash h)
             (let ([property (hash-ref triple-as-hash 'P)]
                   [object (hash-ref triple-as-hash 'O)])
               (hash-set h
                         property
                         (cond [(literal? object)
                                ;; if literal pack in @value
                                (hasheq '@value (literal-lexical-form object))]
                               ;; if not literal pack in @id
                               ;; [else (hasheq '@id object)]
                               ;; TODO but that causes compact-jsonld to fail. Figure out why.
                               [else object]
                               ))))
           (hasheq)
           q))
  (let [(query-result
         (datalog (database) (? (triple id P O))))]
    (hash-set
     (query->hash query-result)
     '@id id)))


;; Debugging helpers
;; =================

;; Just an example triple
(define a-triple
  (triple "http://example.com/test#example1"
          "http://example.com/vocab#embed"
          "http://example.com/vocab#example2"))


;; Load a simple create action
(expand-jsonld
 (call-with-input-file "a-create.jsonld" read-json))

;; I think the json-ld->rdf does not work properly, or at least not as I expect
(json-ld->rdf
 (expand-jsonld
  (call-with-input-file "a-create.jsonld" read-json)))

;; Query for all triples in the database
(datalog (database)
         (? (triple S P O)))

;; Web Server
;; ==========

;; Set up routes
(define-values (dispatch url)
  (dispatch-rules
   ;; The actor profile
   [("") get-actor-profile]

   ;; get inbox
   [("inbox") #:method "get" get-inbox]

   ;; post to inbox (server-to-server)
   [("inbox") #:method "post" post-inbox]

   ;; post to outbox
   [("outbox") #:method "post" post-outbox]

   ;; get outbox (rest of world)
   [("outbox") #:method "get" get-inbox]

   ;; DEBUG: any JSON-LD posted here will be added to the database
   [("post") #:method "post" post!]

   [else error-404]))

(define base-url
  (make-parameter (string->url "http://localhost:8000/")))

(define (absolute-url handler)
  (combine-url/relative (base-url) (url handler)))

;; Respond with a plain text string
(define (response/string
         #:code [code 200]
         #:body [body ""]
         )
  (response/full
   code
   (string->bytes/utf-8 "Yo")
   (current-seconds)
   (string->bytes/utf-8 "text/plain")
   empty
   (list (string->bytes/utf-8 body))))

;; Respond with JSON-LD
(define (response/jsonld
         #:code [code 200]
         #:body [body (hash)]
         )
  (response/full
   code
   (string->bytes/utf-8 "Yo")
   (current-seconds)
   ;; well, really with activity+json mime type
   (string->bytes/utf-8 "application/activity+json")
   empty
   (list (jsexpr->bytes body))))

(define (error-404 req)
  (response/string #:code 404 #:body "Not found"))

;; Get actor profile from DB and return
;; TODO: compacting does not yet work as I would like it to.
(define (get-actor-profile req)
  (let [(actor-profile
         (jsexpr->string
          (compact-jsonld
           (database-get-subject "http://localhost:8000/") as:context)))]
    (response/jsonld #:body actor-profile)))

;; TODO
(define (get-inbox req)
  (response/string #:body "Hello"))

(define (post-inbox req)
  (response/string #:body "Hello"))

(define (get-outbox req)
  (response/string #:body "Hello"))

;; This is where a user posts an activity to be forwarded to other inboxes
;; TODO Create a new Datalog theory with the request content and validate by running datalog queries.
(define (post-outbox req)
  (let ([data (request-post-data/raw req)])
    (database-add-triples! (data->triples data))
    (display (data->triples data))
    (response/string #:body "Ok")))

;; Hiii!
(define (hello req name)
  (response/string
   #:body (string-append "Hello " name ".")))

;; Just add some stuff to the DB
(define (post! req)
  (let ([data (request-post-data/raw req)])
    (database-add-triples! (data->triples data))
    (response/string #:body "Ok")))


;; ActivityPub
;; ===========

;; ActivityStreams context
(define as:context
  (hash-ref
   (call-with-input-file "as.jsonld" read-json) '@context))

;; An actor, the actor
(define the-actor
  (let ([id (url->string (base-url))]
        [name "The Actor"])
    (list
     (triple id
             'https://www.w3.org/ns/activitystreams#inbox
             (url->string (absolute-url get-inbox)))
     (triple id
             '@type
             "https://www.w3.org/ns/activitystream#Person")
     (triple id
             'https://www.w3.org/ns/activitystreams#name
             (literal name #f #f)))))


;; Do it!
;; ======
(define (main)
  ;; Add the actor to the db
  (database-add-triples! the-actor)

  ;; Start web server
  (serve/servlet dispatch
                 #:servlet-regexp #rx""
                 #:launch-browser? #f
                 ))

(main)

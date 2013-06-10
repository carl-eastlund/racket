#lang racket

(require racket/generic
         racket/private/generic
         rackunit)

;; This tests PR 13737 (keyword arguments and #:defaults did
;; not work together)

(define-generics thing
  (foo thing #:stuff other)
  #:defaults
  {[number?
    (define (foo thing #:stuff other) (+ thing other))]})

(check-equal? (foo 1 #:stuff 2) 3)

;; This tests that the keyword & defaults issue doesn't occur for
;; forged generics either

(define-primitive-generics
  #:define-generic gen:foo
  #:define-predicate foo?
  #:define-property prop:foo
  #:define-accessor foo-accessor
  #:define-supported dummy
  #:define-methods [(meth foo #:kw kw)]
  #:given-self foo
  #:given-extensions ()
  #:given-defaults ([number? (define (meth foo #:kw kw) kw)])
  #:given-fallbacks ())

(check-equal? (meth 3 #:kw 5) 5)


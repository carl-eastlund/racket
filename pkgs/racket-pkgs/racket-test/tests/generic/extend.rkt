#lang racket/base

(require racket/generic racket/stream rackunit)

;; Testing inheritance from a "built-in" generic:

(define-generics reversible-stream
  #:extend gen:stream
  (stream-reverse reversible-stream))

(struct singleton-stream [value]
  #:methods gen:reversible-stream
  [(define (stream-empty? x) #f)
   (define (stream-first x) (singleton-stream-value x))
   (define (stream-rest x) empty-stream)
   (define (stream-reverse x) x)])

(check-equal? (reversible-stream? (singleton-stream 5)) #true)
(check-equal? (stream? (singleton-stream 5)) #true)
(check-equal? (stream->list (singleton-stream 5)) '(5))
(check-equal? (stream->list (stream-reverse (singleton-stream 5))) '(5))

(struct computed-stream [length proc]
  #:methods gen:reversible-stream
  [(define (stream-empty? x)
     (zero? (computed-stream-length x)))
   (define (stream-first x)
     ((computed-stream-proc x) (computed-stream-length x)))
   (define (stream-rest x)
     (computed-stream (sub1 (computed-stream-length x))
                      (computed-stream-proc x)))
   (define (stream-reverse x)
     (define len (computed-stream-length x))
     (define proc (computed-stream-proc x))
     (computed-stream len (lambda (x) (proc (- (add1 len) x)))))])

(check-equal? (reversible-stream? (computed-stream 5 number->string)) #true)
(check-equal? (stream? (computed-stream 5 number->string)) #true)
(check-equal? (stream->list (computed-stream 5 number->string))
              (list "5" "4" "3" "2" "1"))
(check-equal? (stream->list (stream-reverse (computed-stream 5 number->string)))
              (list "1" "2" "3" "4" "5"))

;; Testing multiple-inheritance and define/generic:

(define-generics listable
  (to-list listable))

(define-generics vectorable
  (to-vector vectorable))

(define-generics streamable
  #:extend [gen:listable gen:vectorable]
  (to-stream streamable . others))

(struct listish [contents]
  #:methods gen:listable
  [(define (to-list x) (listish-contents x))])

(struct vectorish [contents]
  #:methods gen:vectorable
  [(define (to-vector x) (vectorish-contents x))])

(struct streamish [contents]
  #:methods gen:streamable
  [(define/generic 2lst to-list)
   (define/generic 2vec to-vector)
   (define/generic 2str to-stream)
   (define (to-list x) (stream->list (streamish-contents x)))
   (define (to-vector x) (list->vector (to-list x)))
   (define (to-stream x . others)
     (apply stream-append
            (streamish-contents x)
            (for/list ([other (in-list others)])
              (cond
                [(streamable? other) (2str other)]
                [(listable? other) (2lst other)]
                [(vectorable? other) (vector->list (2vec other))]))))])

(check-equal? (streamable? (streamish (stream 1 2 3))) #true)
(check-equal? (vectorable? (streamish (stream 1 2 3))) #true)
(check-equal? (listable? (streamish (stream 1 2 3))) #true)
(check-equal? (to-list (streamish (stream 1 2 3))) (list 1 2 3))
(check-equal? (to-vector (streamish (stream 1 2 3))) (vector 1 2 3))
(check-equal? (stream->list (to-stream (streamish (stream 1 2))
                                       (listish (list 3 4))
                                       (vectorish (vector 5 6))
                                       (streamish (stream 7 8))))
              (list 1 2 3 4 5 6 7 8))

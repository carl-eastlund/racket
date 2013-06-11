
(load-relative "loadtest.rktl")

(Section 'dict)

(require scheme/dict)

;; Currently relying on the `map' an `for-each' to test `dict-iterate-...',
;; and custom hashes to test `prop:dict' use.

(define (try-simple d mutable? can-remove? can-update? [orig-one 1])
  (test #t dict? d)

  (test 'one dict-ref d 1)
  (test #t dict-has-key? d 1)
  (test 'nope dict-ref d 100 'nope)
  (test #f dict-has-key? d 100)
  (test 'nope dict-ref d 100 (lambda () 'nope))
  
  (test #t ormap values (dict-map d (lambda (k v) (equal? k orig-one))))
  (test #t ormap values (dict-map d (lambda (k v) (equal? v 'one))))
  (test #f ormap values (dict-map d (lambda (k v) (equal? k 100))))

  (test mutable? dict-mutable? d)
  (test can-remove? dict-can-remove-keys? d)
  (test can-update? dict-can-functional-set? d)

  (test (dict-map d cons) 'dict->list (dict->list d))
  (test (dict-map d (λ (k v) k)) 'dict-keys (dict-keys d))
  (test (dict-map d (λ (k v) v)) 'dict-values (dict-values d))
  
  (test (dict-map d cons) 'in-dict
        (for/list ([(k v) (in-dict d)])
          (cons k v)))
  (test (dict-map d cons) 'in-dict/keys/vals
        (for/list ([k (in-dict-keys d)]
                   [v (in-dict-values d)])
          (cons k v)))
  (test (dict-map d cons) 'in-dict-pairs
        (for/list ([p (in-dict-pairs d)])
          p))
  
  (let ([l null])
    (dict-for-each d (lambda (k v) (set! l (cons (cons k v) l))))
    (test (reverse l) dict-map d cons)
    (test (length l) dict-count d))

  (if (not can-remove?)
      (begin
        (err/rt-test (dict-remove! d 1))
        (err/rt-test (dict-remove d 1))
        (err/rt-test (dict-set d 1 "ONE"))
        (test (void) dict-set! d 1 "ONE")
        (test "ONE" dict-ref d 1)
        (test (void) dict-set*! d 1 (gensym) 1 "TWO")
        (err/rt-test (dict-set*! d 1) exn:fail?)
        (test "TWO" dict-ref d 1)
        (test "TWO" dict-ref! d 1 (gensym)))
      (let ([cnt (dict-count d)]
            [smaller (if mutable?
                         (begin
                           (err/rt-test (dict-remove d 1))
                           (dict-remove! d 1)
                           d)
                         (begin
                           (err/rt-test (dict-remove! d 1))
                           (dict-remove d 1)))])
        (test 'nope dict-ref smaller 1 'nope)
        (test (sub1 cnt) dict-count smaller)
        (let ([try-add
               (lambda (d val)
                 (let ([bigger (if mutable?
                                   (begin
                                     (err/rt-test (dict-set smaller 1 val))
                                     (dict-set! smaller 1 val)
                                     d)
                                   (begin
                                     (err/rt-test (dict-set! smaller 1 val))
                                     (dict-set smaller 1 val)))])
                   (test cnt dict-count bigger)
                   (when (eq? val 'one)
                     (unless (pair? d)
                       (test #t equal? d bigger)))))])
          (try-add smaller "ONE")
          (try-add d "ONE")
          (try-add d 'one))
        (let ([try-add
               (lambda (d val)
                 (let ([bigger (if mutable?
                                   (begin
                                     (err/rt-test (dict-set* smaller 1 val))
                                     (dict-set*! smaller 1 (gensym) 1 val)
                                     (err/rt-test (dict-set*! smaller 1) exn:fail?)
                                     d)
                                   (begin
                                     (err/rt-test (dict-set*! smaller 1 val))
                                     (err/rt-test (dict-set* smaller 1) exn:fail?)
                                     (dict-set* smaller 1 (gensym) 1 val)))])
                   (test cnt dict-count bigger)
                   (when (eq? val 'one)
                     (unless (pair? d)
                       (test #t equal? d bigger)))))])
          (try-add smaller "ONE")
          (try-add d "ONE")
          (try-add d 'one)))))

(try-simple (vector 'zero 'one 'two) #t #f #f)
(try-simple #hash((1 . one) (#f . 7)) #f #t #t)

(let ([d (make-hasheq '((1 . one) (#f . 7)))])
  (test 'one dict-ref! d 1 (gensym))
  (test 'two dict-ref! d 2 'two)
  (test 'two dict-ref d 2)
  (test 'three dict-ref! d 3 (λ () 'three))
  (test 'three dict-ref d 3))

(try-simple #hasheq((1 . one) (#f . 7)) #f #t #t)
(try-simple (hash-copy #hash((1 . one) (#f . 7))) #t #t #f)
(try-simple (hash-copy #hasheq((1 . one) (#f . 7))) #t #t #f)
(try-simple '((0 . zero) (1 . one)) #f #t #t)
(try-simple '((1 . one) (0 . zero)) #f #t #t)
(try-simple (let ([h (make-custom-hash (lambda (a b)
                                         (string=? (format "~a" a)
                                                   (format "~a" b)))
                                       (lambda (a)
                                         (equal-hash-code (format "~a" a))))])
              (dict-set! h "1" 'one)
              (dict-set! h "2" 'two)
              h)
            #t #t #f
            "1")
(try-simple (let* ([h (make-immutable-custom-hash 
                       (lambda (a b)
                         (string=? (format "~a" a)
                                   (format "~a" b)))
                       (lambda (a)
                         (equal-hash-code (format "~a" a))))]
                   [h (dict-set h "1" 'one)]
                   [h (dict-set h "2" 'two)])
              h)
            #f #t #t
            "1")
(let ([s1 (make-string 1 #\1)]
      [s2 (make-string 1 #\2)])
  (try-simple (let ([h (make-weak-custom-hash (lambda (a b)
                                                (string=? (format "~a" a)
                                                          (format "~a" b)))
                                              (lambda (a)
                                                (equal-hash-code (format "~a" a))))])
                (dict-set! h s1 'one)
                (dict-set! h s2 'two)
                h)
              #t #t #f
              "1")
  ;; preserve from GC:
  (list s1 s2))

(let ()
  (local-require racket/fixnum)
  (define (int=? x y) (fx= x y))
  (define (int-hc x) (fxand x 1))
  (define (int-hc2 x) (fxand x 2))
  (define (int-hash . numbers)
    (for/fold
        ([ht (make-immutable-custom-hash int=? int-hc int-hc2)])
        ([number (in-list numbers)])
      (dict-set ht number (number->string number))))
  (define (int-assoc . numbers)
    (for/fold
        ([ht (make-immutable-custom-hash int=?)])
        ([number (in-list numbers)])
      (dict-set ht number (number->string number))))

  (test #t equal? (int-hash) (int-hash))
  (test #t equal? (int-assoc) (int-assoc))
  (test #f equal? (int-hash) (int-assoc))
  (test #f equal? (int-assoc) (int-hash))

  (test #t equal? (int-hash 1) (int-hash 1))
  (test #f equal? (int-hash 1) (int-hash))
  (test #f equal? (int-hash) (int-hash 1))

  (test #t equal? (int-assoc 1) (int-assoc 1))
  (test #f equal? (int-assoc 1) (int-assoc))
  (test #f equal? (int-assoc) (int-assoc 1))

  (test #f equal? (int-assoc 1) (int-hash 1))
  (test #f equal? (int-hash 1) (int-assoc 1))
  (test #f equal? (int-hash 1) (int-assoc))
  (test #f equal? (int-assoc 1) (int-hash))
  (test #f equal? (int-hash) (int-assoc 1))
  (test #f equal? (int-assoc) (int-hash 1))

  (test #t equal? (int-hash 1 2) (int-hash 1 2))
  (test #t equal? (int-hash 1 2) (int-hash 2 1))
  (test #t equal? (int-hash 2 1) (int-hash 1 2))
  (test #f equal? (int-hash 1 2) (int-hash 1))
  (test #f equal? (int-hash 1 2) (int-hash))
  (test #f equal? (int-hash 1) (int-hash 1 2))
  (test #f equal? (int-hash) (int-hash 1 2))

  (test #t equal? (int-assoc 1 2) (int-assoc 1 2))
  (test #t equal? (int-assoc 1 2) (int-assoc 2 1))
  (test #t equal? (int-assoc 2 1) (int-assoc 1 2))
  (test #f equal? (int-assoc 1 2) (int-assoc 1))
  (test #f equal? (int-assoc 1 2) (int-assoc))
  (test #f equal? (int-assoc 1) (int-assoc 1 2))
  (test #f equal? (int-assoc) (int-assoc 1 2))

  (test #f equal? (int-assoc 1 2) (int-hash 1 2))
  (test #f equal? (int-assoc 1 2) (int-hash 2 1))
  (test #f equal? (int-assoc 2 1) (int-hash 1 2))
  (test #f equal? (int-assoc 1 2) (int-hash 1))
  (test #f equal? (int-assoc 1 2) (int-hash))
  (test #f equal? (int-assoc 1) (int-hash 1 2))
  (test #f equal? (int-assoc) (int-hash 1 2))

  (test #f equal? (int-hash 1 2) (int-assoc 1 2))
  (test #f equal? (int-hash 1 2) (int-assoc 2 1))
  (test #f equal? (int-hash 2 1) (int-assoc 1 2))
  (test #f equal? (int-hash 1 2) (int-assoc 1))
  (test #f equal? (int-hash 1 2) (int-assoc))
  (test #f equal? (int-hash 1) (int-assoc 1 2))
  (test #f equal? (int-hash) (int-assoc 1 2)))

;; ----------------------------------------

(report-errs)

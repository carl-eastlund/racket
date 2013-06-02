
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
  ;; Weak hash table tests, because we apparently don't have any

  (define x1 (string-copy "one"))
  (define x2 (string-copy "two"))
  (define x3 (string-copy "three"))
  (define x4 (string-copy "four"))

  (define (build-from make . xs)
    (define table (make))
    (for ([x (in-list xs)])
      (hash-set! table x (string->symbol x)))
    (collect-garbage)
    table)

  (define (build . xs) (apply build-from make-weak-hash xs))
  (define (buildeq . xs) (apply build-from make-weak-hasheq xs))
  (define (buildeqv . xs) (apply build-from make-weak-hasheqv xs))

  (test #true equal? (build) (build))
  (test #true equal? (buildeq) (buildeq))
  (test #true equal? (buildeqv) (buildeqv))

  (test #false equal? (build) (buildeq))
  (test #false equal? (buildeq) (buildeqv))
  (test #false equal? (buildeqv) (build))

  (test #true equal? (build x1 x2 x3) (build x1 x2 x3))
  (test #true equal? (buildeq x1 x2 x3) (buildeq x1 x2 x3))
  (test #true equal? (buildeqv x1 x2 x3) (buildeqv x1 x2 x3))

  (test #false equal? (build x1 x2) (build x1 x2 x3))
  (test #false equal? (buildeq x1 x2) (buildeq x1 x2 x3))
  (test #false equal? (buildeqv x1 x2) (buildeqv x1 x2 x3))

  (test #false equal? (build x1 x2 x3) (buildeq x1 x2 x3))
  (test #false equal? (buildeq x1 x2 x3) (buildeqv x1 x2 x3))
  (test #false equal? (buildeqv x1 x2 x3) (build x1 x2 x3))

  (define b (build x1 x2 x3))
  (define beq (buildeq x1 x2 x3))
  (define beqv (buildeqv x1 x2 x3))

  (define b* (hash-copy b))
  (define beq* (hash-copy beq))
  (define beqv* (hash-copy beqv))

  (test #true equal? (hash->list b) (hash->list b*))
  (test #true equal? (hash->list beq) (hash->list beq*))
  (test #true equal? (hash->list beqv) (hash->list beqv*))

  (hash-remove! b x1)
  (hash-remove! beq x1)
  (hash-remove! beqv x1)

  (hash-set! b* x4 'four)
  (hash-set! beq* x4 'four)
  (hash-set! beqv* x4 'four)

  (test #false equal? (hash->list b) (hash->list b*))
  (test #false equal? (hash->list beq) (hash->list beq*))
  (test #false equal? (hash->list beqv) (hash->list beqv*))

  (test #true equal? (hash->list b) (hash->list (build x2 x3)))
  (test #true equal? (hash->list beq) (hash->list (buildeq x2 x3)))
  (test #true equal? (hash->list beqv) (hash->list (buildeqv x2 x3)))

  (test #true equal? (hash->list b*) (hash->list (build x1 x2 x3 x4)))
  (test #true equal? (hash->list beq*) (hash->list (buildeq x1 x2 x3 x4)))
  (test #true equal? (hash->list beqv*) (hash->list (buildeqv x1 x2 x3 x4))))

(let ()
  (local-require racket/fixnum)
  (define (int=? x y) (fx= x y))
  (define (int-hc x) (fxand x 1))
  (define (int-hc2 x) (fxand x 2))

  (define (test-custom-hash make-A make-B)

    (test '() sort (dict->list (make-A)) < #:key car)
    (test '() sort (dict->list (make-B)) < #:key car)

    (test #t equal? (make-A) (make-A))
    (test #t equal? (make-B) (make-B))
    (test #f equal? (make-A) (make-B))
    (test #f equal? (make-B) (make-A))

    (test '((1 . "1")) sort (dict->list (make-A 1)) < #:key car)
    (test '((1 . "1")) sort (dict->list (make-B 1)) < #:key car)

    (test #t equal? (make-A 1) (make-A 1))
    (test #f equal? (make-A 1) (make-A))
    (test #f equal? (make-A) (make-A 1))

    (test #t equal? (make-B 1) (make-B 1))
    (test #f equal? (make-B 1) (make-B))
    (test #f equal? (make-B) (make-B 1))

    (test #f equal? (make-B 1) (make-A 1))
    (test #f equal? (make-A 1) (make-B 1))
    (test #f equal? (make-A 1) (make-B))
    (test #f equal? (make-B 1) (make-A))
    (test #f equal? (make-A) (make-B 1))
    (test #f equal? (make-B) (make-A 1))

    (test '((1 . "1") (2 . "2")) sort (dict->list (make-A 1 2)) < #:key car)
    (test '((1 . "1") (2 . "2")) sort (dict->list (make-B 1 2)) < #:key car)

    (test #t equal? (make-A 1 2) (make-A 1 2))
    (test #t equal? (make-A 1 2) (make-A 2 1))
    (test #t equal? (make-A 2 1) (make-A 1 2))
    (test #f equal? (make-A 1 2) (make-A 1))
    (test #f equal? (make-A 1 2) (make-A))
    (test #f equal? (make-A 1) (make-A 1 2))
    (test #f equal? (make-A) (make-A 1 2))

    (test #t equal? (make-B 1 2) (make-B 1 2))
    (test #t equal? (make-B 1 2) (make-B 2 1))
    (test #t equal? (make-B 2 1) (make-B 1 2))
    (test #f equal? (make-B 1 2) (make-B 1))
    (test #f equal? (make-B 1 2) (make-B))
    (test #f equal? (make-B 1) (make-B 1 2))
    (test #f equal? (make-B) (make-B 1 2))

    (test #f equal? (make-B 1 2) (make-A 1 2))
    (test #f equal? (make-B 1 2) (make-A 2 1))
    (test #f equal? (make-B 2 1) (make-A 1 2))
    (test #f equal? (make-B 1 2) (make-A 1))
    (test #f equal? (make-B 1 2) (make-A))
    (test #f equal? (make-B 1) (make-A 1 2))
    (test #f equal? (make-B) (make-A 1 2))

    (test #f equal? (make-A 1 2) (make-B 1 2))
    (test #f equal? (make-A 1 2) (make-B 2 1))
    (test #f equal? (make-A 2 1) (make-B 1 2))
    (test #f equal? (make-A 1 2) (make-B 1))
    (test #f equal? (make-A 1 2) (make-B))
    (test #f equal? (make-A 1) (make-B 1 2))
    (test #f equal? (make-A) (make-B 1 2)))

  (define (int-hash . numbers)
    (define ht (make-custom-hash int=? int-hc int-hc2))
    (for ([number (in-list numbers)])
      (dict-set! ht number (number->string number)))
    ht)
  (define (int-assoc . numbers)
    (define ht (make-custom-hash int=?))
    (for ([number (in-list numbers)])
      (dict-set! ht number (number->string number)))
    ht)

  (define (weak-int-hash . numbers)
    (define ht (make-weak-custom-hash int=? int-hc int-hc2))
    (for ([number (in-list numbers)])
      (dict-set! ht number (number->string number)))
    (collect-garbage)
    ht)
  (define (weak-int-assoc . numbers)
    (define ht (make-weak-custom-hash int=?))
    (for ([number (in-list numbers)])
      (dict-set! ht number (number->string number)))
    (collect-garbage)
    ht)

  (define (immutable-int-hash . numbers)
    (for/fold
        ([ht (make-immutable-custom-hash int=? int-hc int-hc2)])
        ([number (in-list numbers)])
      (dict-set ht number (number->string number))))
  (define (immutable-int-assoc . numbers)
    (for/fold
        ([ht (make-immutable-custom-hash int=?)])
        ([number (in-list numbers)])
      (dict-set ht number (number->string number))))

  (test-custom-hash int-hash int-assoc)
  (test-custom-hash weak-int-hash weak-int-assoc)
  (test-custom-hash immutable-int-hash immutable-int-assoc))

;; ----------------------------------------

(report-errs)

#lang racket/base

(require racket/private/generic ; to avoid circular dependencies
         (for-syntax racket/base))

(define-primitive-generics
  #:define-generic gen:dict
  #:define-predicate dict?
  #:define-property prop:dict
  #:define-accessor dict-method-table
  #:define-supported dict-def-table
  #:define-methods [(dict-ref  dict key [default])
                    (dict-set! dict key val)
                    (dict-set  dict key val)
                    (dict-remove! dict key)
                    (dict-remove  dict key)
                    (dict-count dict)
                    (dict-iterate-first dict)
                    (dict-iterate-next dict pos)
                    (dict-iterate-key dict pos)
                    (dict-iterate-value dict pos)]
  #:given-self dict
  #:given-extensions ()
  #:given-defaults ()
  #:given-fallbacks ())

(define (assoc? v)
  (and (list? v) (andmap pair? v)))

(define (d:dict? v)
  (or (hash? v)
      (vector? v)
      (assoc? v)
      (dict? v)))

(define (dict-mutable? d)
  (if (d:dict? d)
      (or (and (or (hash? d)
                   (vector? d))
               (not (immutable? d)))
          (and (dict? d)
               (hash-ref (dict-def-table d) 'dict-set! #f)
               #t))
      (raise-argument-error 'dict-mutable? "dict?" d)))

(define (dict-can-remove-keys? d)
  (if (d:dict? d)
      (or (hash? d)
          (assoc? d)
          (and (dict? d)
               (or (hash-ref (dict-def-table d) 'dict-remove! #f)
                   (hash-ref (dict-def-table d) 'dict-remove #f))
               #t))
      (raise-argument-error 'dict-can-remove-keys? "dict?" d)))

(define (dict-can-functional-set? d)
  (if (d:dict? d)
      (or (and (hash? d) (immutable? d))
          (assoc? d)
          (and (dict? d)
               (hash-ref (dict-def-table d) 'dict-set #f)
               #t))
      (raise-argument-error 'dict-can-functional-set? "dict?" d)))

(define (dict-has-key? d k)
  (define not-there (gensym))
  (not (eq? not-there (d:dict-ref d k not-there))))

(define d:dict-ref
  (case-lambda
   [(d key)
    (cond
     [(hash? d) (hash-ref d key)]
     [(vector? d) (vector-ref d key)]
     [(assoc? d)
      (let ([a (assoc key d)])
        (if a
            (cdr a)
            (raise-mismatch-error 'dict-ref
                                  (format "no value for key: ~e in: "
                                          key)
                                  d)))]
     [(dict? d) (dict-ref d key)]
     [else
      (raise-argument-error 'dict-ref "dict?" 0 d key)])]
   [(d key default)
    (cond
     [(hash? d) (hash-ref d key default)]
     [(vector? d) (if (and (exact-nonnegative-integer? key)
                           (key . < . (vector-length d)))
                      (vector-ref d key)
                      (if (procedure? default)
                          (default)
                          default))]
     [(assoc? d)
      (let ([a (assoc key d)])
        (if a
            (cdr a)
            (if (procedure? default)
                (default)
                default)))]
     [(dict? d)
      (dict-ref d key default)]
     [else
      (raise-argument-error 'dict-ref "dict?" 0 d key default)])]))

(define (dict-ref! d key new)
  (define not-there (gensym))
  (define v (d:dict-ref d key not-there))
  (if (eq? not-there v)
      (let ([n (if (procedure? new) (new) new)])
        (d:dict-set! d key n)
        n)
      v))

(define (d:dict-set! d key val)
  (cond
   [(hash? d) (hash-set! d key val)]
   [(vector? d) (vector-set! d key val)]
   [(assoc? d)
    (raise-argument-error 'dict-set! "mutable-dict?" 0 d key val)]
   [(dict? d)
    (let ([s! (hash-ref (dict-def-table d) 'dict-set! #f)])
      (if s!
          (dict-set! d key val)
          (raise-argument-error 'dict-set! "mutable-dict?" 0 d key val)))]
   [else
    (raise-argument-error 'dict-set! "dict?" 0 d key val)]))

(define (dict-set*! d . pairs)
  (unless (even? (length pairs))
    (error 'dict-set*! "expected an even number of association elements, but received an odd number: ~e" pairs))
  (let loop ([pairs pairs])
    (unless (null? pairs)
      (d:dict-set! d (car pairs) (cadr pairs))
      (loop (cddr pairs)))))

(define (d:dict-set d key val)
  (cond
   [(hash? d) (hash-set d key val)]
   [(vector? d)
    (raise-argument-error 'dict-set "functional-update-dict?" 0  d key val)]
   [(assoc? d)
    (let loop ([xd d])
      (cond
       [(null? xd) (list (cons key val))]
       [else
        (let ([a (car xd)])
          (if (equal? (car a) key) 
              (cons (cons key val) (cdr xd))
              (cons a (loop (cdr xd)))))]))]
   [(dict? d)
    (let ([s (hash-ref (dict-def-table d) 'dict-set #f)])
      (if s
          (dict-set d key val)
          (raise-argument-error 'dict-set "functional-update-dict?" 0 d key val)))]
   [else
    (raise-argument-error 'dict-set "dict?" 0 d key val)]))

(define (dict-set* d . pairs)
    (unless (even? (length pairs))
      (error 'dict-set* "expected an even number of association elements, but received an odd number: ~e" pairs))
    (let loop ([d d]
               [pairs pairs])
      (if (null? pairs)
          d
          (loop (d:dict-set d (car pairs) (cadr pairs))
                (cddr pairs)))))

(define dict-update!
  (case-lambda
   [(d key xform)
    (d:dict-set! d key (xform (d:dict-ref d key)))]
   [(d key xform default)
    (d:dict-set! d key (xform (d:dict-ref d key default)))]))

(define dict-update
  (case-lambda
   [(d key xform)
    (d:dict-set d key (xform (d:dict-ref d key)))]
   [(d key xform default)
    (d:dict-set d key (xform (d:dict-ref d key default)))]))

(define (d:dict-remove! d key)
  (cond
   [(hash? d) (hash-remove! d key)]
   [(vector? d)
    (raise-argument-error 'dict-remove! "dict-with-removeable-keys?" 0 d key)]
   [(assoc? d)
    (raise-argument-error 'dict-remove! "mutable-dict?" 0 d key)]
   [(dict? d)
    (let ([r! (hash-ref (dict-def-table d) 'dict-remove! #f)])
      (if r!
          (dict-remove! d key)
          (raise-argument-error 'dict-remove! "mutable-dict-with-removable-keys?" 0 d key)))]
   [else
    (raise-argument-error 'dict-remove! "dict?" 0 d key)]))

(define (d:dict-remove d key)
  (cond
   [(hash? d) (hash-remove d key)]
   [(vector? d)
    (raise-argument-error 'dict-remove "dict-with-removeable-keys?" 0 d key)]
   [(assoc? d)
    (let loop ([xd d])
      (cond
       [(null? xd) null]
       [else
        (let ([a (car xd)])
          (if (equal? (car a) key) 
              (cdr xd)
              (cons a (loop (cdr xd)))))]))]
   [(dict? d)
    (let ([s (hash-ref (dict-def-table d) 'dict-remove #f)])
      (if s
          (dict-remove d key)
          (raise-argument-error 'dict-remove "dict-with-functionally-removeable-keys?" 0 d key)))]
   [else
    (raise-argument-error 'dict-remove "dict?" 0 d key)]))

(define (d:dict-count d)
  (cond
   [(hash? d) (hash-count d)]
   [(vector? d) (vector-length d)]
   [(assoc? d) (length d)]
   [(dict? d) (dict-count d)]
   [else
    (raise-argument-error 'dict-count "dict?" d)]))

(struct assoc-iter (head pos))

(define (d:dict-iterate-first d)
  (cond
   [(hash? d) (hash-iterate-first d)]
   [(vector? d) (if (zero? (vector-length d))
                    #f
                    0)]
   [(assoc? d) (if (null? d) #f (assoc-iter d d))]
   [(dict? d) (dict-iterate-first d)]
   [else
    (raise-argument-error 'dict-iterate-first "dict?" d)]))

(define (d:dict-iterate-next d i)
  (cond
   [(hash? d) (hash-iterate-next d i)]
   [(vector? d) (let ([len (vector-length d)])
                  (cond
                   [(and (exact-nonnegative-integer? i)
                         (i . < . len))
                    (let ([i (add1 i)])
                      (if (= i len)
                          #f
                          i))]
                   [else
                    (raise-mismatch-error 
                     'dict-iterate-next
                     "invalid iteration position for vector: " 
                     i)]))]
   [(and (assoc-iter? i)
         (eq? d (assoc-iter-head i)))
    (let ([pos (cdr (assoc-iter-pos i))])
      (if (null? pos)
          #f
          (assoc-iter d pos)))]
   [(dict? d) (dict-iterate-next d i)]
   [(assoc? d)
    (raise-mismatch-error 
     'dict-iterate-next
     "invalid iteration position for association list: " 
     i)]
   [else
    (raise-argument-error 'dict-iterate-next "dict?" d)]))

(define (d:dict-iterate-key d i)
  (cond
   [(hash? d) (hash-iterate-key d i)]
   [(vector? d) i]
   [(and (assoc-iter? i) (eq? d (assoc-iter-head i))) (caar (assoc-iter-pos i))]
   [(dict? d) (dict-iterate-key d i)]
   [(assoc? d)
    (raise-mismatch-error 
     'dict-iterate-key
     "invalid iteration position for association list: " 
     i)]
   [else
    (raise-argument-error 'dict-iterate-key "dict?" d)]))

(define (d:dict-iterate-value d i)
  (cond
   [(hash? d) (hash-iterate-value d i)]
   [(vector? d) (vector-ref d i)]
   [(and (assoc-iter? i) (eq? d (assoc-iter-head i))) (cdar (assoc-iter-pos i))]
   [(dict? d) (dict-iterate-value d i)]
   [(assoc? d)
    (raise-mismatch-error 
     'dict-iterate-value
     "invalid iteration position for association list: " 
     i)]
   [else
    (raise-argument-error 'dict-iterate-value "dict?" d)]))

(define-sequence-syntax :in-dict
  (lambda () #'in-dict)
  (lambda (stx)
    (syntax-case stx ()
      [((key-id val-id) (_ dict-expr))
       #'[(key-id val-id)
          (:do-in ([(d) dict-expr])
                  (unless (d:dict? d)
                    (raise-argument-error 'in-dict "dict?" d))
                  ([i (d:dict-iterate-first d)])
                  i
                  ([(key-id) (d:dict-iterate-key d i)]
                   [(val-id) (d:dict-iterate-value d i)])
                  #t
                  #t
                  ((d:dict-iterate-next d i)))]]
      [_ #f])))

(define-sequence-syntax :in-dict-keys
  (lambda () #'in-dict-keys)
  (lambda (stx)
    (syntax-case stx ()
      [((key-id) (_ dict-expr))
       #'[(key-id)
          (:do-in ([(d) dict-expr])
                  (unless (d:dict? d)
                    (raise-argument-error 'in-dict-keys "dict?" d))
                  ([i (d:dict-iterate-first d)])
                  i
                  ([(key-id) (d:dict-iterate-key d i)])
                  #t
                  #t
                  ((d:dict-iterate-next d i)))]]
      [_ #f])))

(define-sequence-syntax :in-dict-values
  (lambda () #'in-dict-values)
  (lambda (stx)
    (syntax-case stx ()
      [((val-id) (_ dict-expr))
       #'[(key-id val-id)
          (:do-in ([(d) dict-expr])
                  (unless (d:dict? d)
                    (raise-argument-error 'in-dict-values "dict?" d))
                  ([i (d:dict-iterate-first d)])
                  i
                  ([(val-id) (d:dict-iterate-value d i)])
                  #t
                  #t
                  ((d:dict-iterate-next d i)))]]
      [_ #f])))

(define (in-dict d)
  (make-dict-sequence
   d
   (lambda (i)
     (values (d:dict-iterate-key d i)
             (d:dict-iterate-value d i)))
   (lambda (k v) #t)
   (lambda (i k v) #t)))

(define (in-dict-keys d)
  (make-dict-sequence
   d
   (lambda (i) (d:dict-iterate-key d i))
   (lambda (k) #t)
   (lambda (i k) #t)))

(define (in-dict-values d)
  (make-dict-sequence
   d
   (lambda (i) (d:dict-iterate-value d i))
   (lambda (v) #t)
   (lambda (i v) #t)))

(define (in-dict-pairs d)
  (make-dict-sequence
   d
   (lambda (i)
     (cons (d:dict-iterate-key d i)
           (d:dict-iterate-value d i)))
   (lambda (p) #t)
   (lambda (i p) #t)))

(define (make-dict-sequence d get val-true val+pos-true)
  (make-do-sequence
   (lambda ()
     (values get
             (lambda (i) (d:dict-iterate-next d i))
             (d:dict-iterate-first d)
             (lambda (i) i)
             val-true
             val+pos-true))))

(define (dict-map d f)
  (for/list ([(k v) (:in-dict d)])
    (f k v)))

(define (dict-for-each d f)
  (for ([(k v) (:in-dict d)])
    (f k v)))

(define (dict-keys d)
  (for/list ([k (:in-dict-keys d)])
    k))

(define (dict-values d)
  (for/list ([v (:in-dict-values d)])
    v))

(define (dict->list d)
  (for/list ([k*v (in-dict-pairs d)])
    k*v))

;; ----------------------------------------

(define custom-hash-ref
  (case-lambda
   [(d k) (hash-ref (parent-hash-table d)
                    ((custom-hash-make-box d) k)
                    (lambda ()
                      (raise-mismatch-error
                       'dict-ref
                       "no value found for key: "
                       k)))]
   [(d k fail) (hash-ref (parent-hash-table d)
                         ((custom-hash-make-box d) k)
                         fail)]))

(define (custom-hash-set! d k v)
  (hash-set! (parent-hash-table d) 
             ((custom-hash-make-box d) k)
             v))

(define (custom-hash-remove! d k)
  (hash-remove! (parent-hash-table d)
                ((custom-hash-make-box d) k)))

(define (custom-hash-set d k v)
  (let ([table (hash-set (parent-hash-table d) 
                         ((custom-hash-make-box d) k)
                         v)])
    (update-custom-hash d table)))

(define (custom-hash-remove d k)
  (let ([table (hash-remove (parent-hash-table d)
                            ((custom-hash-make-box d) k))])
    (update-custom-hash d table)))

(define (update-custom-hash d table)
  (immutable-custom-hash table
                         (parent-hash-equal-proc d)
                         (parent-hash-hash-proc d)
                         (parent-hash-hash2-proc d)))

(define (custom-hash-count d)
  (hash-count (parent-hash-table d)))

(define (custom-hash-iterate-first d)
  (hash-iterate-first (parent-hash-table d)))

(define (custom-hash-iterate-next d i)
  (hash-iterate-next (parent-hash-table d) i))

(define (custom-hash-iterate-key d i)
  (hash-box-key (hash-iterate-key (parent-hash-table d) i)))

(define (custom-hash-iterate-value d i)
  (hash-iterate-value (parent-hash-table d) i))

(define (custom-hash-make-box d)
  (define =? (parent-hash-equal-proc d))
  (define hc (parent-hash-hash-proc d))
  (define hc2 (parent-hash-hash2-proc d))
  (lambda (x) (hash-box x =? hc hc2)))

(struct hash-box (key equal-proc hash-proc hash2-proc)
  #:methods gen:equal+hash
  [(define (equal-proc a b recur)
     (define a.equal? (hash-box-equal-proc a))
     (define b.equal? (hash-box-equal-proc b))
     (and (recur a.equal? b.equal?)
          (recur (hash-box-hash-proc a) (hash-box-hash-proc b))
          (recur (hash-box-hash2-proc a) (hash-box-hash2-proc b))
          (cond
            [(procedure-arity-includes? a.equal? 3)
             (a.equal? (hash-box-key a) (hash-box-key b) recur)]
            [else
             (a.equal? (hash-box-key a) (hash-box-key b))])))
   (define (hash-proc a recur)
     (define a.hash-proc (hash-box-hash-proc a))
     (cond
       [(procedure-arity-includes? a.hash-proc 2)
        (a.hash-proc (hash-box-key a) recur)]
       [else
        (a.hash-proc (hash-box-key a))]))
   (define (hash2-proc a recur)
     (define a.hash2-proc (hash-box-hash2-proc a))
     (cond
       [(procedure-arity-includes? a.hash2-proc 2)
        (a.hash2-proc (hash-box-key a) recur)]
       [else
        (a.hash2-proc (hash-box-key a))]))])

(struct parent-hash (table equal-proc hash-proc hash2-proc)
  #:methods gen:equal+hash
  [(define (equal-proc a b recur)
     (and (recur (parent-hash-equal-proc a) (parent-hash-equal-proc b))
          (recur (parent-hash-hash-proc a) (parent-hash-hash-proc b))
          (recur (parent-hash-hash2-proc a) (parent-hash-hash2-proc b))
          (recur (parent-hash-table a) (parent-hash-table b))))
   (define (hash-proc a recur)
     (recur (parent-hash-table a)))
   (define (hash2-proc a recur)
     (recur (parent-hash-table a)))])

(struct custom-hash parent-hash ()
  #:methods gen:dict
  [(define dict-ref custom-hash-ref)
   (define dict-set! custom-hash-set!)
   (define dict-remove! custom-hash-remove!)
   (define dict-count custom-hash-count)
   (define dict-iterate-first custom-hash-iterate-first)
   (define dict-iterate-next custom-hash-iterate-next)
   (define dict-iterate-key custom-hash-iterate-key)
   (define dict-iterate-value custom-hash-iterate-value)])

(struct immutable-custom-hash parent-hash ()
  #:methods gen:dict
  [(define dict-ref custom-hash-ref)
   (define dict-set custom-hash-set)
   (define dict-remove custom-hash-remove)
   (define dict-count custom-hash-count)
   (define dict-iterate-first custom-hash-iterate-first)
   (define dict-iterate-next custom-hash-iterate-next)
   (define dict-iterate-key custom-hash-iterate-key)
   (define dict-iterate-value custom-hash-iterate-value)])

(define (make-custom-hash =? [hc default-hc] [hc2 default-hc])
  (check-hash-procs! 'make-custom-hash =? hc hc2)
  (custom-hash (make-hash) =? hc hc2))

(define (make-weak-custom-hash =? [hc default-hc] [hc2 default-hc])
  (check-hash-procs! 'make-weak-custom-hash =? hc hc2)
  (custom-hash (make-weak-hash) =? hc hc2))

(define (make-immutable-custom-hash =? [hc default-hc] [hc2 default-hc])
  (check-hash-procs! 'make-immutable-custom-hash =? hc hc2)
  (immutable-custom-hash (make-immutable-hash) =? hc hc2))

(define (default-hc x) 10001)

(define (check-hash-procs! who =? hc hc2)
  (unless (and (procedure? =?)
               (or (procedure-arity-includes? =? 3)
                   (procedure-arity-includes? =? 2)))
    (raise-argument-error who "a procedure of 2 or 3 arguments" =?))
  (unless (and (procedure? hc)
               (or (procedure-arity-includes? hc 2)
                   (procedure-arity-includes? hc 1)))
    (raise-argument-error who "a procedure of 1 or 2 arguments" hc))
  (unless (and (procedure? hc2)
               (or (procedure-arity-includes? hc2 2)
                   (procedure-arity-includes? hc2 1)))
    (raise-argument-error who "a procedure of 1 or 2 arguments" hc2)))

;; --------------------

(provide gen:dict
         prop:dict
         (rename-out
           [d:dict?              dict?]
           [d:dict-ref           dict-ref]
           [d:dict-set!          dict-set!]
           [d:dict-set           dict-set]
           [d:dict-remove!       dict-remove!]
           [d:dict-remove        dict-remove]
           [d:dict-count         dict-count]
           [d:dict-iterate-first dict-iterate-first]
           [d:dict-iterate-next  dict-iterate-next]
           [d:dict-iterate-key   dict-iterate-key]
           [d:dict-iterate-value dict-iterate-value])
         dict-mutable?
         dict-can-remove-keys?
         dict-can-functional-set?
         dict-has-key?
         dict-ref!
         dict-set*!
         dict-set*
         dict-update!
         dict-update
         dict-map
         dict-for-each
         dict-keys
         dict-values
         dict->list
         make-custom-hash
         make-immutable-custom-hash
         make-weak-custom-hash

         (rename-out [:in-dict in-dict]
                     [:in-dict-keys in-dict-keys]
                     [:in-dict-values in-dict-values])
         in-dict-pairs)
  

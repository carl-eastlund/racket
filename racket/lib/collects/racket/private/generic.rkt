#lang racket/base
(require racket/local
         (for-syntax racket/base
                     racket/local
                     racket/syntax
                     syntax/stx)
         "generic-methods.rkt"
         (only-in racket/function arity-includes?))

(provide define-primitive-generics
         define-primitive-generics/derived
         define/generic)

(begin-for-syntax

  (define (keyword-stx? v)
    (keyword? (syntax->datum v)))

  (define (check-identifier! stx)
    (unless (identifier? stx)
      (wrong-syntax stx "expected an identifier"))))

(define-syntax (define-primitive-generics/derived stx)
  (syntax-case stx ()
    [(_ #:define-generic generic-name
        #:define-predicate predicate-name
        #:define-property property-name
        #:define-accessor accessor-name
        #:define-supported supported-name
        #:define-methods [(method-name . method-signature) ...]
        #:given-self self-name
        #:given-defaults ([default-pred default-defn ...] ...)
        #:given-fallbacks [fallback-defn ...]
        #:given-source original)
     (parameterize ([current-syntax-context #'original])
       (check-identifier! #'self-name)
       (check-identifier! #'generic-name)
       (check-identifier! #'predicate-name)
       (check-identifier! #'supported-name)
       (for-each check-identifier! (syntax->list #'(method-name ...)))
       (define n (length (syntax->list #'(method-name ...))))
       (define/with-syntax size n)
       (define/with-syntax (index ...) (for/list ([i (in-range n)]) i))
       (define/with-syntax contract-str
         (format "~s" (syntax-e #'predicate-name)))
       (define/with-syntax (default-pred-name ...)
         (generate-temporaries #'(default-pred ...)))
       (define/with-syntax (default-impl-name ...)
         (generate-temporaries #'(default-pred ...)))
       #'(begin
           (define-syntax generic-name
             (make-generic-info (quote-syntax property-name)
                                (list (quote-syntax method-name) ...)))
           (define (prop:guard x info)
             (unless (and (vector? x) (= (vector-length x) 'size))
               (raise-argument-error 'generic-name
                                     (format "expected a vector of length ~a"
                                             'size)
                                     x))
             (check-generic-method
               #:given-generic generic-name
               #:given-method method-name
               #:given-signature method-signature
               #:given-impl (vector-ref x 'index)
               #:given-source original)
             ...
             x)
           (define-values (property-name prop:pred prop:get)
             (make-struct-type-property 'generic-name prop:guard))
           (define (predicate-name self-name)
             (or (prop:pred self-name) (default-pred-name self-name) ...))
           (define (accessor-name self-name [who 'accessor-name])
             (cond
               [(prop:pred self-name) (prop:get self-name)]
               [(default-pred-name self-name) default-impl-name]
               ...
               [else (raise-syntax-error who 'contract-name self-name)]))
           (define-generic-support
             #:define-supported supported-name
             #:given-self self-name
             #:given-methods [method-name ...]
             #:given-table (accessor-name self-name 'supported-name)
             #:given-source original)
           (define-generic-method
             #:define-method method-name
             #:given-signature method-signature
             #:given-self self-name
             #:given-proc
             (or (vector-ref (accessor-name self-name 'method-name) 'index)
                 (vector-ref fallback-name 'index))
             #:given-source original)
           ...
           (define-values (default-pred-name ...)
             (values default-pred ...))
           (define fallback-name
             (generic-method-table generic-name fallback-defn ...))
           (define default-impl-name
             (generic-method-table generic-name default-defn ...))
           ...))]))

(define-syntax (define-primitive-generics stx)
  (syntax-case stx ()
    [(_ #:define-generic generic-name
        #:define-predicate predicate-name
        #:define-property property-name
        #:define-accessor accessor-name
        #:define-supported supported-name
        #:define-methods [(method-name . signature-name) ...]
        #:given-self self-name
        #:given-defaults ([default-pred default-defn ...] ...)
        #:given-fallbacks [fallback-defn ...])
     #`(define-primitive-generics/derived
         #:define-generic generic-name
         #:define-predicate predicate-name
         #:define-property property-name
         #:define-accessor accessor-name
         #:define-supported supported-name
         #:define-methods [(method-name . signature-name) ...]
         #:given-self self-name
         #:given-defaults ([default-pred default-defn ...] ...)
         #:given-fallbacks [fallback-defn ...]
         #:given-source #,stx)]))

(define-syntax (define-generic-support stx)
  (syntax-case stx ()
    [(_ #:define-supported supported-name
        #:given-self self-name
        #:given-methods [method-name ...]
        #:given-table table
        #:given-source original)
     (parameterize ([current-syntax-context #'original])
       (check-identifier! #'supported-name)
       (check-identifier! #'self-name)
       (for-each check-identifier! (syntax->list #'(method-name ...)))
       (define/with-syntax (index ...)
         (for/list ([idx (in-naturals)]
                    [stx (in-list (syntax->list #'(method-name ...)))])
           idx))
       #'(define (supported-name self-name)
           (define v table)
           (make-immutable-hasheqv
             (list (cons 'method-name (vector-ref v 'index)) ...))))]))

(begin-for-syntax

  (define (method-formals/application name-stx proc-stx self-id sig-stx)

    (define (check-method-signature!)
      (define dup (check-duplicate-identifier ids))
      (when dup (wrong-syntax dup "duplicate method argument"))
      (for ([id (in-list non-req)]
            #:when (free-identifier=? id self-id))
        (wrong-syntax id
                      "the generic name must be used as ~a"
                      "a required, by-position argument"))
      (define matches
        (for/list ([id (in-list req)]
                   #:when (free-identifier=? id self-id))
          id))
      (unless (pair? matches)
        (wrong-syntax sig-stx
                      "did not find ~a among ~a to ~s: ~s"
                      "the generic name"
                      "the required, by-position arguments"
                      (syntax-e name-stx)
                      (map syntax->datum req)))
      (when (pair? (cdr matches))
        (wrong-syntax (cadr matches)
                      "found ~a among the arguments to ~s"
                      "more than one occurrence of the generic name"
                      (syntax-e name-stx))))

    (define (method-formals)
      (define/with-syntax [req-name ...] req)
      (define/with-syntax [opt-name ...] opt)
      (define/with-syntax ([req-arg ...] ...) req-kw)
      (define/with-syntax ([opt-key opt-val] ...) opt-kw)
      (define/with-syntax ([opt-arg ...] ...)
        #'([opt-key [opt-val default-arg]] ...))
      (define/with-syntax tail (or rest '()))
      #'(req-name ...
         [opt-name default-arg] ...
         req-arg ... ...
         opt-arg ... ...
         . tail))

    (define (method-application)
      (define app-count (* (add1 (length opt)) (expt 2 (length opt-kw))))
      (if (<= app-count app-threshold)
          (by-position req opt rest
                       (lambda (pos tail)
                         (by-keyword req-kw opt-kw
                                     (lambda (keys vals)
                                       (make-application pos keys vals tail)))))
          (brute-force-application)))

    (define app-threshold 64)

    (define (brute-force-application)
      (define/with-syntax [r ...] req)
      (define/with-syntax [o ...] opt)
      (define/with-syntax ([key val] ...)
        (sort (append req-kw opt-kw) keyword<?
              #:key (compose car syntax->datum)))
      (define/with-syntax tail (if rest rest #'(quote ())))
      (define/with-syntax f proc-stx)
      (define/with-syntax [tmp.ks tmp.vs tmp.k tmp.v tmp.args tmp.arg]
        (generate-temporaries '(ks vs k v args arg)))
      #'(let ()
          (define-values (tmp.ks tmp.vs)
            (for/lists
                (tmp.ks tmp.vs)
                ([tmp.k (in-list '(key ...))]
                 [tmp.v (in-list (list val ...))]
                 #:unless (eq? tmp.v default-arg))
              (values tmp.k tmp.v)))
          (define tmp.args
            (for/list ([tmp.arg (in-list (list* o ... tail))]
                       #:unless (eq? tmp.arg default-arg))
              tmp.arg))
          (keyword-apply f tmp.ks tmp.vs r ... tmp.args)))

    (define (push lst x) (append lst (list x)))

    (define (by-position req opt tail make-app)
      (cond
        [tail #`(if (pair? #,tail)
                    #,(make-app (append req opt) tail)
                    #,(by-position req opt #f make-app))]
        [(null? opt) (make-app req tail)]
        [else
         (define/with-syntax arg (car opt))
         #`(if (eq? arg default-arg)
               #,(make-app req #f)
               #,(by-position (push req (car opt)) (cdr opt) tail make-app))]))

    (define (by-keyword req opt make-app)
      (cond
        [(null? opt) (make-app (map car req) (map cadr req))]
        [else
         (define/with-syntax arg (cadr (car opt)))
         #`(if (eq? arg default-arg)
               #,(by-keyword req (cdr opt) make-app)
               #,(by-keyword (push req (car opt)) (cdr opt) make-app))]))

    (define (make-application pos keys vals tail)
      (define/with-syntax f proc-stx)
      (define/with-syntax [arg ...] pos)
      (define/with-syntax ([kw ...] ...) (map list keys vals))
      (define/with-syntax x (generate-temporary 'x))
      (if tail
          (with-syntax ([rest tail])
            #'(apply f kw ... ... arg ... tail))
          #'(f kw ... ... arg ...)))

    (define-values (req req-kw opt opt-kw rest)
      (parse-method-signature sig-stx))
    (define req-kw-ids (map cadr req-kw))
    (define opt-kw-ids (map cadr opt-kw))
    (define rest-ids (if rest (list rest) '()))
    (define non-req (append opt req-kw-ids opt-kw-ids rest-ids))
    (define ids (append req non-req))

    (check-method-signature!)
    (list (method-formals)
          (method-application)))

  (define (parse-method-signature stx)
    (syntax-case stx ()
      [(kw [val] . args)
       (and (keyword-stx? #'kw) (identifier? #'val))
       (let-values ([(req req-kw opt opt-kw rest)
                     (parse-method-signature #'args)])
         (values req req-kw opt (cons (list #'kw #'val) opt-kw) rest))]
      [(kw val . args)
       (and (keyword-stx? #'kw) (identifier? #'val))
       (let-values ([(req req-kw opt opt-kw rest)
                     (parse-method-signature #'args)])
         (values req (cons (list #'kw #'val) req-kw) opt opt-kw rest))]
      [(kw other . args)
       (keyword-stx? #'kw)
       (wrong-syntax #'other
                     "expected required or optional identifier")]
      [(kw . args)
       (keyword-stx? #'kw)
       (wrong-syntax #'kw
                     "expected a required or optional identifier following ~s"
                     (syntax-e #'kw))]
      [([val] . args)
       (identifier? #'val)
       (let-values ([(req req-kw opt opt-kw rest)
                     (parse-method-signature #'args)])
         (values req req-kw (cons #'val opt) opt-kw rest))]
      [(val . args)
       (identifier? #'val)
       (let-values ([(req req-kw opt opt-kw rest)
                     (parse-method-signature #'args)])
         (values (cons #'val req) req-kw opt opt-kw rest))]
      [(other . args)
       (wrong-syntax #'other
                     "expected a keyword or a required or optional identifier")]
      [rest (identifier? #'rest) (values '() '() '() '() #'rest)]
      [() (values '() '() '() '() #f)]
      [other
       (wrong-syntax #'other
                     "expected an identifier or an empty list")])))

(define default-arg
  (gensym 'default-arg))

(define-syntax (define-generic-method stx)
  (syntax-case stx ()
    [(_ #:define-method method-name
        #:given-signature method-signature
        #:given-self self-name
        #:given-proc proc
        #:given-source original)
     (parameterize ([current-syntax-context #'original])
       (check-identifier! #'method-name)
       (check-identifier! #'self-name)
       (define/with-syntax proc-name (generate-temporary #'method-name))
       (define/with-syntax [method-formals method-apply]
         (method-formals/application #'method-name
                                     #'proc-name
                                     #'self-name
                                     #'method-signature))
       #'(define (method-name . method-formals)
           (define proc-name proc)
           (unless proc-name
             (raise-syntax-error 'method-name
                                 "not implemented for ~e"
                                 self-name))
           method-apply))]))

(define-syntax (check-generic-method stx)
  (syntax-case stx ()
    [(check-generic-method
       #:given-generic generic-name
       #:given-method method-name
       #:given-signature method-signature
       #:given-impl method-expr
       #:given-source original)
     (parameterize ([current-syntax-context #'original])
       (check-identifier! #'generic-name)
       (check-identifier! #'method-name)
       (define-values (req req-kw opt opt-kw rest)
         (parse-method-signature #'method-signature))
       (define/with-syntax req-n (length req))
       (define/with-syntax opt-n (length opt))
       (define/with-syntax rest? (identifier? rest))
       (define/with-syntax [req-key ...]
         (sort (map car req-kw) keyword<? #:key syntax-e))
       (define/with-syntax [opt-key ...]
         (sort (map car opt-kw) keyword<? #:key syntax-e))
       #'(check-method 'generic-name
                       'method-name
                       method-expr
                       'req-n
                       'opt-n
                       'rest?
                       '(req-key ...)
                       '(opt-key ...)))]))

(define (check-method who what v req-n opt-n rest? req-kws opt-kws)
  (when v

    (define (expect fmt . args)
      (format "expected ~a for method ~s"
              (apply format fmt args)
              what))
    (define (arguments n)
      (format "~a ~a" n (if (= n 1) "argument" "arguments")))

    (unless (procedure? v)
      (raise-arguments-error who (expect "a procedure") (format "~s" what) v))

    (define arity (procedure-arity v))
    (for ([i (in-range req-n (+ req-n opt-n 1))])
      (unless (arity-includes? arity i)
        (raise-arguments-error
         who
         (expect "a procedure that accepts ~a" (arguments i))
         (format "~s" what)
         v)))
    (when rest?
      (unless (arity-includes? arity (arity-at-least (+ req-n opt-n)))
        (raise-arguments-error
         who
         (expect "a procedure that accepts ~a or more arguments" (+ req-n opt-n))
         (format "~s" what)
         v)))

    (define-values (v-req-kws v-opt-kws) (procedure-keywords v))

    (define (check-accepts kws [among v-opt-kws])
      (cond
        [(null? kws) (void)]
        [(or (null? among)
             (keyword<? (car kws) (car among)))
         (raise-arguments-error
          who
          (expect "a procedure that accepts keyword argument ~s" (car kws))
          (format "~s" what)
          v)]
        [(keyword<? (car among) (car kws))
         (check-accepts kws (cdr among))]
        [else
         (check-accepts (cdr kws) (cdr among))]))
    (when v-opt-kws
      (check-accepts req-kws)
      (check-accepts opt-kws))

    (define (check-requires kws [among v-req-kws])
      (cond
        [(null? among) (void)]
        [(or (null? kws)
             (keyword<? (car among) (car kws)))
         (raise-arguments-error
          who
          (expect "a procedure that does not require keyword argument ~s"
                  (car among))
          (format "~s" what)
          v)]
        [(keyword<? (car kws) (car among))
         (check-requires kws (cdr among))]
        [else
         (check-requires (cdr kws) (cdr among))]))
    (check-requires req-kws)))

#;(define-syntax (define-generics stx)
  (syntax-case stx () ;; can't use syntax-parse, since it depends on us
    ;; keyword arguments must _all_ be provided _in_order_. For the
    ;; user-facing version of `define-generics', see racket/generic.
    ;;
    ;; The `header` is the original name the library writer provides
    ;; that is used to define the `name`, `prop:name`, and `name?`
    ;; identifiers. We have it here so that we can use it to match
    ;; the method header's self argument.
    [(_ (header name prop:name name?
                #:defined-table defined-table
                #:defaults ([pred? impl ...] ...)
                #:fallbacks (fallback ...)
                ;; are we being passed an existing struct property? If so,
                ;; this kw arg is bound to the struct property accessor, and
                ;; we don't define the struct property
                #:prop-defined-already? defined-already?
                ;; Passed in by `define-generics` in racket/generic.
                ;; This enables us to cut the dependency on racket/contract
                ;; for users of this private module. Pass in #f
                ;; to avoid defining a contract.
                #:define-contract define-generics-contract)
        (generic . generic-args) ...)
     (parameterize ([current-syntax-context stx])
       (check-identifier! #'header)
       (check-identifier! #'name)
       (check-identifier! #'prop:name)
       (check-identifier! #'name?)
       (check-identifier! #'defined-table)
       (for-each check-identifier! (syntax->list #'(generic ...)))
       (define generics (syntax->list #'(generic ...)))
       (define name-stx (symbol->string (syntax-e #'name?)))
       (define idxs (for/list ([i (in-naturals 0)] [_ generics]) i))
       (define prop-defined-already? (syntax-e #'defined-already?))
       ;; syntax introducers for each default implementation set
       ;; these connect the default method definitions to the
       ;; appropriate dispatch reference in the generic function body
       (define pred-introducers
         (map (λ (_) (make-syntax-introducer))
              (syntax->list #'(pred? ...))))
       ;; mark each set of default methods for a default set and
       ;; then flatten all of the default definitions
       (define method-impl-list
         (map syntax->list
              (for/list ([introducer pred-introducers]
                         [meths (syntax->list #'((impl ...) ...))])
                (introducer meths))))
       ;; mark each generic function name for a default set
       (define marked-generics
         (for/list ([generic generics])
           (for/list ([introducer pred-introducers])
             (introducer generic))))
       (define/with-syntax name-str name-stx)
       (define/with-syntax how-many-generics (length idxs))
       (define/with-syntax (generic-arity-coerce ...)
         (generate-temporaries #'(generic ...)))
       (define/with-syntax (generic-idx ...) idxs)
       (define/with-syntax (generic-this-idx ...)
         (for/list ([top-ga (syntax->list #'(generic-args ...))])
           (let loop ([ga top-ga]
                      [i 0])
             (syntax-case ga ()
               [(keyword id . ga)
                (and (keyword-stx? #'keyword)
                  (identifier? #'id))
                (loop #'ga i)]
               [(id . ga)
                (and (identifier? #'id))
                (if (free-identifier=? #'header #'id)
                  i
                  (loop #'ga (add1 i)))]
               [(keyword [id] . ga)
                (and (keyword-stx? #'keyword)
                  (identifier? #'id))
                (loop #'ga i)]
               [([id] . ga)
                (and (identifier? #'id))
                (loop #'ga i)]
               [_
                (identifier? #'id)
                (raise-syntax-error #f "No required by-position generic argument" top-ga)]))))
       (define/with-syntax (fake-args ...)
         (for/list ([ga (syntax->list #'(generic-args ...))])
           (let loop ([ga ga])
             (syntax-case ga ()
               [(keyword id . ga)
                (and (keyword-stx? #'keyword)
                  (identifier? #'id))
                #`(keyword id . #,(loop #'ga))]
               [(id . ga)
                (and (identifier? #'id))
                #`(id . #,(loop #'ga))]
               [(keyword [id] . ga)
                (and (keyword-stx? #'keyword)
                  (identifier? #'id))
                #`(keyword [id #f] . #,(loop #'ga))]
               [([id] . ga)
                (and (identifier? #'id))
                #`([id #f] . #,(loop #'ga))]
               [id
                (identifier? #'id)
                #'id]
               [()
                #'()]))))
       ;; if we're the ones defining the struct property,
       ;; generate a new id, otherwise use the struct property
       ;; accessor that we were passed
       (define/with-syntax get-generics
         (if prop-defined-already?
           #'defined-already?
           (generate-temporary 'get-generics)))
       ;; for each generic method, builds a cond clause to do the
       ;; predicate dispatch found in method-impl-list
       (define/with-syntax ((cond-impl ...) ...) marked-generics)
       (define/with-syntax ((cond-name ...) ...)
         (apply map (compose cdr list) pred-introducers marked-generics))
       (define/with-syntax (-name?) (generate-temporaries #'(name?)))
       (define/with-syntax (prop-defn ...)
         (if prop-defined-already?
             '() ;; we don't need to define it
             (list
              #'(begin
                  (define-values (prop:name -name? get-generics)
                    (make-struct-type-property
                     'name
                     (lambda (generic-vector si)
                       (unless (vector? generic-vector)
                         (error 'name
                                "bad generics table, expecting a vector, got ~e"
                                generic-vector))
                       (unless (= (vector-length generic-vector)
                                  how-many-generics)
                         (error 'name
                                "bad generics table, expecting a vector of length ~e, got ~e"
                                how-many-generics
                                (vector-length generic-vector)))
                       (vector (let ([mthd-generic (vector-ref generic-vector generic-idx)])
                                 (and mthd-generic
                                      (generic-arity-coerce 'generic mthd-generic)))
                               ...))
                     null #t))
                  ;; overrides the interface predicate so that any of the default
                  ;; types also answer #t
                  (define (name? x)
                    (or (-name? x) (pred? x) ...))))))
       (define/with-syntax (contract-defn ...)
         (if (syntax-e #'define-generics-contract)
             (list #'(define-generics-contract header name? get-generics
                       (generic generic-idx) ...))
             ;; don't define a contract when given #f
             '()))
       (define/with-syntax ((method-impl ...) ...) method-impl-list)
       (define/with-syntax pred-name
         (if prop-defined-already?
             #'(name? this)
             #'(-name? this)))
       (define/with-syntax (fallback-name ...)
         (generate-temporaries #'(generic ...)))
       #'(begin
           (define-syntax name (list #'prop:name #'generic ...))
           ;; XXX optimize no kws or opts
           (define generic-arity-coerce
             (let*-values ([(p) (lambda fake-args #f)]
                           [(generic-arity-spec) (procedure-arity p)]
                           [(generic-required-kws generic-allowed-kws) (procedure-keywords p)])
               (lambda (method-name f)
                 (unless (procedure? f)
                   (raise-arguments-error
                    'name
                    "generic method definition is not a function"
                    "method" method-name
                    "given" f))
                 (unless (arity-includes? (procedure-arity f) generic-arity-spec)
                   (raise-arguments-error
                    'name
                    "method definition has an incorrect arity"
                    "method" method-name
                    "given arity" (procedure-arity f)
                    "expected arity" generic-arity-spec))
                 (procedure-rename
                  (procedure-reduce-keyword-arity f generic-arity-spec generic-required-kws generic-allowed-kws)
                  method-name))))
           ...
           prop-defn ...
           ;; Hash table mapping method name symbols to
           ;; whether the given method is implemented
           (define (defined-table this)
             (unless (name? this)
               (raise-argument-error 'defined-table name-str this))
             (cond
               [pred-name
                (for/hash ([name (in-list '(generic ...))]
                           [gen (in-vector (get-generics this))])
                  (values name (not (not gen))))]
               [(pred? this)
                (for/hash ([name (in-list '(generic ...))]
                           [gen (in-list (list cond-name ...))])
                  (values name (not (not gen))))]
               ...
               [else (raise-argument-error 'defined-table name-str this)]))
           ;; Define the contract that goes with this generic interface
           contract-defn ...
           ;; Define default implementations
           (define-values (cond-name ...)
             (let ([cond-name #f] ...)
               method-impl ...
               (values cond-name ...)))
           ...
           (define-values (fallback-name ...)
             (let ([generic #f] ...)
               fallback ...
               (values generic ...)))
           ;; Define generic functions
           (define generic
             (generic-arity-coerce
              'generic
              ;; We could put `generic-args` here for the method header, but
              ;; since we need to keyword-apply the method in the method table,
              ;; it doesn't help. Thus we use `make-keyword-procedure`.
              ;;
              ;; If keyword-apply ends up being a bottleneck, consider
              ;; adding the second argument to `make-keyword-procedure` again.
              (make-keyword-procedure
               (lambda (kws kws-args . given-args)
                 (define this (list-ref given-args generic-this-idx))
                 (define m
                   (cond
                     [pred-name (vector-ref (get-generics this) generic-idx)]
                     [(pred? this) cond-impl]
                     ...
                     [else (raise-argument-error 'generic 'name-str this)]))
                 (define p (or m fallback-name))
                 (unless p
                   (error 'generic "not implemented for ~e" this))
                 (keyword-apply p kws kws-args given-args)))))
           ...))]))

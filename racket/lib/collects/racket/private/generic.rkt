#lang racket/base
(require racket/local
         (for-syntax racket/base
                     racket/local
                     racket/syntax
                     syntax/stx)
         "generic-methods.rkt"
         (only-in racket/function arity-includes?))

(provide define-generics/derived
         define-generics-for-property
         ignore-generics-contract
         define/generic)

(begin-for-syntax

  (define (keyword-stx? v)
    (keyword? (syntax->datum v)))

  (define (check-identifier! stx)
    (unless (identifier? stx)
      (wrong-syntax stx "expected an identifier"))))

(define-syntax (define-generics-for-property stx)
  (syntax-case stx ()
    [(_ #:define-generic generic-name
        #:define-supported supported-name
        #:define-methods [(method-name . method-signature) ...]
        #:given-self self-name
        #:given-predicate predicate-name
        #:given-property property-name
        #:given-accessor accessor-name)
     (parameterize ([current-syntax-context stx])
       (check-identifier! #'generic-name)
       (check-identifier! #'supported-name)
       (check-identifier! #'property-name)
       (check-identifier! #'accessor-name)
       (check-identifier! #'predicate-name)
       (for-each check-identifier!
                 (syntax->list #'(method-name ...)))
       (define/with-syntax (index ...)
         (for/list ([idx (in-naturals ...)]
                    [stx (in-list (syntax->list #'(method-name ...)))])
           idx))
       (define/with-syntax contract-str
         (format "~s" (syntax-e #'predicate-name)))
       (define/with-syntax original stx)
       #'(begin
           (define-syntax generic-name
             (make-generic-info (quote-syntax property-name)
                                (list (quote-syntax method-name) ...)))
           (define-syntax-rule (get who self-name)
             (if (predicate-name self-name)
                 (accessor-name self-name)
                 (raise-argument-error who 'contract-str self-name)))
           (define-generic-support
             #:define-supported supported-name
             #:given-self self-name
             #:given-methods [method-name ...]
             #:given-impls (get 'supported-name self-name)
             #:given-source original)
           (define-generic-method
             #:define-method method-name
             #:given-signature method-signature
             #:given-self self-name
             #:given-impl (vector-ref (get 'method-name self-name) 'index)
             #:given-source original)
           ...))]))

(define-syntax (define-generics/derived stx)
  (syntax-case stx ()
    [(_ #:define-generic generic-name
        #:define-predicate predicate-name
        #:define-supported supported-name
        #:define-methods [(method-name . signature-name) ...]
        #:given-self self-name
        #:given-defaults ([default-pred default-defn ...] ...)
        #:given-fallbacks [fallback-defn ...]
        #:given-contract define-contract-name
        #:given-source original)
     (parameterize ([current-syntax-context #'original])
       (check-identifier! #'self-name)
       (check-identifier! #'define-contract-name)
       (check-identifier! #'generic-name)
       (check-identifier! #'predicate-name)
       (check-identifier! #'supported-name)
       (for-each check-identifier! (syntax->list #'(method-name ...)))
       (define/with-syntax (index ...)
         (for/list ([idx (in-naturals ...)]
                    [stx (in-list (syntax->list #'(method-name ...)))])
           idx))
       (define/with-syntax contract-str
         (format "~s" (syntax-e #'predicate-name)))
       (define/with-syntax default-pred-name
         (generate-temporaries #'(default-pred ...)))
       (define/with-syntax default-impl-name
         (generate-temporaries #'(default-pred ...)))
       #'(begin
           (define-syntax generic-name
             (make-generic-info (quote-syntax property-name)
                                (list (quote-syntax method-name) ...)))
           (define-values (prop:name prop:pred prop:get)
             (make-struct-type-property 'generic-name))
           (define-syntax-rule (get who self-name)
             (cond
               [(prop:pred self-name) (prop:get self-name)]
               [(default-pred-name self-name) default-impl-name]
               ...
               [else (raise-syntax-error who 'contract-name self-name)]))
           (define-generic-support
             #:define-supported supported-name
             #:given-self self-name
             #:given-methods [method-name ...]
             #:given-impl (get 'supported-name self-name)
             #:given-source original)
           (define-generic-method
             #:define-method method-name
             #:given-signature method-signature
             #:given-self self-name
             #:given-impl (or (vector-ref (get 'method-name self-name) 'index)
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
#lang racket/base
(require racket/contract/base
         racket/contract/combinator
         "private/generic.rkt"
         (for-syntax racket/base racket/local racket/syntax syntax/stx))

;; Convenience layer on top of racket/private/generic.
;; To avoid circular dependencies, racket/private/generic cannot use
;; `parse-keyword-options' (which depends on racket/dict). So we do
;; keyword argument parsing here.
;; Files that use racket/private/generic _must_ pass _all_ keyword
;; arguments to define-generics _in_order_.

(provide define-generics define/generic)

(begin-for-syntax

  (define (parse stx table defaults fallbacks methods)
    (syntax-case stx ()
      [(#:defined-table name . args)
       (identifier? #'name)
       (if table
           (wrong-syntax (stx-car stx)
                         "duplicate #:defined-table specification")
           (parse #'args #'name defaults fallbacks methods))]
      [(#:defined-table . other)
       (wrong-syntax (stx-car stx) "invalid #:defined-table specification")]
      [(#:defaults ([pred defn ...] ...) . args)
       (if defaults
           (wrong-syntax (stx-car stx) "duplicate #:defaults specification")
           (parse #'args table #'([pred defn ...] ...) fallbacks methods))]
      [(#:defaults . other)
       (wrong-syntax (stx-car stx) "invalid #:defaults specification")]
      [(#:fallbacks [fallback ...] . args)
       (if fallbacks
           (wrong-syntax (stx-car stx) "duplicate #:fallbacks specification")
           (parse #'args table defaults #'[fallback ...] methods))]
      [(#:fallbacks . other)
       (wrong-syntax (stx-car stx) "invalid #:fallbacks specification")]
      [(kw . args)
       (keyword? (syntax-e #'kw))
       (wrong-syntax #'kw "invalid keyword argument")]
      [((method . sig) . args)
       (parse #'args table defaults fallbacks (cons #'(method . sig) methods))]
      [(other . args)
       (wrong-syntax #'other
                     "expected a method identifier with formal arguments")]
      [() (values (or table (generate-temporary 'table))
                  (or defaults '())
                  (or fallbacks '())
                  (reverse methods))]
      [other
       (wrong-syntax #'other
                     "expected a list of arguments with no dotted tail")])))

(define-syntax (define-generics stx) ; allows out-of-order / optional kw args
  (syntax-case stx ()
    [(_ name . rest)
     (parameterize ([current-syntax-context stx])
       (unless (identifier? #'name)
         (wrong-syntax #'name "expected an identifier"))
       (define-values (table defaults fallbacks methods)
         (parse #'rest #f #f #f '()))
       (define/with-syntax [default ...] defaults)
       (define/with-syntax [fallback ...] fallbacks)
       (define/with-syntax [(method . sig) ...] methods)
       (define/with-syntax pred-name (format-id #'name "~a?" #'name))
       (define/with-syntax gen-name (format-id #'name "gen:~a" #'name))
       (define/with-syntax ctc-name (format-id #'name "~a/c" #'name))
       (define/with-syntax table-name table)
       (define/with-syntax prop-name (generate-temporary 'prop))
       (define/with-syntax get-name (generate-temporary 'get))
       (define/with-syntax original stx)
       #'(begin
           (define-primitive-generics/derived
             #:define-generic gen-name
             #:define-predicate pred-name
             #:define-property prop-name
             #:define-accessor get-name
             #:define-supported table-name
             #:define-methods [(method . sig) ...]
             #:given-self name
             #:given-defaults [default ...]
             #:given-fallbacks [fallback ...]
             #:given-source original)
           (define-generics-contract
             #:define-contract ctc-name
             #:given-predicate pred-name
             #:given-accessor get-name
             #:given-methods [method ...])))]))

;; generate a contract combinator for instances of a generic interface
(define-syntax (define-generics-contract stx)
  (syntax-case stx ()
    [(_ #:define-contract name/c
        #:given-predicate name?
        #:given-accessor accessor
        #:given-methods [generic ...])
     (with-syntax ([(generic-idx ...)
                    (for/list ([i (in-naturals)]
                               [stx (in-list (syntax->list #'(generic ...)))])
                      i)])
       #`(define-syntax (name/c stx)
           (syntax-case stx ()
             [(_ [method-id ctc] (... ...))
              (andmap (λ (id) (and (identifier? id)
                                   ;; make sure the ids are all
                                   ;; in the interface
                                   (member (syntax-e id) (list 'generic ...))))
                      (syntax->list #'(method-id  (... ...))))
              #'(make-generic-instance/c
                 (quote #,(syntax-e #'name/c))
                 name?
                 accessor
                 (list 'method-id (... ...))
                 (list ctc (... ...))
                 (make-immutable-hash
                  (list (cons 'generic 'generic-idx) ...)))])))]))

;; make a generic instance contract
(define (make-generic-instance/c name name? accessor ids ctc-args method-map)
  (define ctcs (coerce-contracts 'generic-instance/c ctc-args))
  ;; map method table indices to ids & projections
  (define id+ctc-map
    (for/hash ([id ids] [ctc ctcs])
      (values (hash-ref method-map id)
              (cons id (contract-projection ctc)))))
  (cond [(andmap chaperone-contract? ctcs)
         (chaperone-generic-instance/c
          name name? ids ctcs accessor id+ctc-map method-map)]
        [else
         (impersonator-generic-instance/c
          name name? ids ctcs accessor id+ctc-map method-map)]))

(define (generic-instance/c-name ctc)
  (define method-names
    (map (λ (id ctc) (build-compound-type-name id ctc))
         (base-generic-instance/c-ids ctc)
         (base-generic-instance/c-ctcs ctc)))
  (apply build-compound-type-name
         (cons (base-generic-instance/c-name ctc) method-names)))

;; redirect for use with chaperone-vector
(define ((method-table-redirect ctc blame) vec idx val)
  (cond
    [(eq? val #f) #f]
    [else
     (define id+ctc-map (base-generic-instance/c-id+ctc-map ctc))
     (define maybe-id+ctc (hash-ref id+ctc-map idx #f))
     (cond
       [maybe-id+ctc
        (define id (car maybe-id+ctc))
        (define proj (cdr maybe-id+ctc))
        (define blame-string (format "the ~a method of" id))
        ((proj (blame-add-context blame blame-string)) val)]
       [else val])]))

;; projection for generic methods
(define ((generic-instance/c-proj proxy-struct proxy-vector) ctc)
  (λ (blame)
    ;; for redirecting the method table accessor
    (define (redirect struct v)
      (proxy-vector
       v
       (method-table-redirect ctc blame)
       (λ (vec i v) v)))
    (λ (val)
      (unless (contract-first-order-passes? ctc val)
        (raise-blame-error
         blame val
         '(expected: "~s" given: "~e")
         (contract-name ctc)
         val))
      (define accessor (base-generic-instance/c-accessor ctc))
      (proxy-struct val accessor redirect))))

;; recognizes instances of this generic interface
(define ((generic-instance/c-first-order ctc) v)
  (cond [((base-generic-instance/c-name? ctc) v)
         (define accessor (base-generic-instance/c-accessor ctc))
         (define method-table (accessor v))
         (define ids (base-generic-instance/c-ids ctc))
         (define ctcs (base-generic-instance/c-ctcs ctc))
         (define method-map (base-generic-instance/c-method-map ctc))
         ;; do sub-contract first-order checks
         (for/and ([id ids] [ctc ctcs])
           (define v (vector-ref method-table (hash-ref method-map id)))
           (or (eq? v #f)
               (contract-first-order-passes? ctc v)))]
        [else #f]))

;; name        - for building ctc name
;; name?       - for first-order checks
;; ids         - for method names (used to build the ctc name)
;; ctcs        - for the contract name
;; accessor    - for chaperoning the struct type property
;; id+ctc-map  - for chaperoning the method table vector
;; method-map  - for first-order checks
(struct base-generic-instance/c
  (name name? ids ctcs accessor id+ctc-map method-map))

(struct chaperone-generic-instance/c base-generic-instance/c ()
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:projection (generic-instance/c-proj chaperone-struct chaperone-vector)
   #:first-order generic-instance/c-first-order
   #:name generic-instance/c-name))

(struct impersonator-generic-instance/c base-generic-instance/c ()
  #:property prop:contract
  (build-contract-property
   #:projection (generic-instance/c-proj impersonate-struct impersonate-vector)
   #:first-order generic-instance/c-first-order
   #:name generic-instance/c-name))

#lang racket/base
(require racket/local
         (for-syntax racket/base
                     racket/local
                     racket/syntax
                     syntax/stx
                     syntax/boundmap)
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
      (wrong-syntax stx "expected an identifier")))

  (define (free-id-table) (make-free-identifier-mapping))
  (define (free-id-ref table id default)
    (free-identifier-mapping-get table id (lambda () default)))
  (define (free-id-set! table id value)
    (free-identifier-mapping-put! table id value))

  (define (parse-extensions meths exts0)
    (define ext-table (free-id-table))
    (define infos
      (let loop ([exts exts0] [tail '()])
        (cond
          [(null? exts) tail]
          [(free-id-ref ext-table (car exts) #f)
           (wrong-syntax (car exts) "duplicate extension of generics group")]
          [else
           (define ext (car exts))
           (define info
             (and (identifier? ext)
                  (syntax-local-value ext (lambda () #f))))
           (unless (generic-info? info)
             (wrong-syntax ext "expected a generics group name"))
           (free-id-set! ext-table ext #t)
           (cons info
                 (loop (generic-info-supers info)
                       (loop (cdr exts) tail)))])))
    (define super-table (free-id-table))
    (define-values (props super-lists)
      (for/lists (props super-lists) ([info (in-list infos)])
        (values (generic-info-property info)
                (map syntax-local-introduce
                     (generic-info-methods info)))))
    (define count (length meths))
    (define supers
      (for*/list ([supers (in-list super-lists)]
                  [super (in-list supers)]
                  #:unless (free-id-ref super-table super #f))
        (define index count)
        (set! count (add1 index))
        (free-id-set! super-table super index)
        super))
    (for ([meth (in-list meths)])
      (when (free-id-ref super-table meth #f)
        (wrong-syntax meth
                      "cannot redefine method from extended generics group")))
    (define super-indices
      (for/list ([supers (in-list super-lists)])
        (for/list ([super (in-list supers)])
          (free-id-ref super-table super #f))))
    (values supers props super-indices)))

(define-syntax (define-primitive-generics/derived stx)
  (syntax-case stx ()
    [(_ #:define-generic generic-name
        #:define-predicate predicate-name
        #:define-property property-name
        #:define-accessor accessor-name
        #:define-supported supported-name
        #:define-methods [(method-name . method-signature) ...]
        #:given-self self-name
        #:given-extensions [extension-name ...]
        #:given-defaults ([default-pred default-defn ...] ...)
        #:given-fallbacks [fallback-defn ...]
        #:given-source original)
     (parameterize ([current-syntax-context #'original])
       (check-identifier! #'generic-name)
       (check-identifier! #'predicate-name)
       (check-identifier! #'property-name)
       (check-identifier! #'accessor-name)
       (check-identifier! #'supported-name)
       (check-identifier! #'self-name)
       (define methods (syntax->list #'(method-name ...)))
       (define extensions (syntax->list #'(extension-name ...)))
       (for-each check-identifier! methods)
       (for-each check-identifier! extensions)
       (define-values {supers super-props super-indices}
         (parse-extensions methods extensions))
       (define n-methods (length methods))
       (define n-supers (length supers))
       (define n (+ n-methods n-supers))
       (define method-indices (for/list ([i (in-range n-methods)]) i))
       (define/with-syntax size n)
       (define/with-syntax [super-name ...] supers)
       (define/with-syntax [method-index ...] method-indices)
       (define/with-syntax [super-prop ...] super-props)
       (define/with-syntax [[super-index ...] ...] super-indices)
       (define/with-syntax contract-str
         (format "~s" (syntax-e #'predicate-name)))
       (define/with-syntax (default-pred-name ...)
         (generate-temporaries #'(default-pred ...)))
       (define/with-syntax (default-impl-name ...)
         (generate-temporaries #'(default-pred ...)))
       #'(begin
           (define-syntax generic-name
             (make-generic-info (quote-syntax property-name)
                                (list (quote-syntax extension-name) ...)
                                (list (quote-syntax method-name) ...
                                      (quote-syntax super-name) ...)))
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
               #:given-impl (vector-ref x 'method-index)
               #:given-source original)
             ...
             x)
           (define-values (property-name prop:pred accessor-name)
             (make-struct-type-property 'generic-name prop:guard
               (list (cons super-prop
                           (lambda (x)
                             (vector (vector-ref x 'super-index) ...)))
                     ...)
               #t))
           (define (predicate-name self-name)
             (or (prop:pred self-name) (default-pred-name self-name) ...))
           (define (table-name self-name [who 'table-name])
             (cond
               [(prop:pred self-name) (accessor-name self-name)]
               [(default-pred-name self-name) default-impl-name]
               ...
               [else (raise-argument-error who 'contract-str self-name)]))
           (define-generic-support
             #:define-supported supported-name
             #:given-self self-name
             #:given-methods [method-name ...]
             #:given-table (table-name self-name 'supported-name)
             #:given-source original)
           (define-generic-method
             #:define-method method-name
             #:given-signature method-signature
             #:given-self self-name
             #:given-proc
             (or (vector-ref (table-name self-name 'method-name) 'method-index)
                 (vector-ref fallback-name 'method-index))
             #:given-source original)
           ...
           (define-values (default-pred-name ...)
             (values default-pred ...))
           (define fallback-name
             (generic-method-table
               generic-name
               fallback-defn
               ...
               (ensure-unimplemented
                 "cannot define fallbacks for inherited methods"
                 super-name
                 ...)))
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
        #:given-extensions [ext-name ...]
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
         #:given-extensions [ext-name ...]
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
                      "did not find ~a among ~a to ~s"
                      "the generic name"
                      "the required, by-position arguments"
                      (syntax-e name-stx)))
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
            #'(apply f kw ... ... arg ... rest))
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

    (unless (procedure? v)
      (define msg "generic method definition is not a function")
      (raise-arguments-error who msg (format "~s" what) v))

    (define (arity-error why)
      (define msg
        (format "generic method definition has an incorrect arity; ~a" why))
      (raise-arguments-error who msg (format "~s" what) v))

    (define arity (procedure-arity v))

    (cond
      [rest?
       (unless (arity-includes? arity (arity-at-least req-n))
         (arity-error
           (format "expected a procedure that accepts ~a or more arguments"
                   req-n)))]
      [(zero? opt-n)
       (unless (arity-includes? arity req-n)
         (arity-error (format "expected a procedure that accepts ~a ~a"
                              req-n
                              (if (= 1 req-n) "argument" "arguments"))))]
      [else
       (for ([i (in-range req-n (+ req-n opt-n 1))])
         (unless (arity-includes? arity i)
           (arity-error (format "~a ~a required ~a and up to ~a optional ~a"
                                "expected a procedure that accepts"
                                req-n
                                (if (= 1 req-n) "argument" "arguments")
                                opt-n
                                (if (= 1 opt-n) "argument" "arguments")))))])

    (define-values (v-req-kws v-opt-kws) (procedure-keywords v))

    (define (keyword-subset? xs ys)
      (cond
        [(null? xs) #t]
        [(null? ys) #f]
        [else
         (define x (car xs))
         (define y (car ys))
         (cond
           [(keyword<? x y) #f]
           [(keyword<? y x) (keyword-subset? xs (cdr ys))]
           [else (keyword-subset? (cdr xs) (cdr ys))])]))

    (unless (and (keyword-subset? v-req-kws req-kws)
                 (or (not v-opt-kws)
                     (and (keyword-subset? req-kws v-opt-kws)
                          (keyword-subset? opt-kws v-opt-kws))))
      (define r (keywords-message #t req-kws))
      (define o (keywords-message #f opt-kws))
      (arity-error (format "expected a procedure that accepts ~a~a" r o)))))

(define (keywords-message required? kws)
  (cond
    [(null? kws) (if required? "no required keyword arguments" "")]
    [(null? (cdr kws))
     (format "~athe ~a keyword argument ~s"
             (if required? "" " and ")
             (if required? "required" "optional")
             (car kws))]
    [(null? (cddr kws))
     (format "~athe ~a keyword arguments ~s and ~s"
             (if required? "" " and ")
             (if required? "required" "optional")
             (car kws)
             (cadr kws))]
    [else
     (define strs
       (let loop ([kws kws])
         (cond
           [(null? (cdr kws)) (list (format "and ~s" (car kws)))]
           [else (cons (format "~s, " (car kws)) (loop (cdr kws)))])))
     (format "~athe ~a keyword arguments ~a"
             (if required? "" " and ")
             (if required? "required" "optional")
             (apply string-append strs))]))

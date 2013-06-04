(module generic-methods '#%kernel

  (#%require (for-syntax '#%kernel "small-scheme.rkt" "define.rkt"
                         "stx.rkt" "stxcase-scheme.rkt")
             "define.rkt" "../stxparam.rkt")

  (#%provide define/generic generic-property
             generic-method-table generic-method-ref
             (for-syntax generic-info? make-generic-info
                         generic-info-property generic-info-methods))

  (begin-for-syntax

    (define-values (struct:generic-info
                    make-generic-info
                    generic-info?
                    generic-info-get
                    generic-info-set!)
      (make-struct-type 'generic-info #f 2 0))

    (define-values (generic-info-property generic-info-methods)
      (values (make-struct-field-accessor generic-info-get 0 'property)
              (make-struct-field-accessor generic-info-get 1 'methods)))

    (define (check-identifier! name ctx stx)
      (unless (identifier? stx)
        (raise-syntax-error name "expected an identifier" ctx stx)))

    (define (get-info name ctx stx)
      (check-identifier! name ctx stx)
      (define info (syntax-local-value stx (lambda () #f)))
      (unless (generic-info? info)
        (raise-syntax-error name "bad generics group name" ctx stx))
      info)

    (define (get-method name ctx gen-id info method-id)
      (let loop ([methods (generic-info-methods info)]
                 [index 0])
        (cond
          [(null? methods)
           (define message
             (format "~.s is not a method of ~.s"
                     (syntax-e method-id)
                     (syntax-e gen-id)))
           (raise-syntax-error 'name message ctx method-id)]
          [(free-identifier=? (car methods) method-id)
           (values (car methods) index)]
          [else (loop (cdr methods) (add1 index))]))))

  (define-syntax-parameter generic-method-context #f)

  (define-syntax (unimplemented stx)
    (raise-syntax-error #f "unimplemented generic method" stx))

  (define-syntax (implementation stx)
    (syntax-case stx (unimplemented)
      [(_ unimplemented) #'(quote #f)]
      [(_ expr) #'expr]))

  (define-syntax (generic-property stx)
    (syntax-case stx ()
      [(_ gen)
       (generic-info-property (get-info 'generic-property stx #'gen))]))

  (define-syntax (generic-method-ref stx)
    (syntax-case stx ()
      [(_ gen table method)
       (let ()
         (define info (get-info 'generic-method-ref stx #'gen))
         (check-identifier! 'generic-method-ref stx #'method)
         (define-values (index impl-id) (get-method info #'method))
         (with-syntax ([i index])
           (syntax/loc stx
             (vector-ref table 'i))))]))

  (define-syntax (generic-method-table stx)
    (syntax-case stx ()
      [(_ gen def ...)
       (let ()
         (define info (get-info 'generic-method-table stx #'gen))
         (define delta (syntax-local-make-delta-introducer #'gen))
         (define methods (map delta (generic-info-methods info)))
         (with-syntax ([(method ...) methods])
           (syntax/loc stx
             (syntax-parameterize ([generic-method-context #'gen])
               (letrec-syntaxes+values
                   ([(method) (make-rename-transformer #'unimplemented)] ...)
                   ()
                 def ...
                 (vector (implementation method) ...))))))]))

  (define-syntax (define/generic stx)
    (define gen-id (syntax-parameter-value #'generic-method-context))
    (define gen-val
      (and (identifier? gen-id)
           (syntax-local-value gen-id (lambda () #f))))
    (unless (generic-info? gen-val)
      (raise-syntax-error 'define/generic "only allowed inside methods" stx))
    (syntax-case stx ()
      [(_ name method)
       (let ()
         (check-identifier! 'define/generic stx #'name)
         (check-identifier! 'define/generic stx #'method)
         (define-values (index impl-id) (get-method gen-val #'method))
         (with-syntax ([impl ((make-syntax-introducer) impl-id)])
           (syntax/loc stx
             (define name impl))))])))

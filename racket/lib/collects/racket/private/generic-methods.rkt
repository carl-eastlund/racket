(module generic-methods '#%kernel

  (#%require (for-syntax '#%kernel))

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
      (define-values pair
        (for/first ([index (in-naturals 0)]
                    [impl-id (in-list (generic-info-methods info))]
                    #:when (free-identifier=? impl-id method-id))
          (cons index impl-id)))
      (unless pair
        (define message
          (format "~.s is not a method of ~.s"
                  (syntax-e method-id)
                  (syntax-e gen-id)))
        (raise-syntax-error 'name message ctx method-id))
      (values (car pair) (cdr pair))))

  (define-values [struct:method-table
                  make-method-table
                  method-table?
                  method-table-get
                  method-table-set!]
    (make-struct-type 'method-table #f 1 0))

  (define method-table-vector
    (make-struct-field-accessor method-table-get 0 'vector))

  (define-syntax-parameter generic-method-context #f)

  (define-syntax (unimplemented stx)
    (raise-syntax #f "unimplemented generic method" stx))

  (define-syntax (implementation stx)
    (syntax-case stx (unimplemented)
      [(_ unimplemented) #'(quote #f)]
      [(_ expr) #'expr]))

  (define-syntax (generic-property stx)
    (syntax-case stx ()
      [(_ gen)
       (define info (get-info 'generic-property stx #'gen))
       (generic-info-property info)]))

  (define-syntax (generic-method-ref stx)
    (syntax-case stx ()
      [(_ gen table method)
       (define info (get-info 'generic-method-ref stx #'gen))
       (check-identifier! 'generic-method-ref stx #'method)
       (define methods (generic-info-methods info))
       (define-values (index impl-id) (get-method info #'method))
       (with-syntax ([i index])
         (syntax/loc stx
           (vector-ref (method-table-vector table) 'i)))]))

  (define-syntax (generic-method-table stx)
    (syntax-case stx ()
      [(_ gen def ...)
       (define info (get-info 'generic-method-table stx #'gen))
       (define delta (syntax-local-make-delta-introducer #'gen))
       (define methods (map delta (generic-info-methods info)))
       (with-syntax ([(method ...) methods])
         (syntax/loc stx
           (syntax-parameterize ([generic-method-context #'gen])
             (let-syntax ([method (rename-transformer #'unimplemented)] ...)
               def ...
               (make-method-table (vector (implementation method) ...))))))]))

  (define-syntax (define/generic stx)
    (define gen-id (syntax-parameter-value #'generic-method-context))
    (define gen-val
      (and (identifier? gen-id)
           (syntax-local-value gen-id (lambda () #f))))
    (unless (generic-info? gen-val)
      (raise-syntax-error 'define/generic "only allowed inside methods" stx))
    (define delta (syntax-local-make-delta-introducer gen-id))
    (define methods (map delta (generic-info-methods gen-val)))
    (syntax-case stx ()
      [(_ name method)
       (check-identifier! 'define/generic stx #'name)
       (check-identifier! 'define/generic stx #'method)
       (define-values (index impl-id) (get-method gen-val #'method))
       (with-syntax ([impl ((make-syntax-introducer) impl-id)])
         (syntax/loc stx
           (define name impl)))])))

(module generic-methods '#%kernel

  (#%require (for-syntax '#%kernel "small-scheme.rkt" "define.rkt"
                         "stx.rkt" "stxcase-scheme.rkt")
             "define.rkt" "../stxparam.rkt")

  (#%provide define/generic generic-property generic-method-table
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
      info))

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
      [(_ bind ref)
       (let ()
         (unless (identifier? #'bind)
           (raise-syntax-error 'define/generic "expected an identifier" #'bind))
         (unless (identifier? #'ref)
           (raise-syntax-error 'define/generic "expected an identifier" #'ref))
         (define methods
           (map syntax-local-get-shadower
                (map (syntax-local-make-delta-introducer gen-id)
                     (generic-info-methods gen-val))))
         (define matches
           (let loop ([methods methods])
             (cond
               [(null? methods) '()]
               [(free-identifier=? (car methods) #'ref)
                (cons (car methods) (loop (cdr methods)))]
               [else
                (loop (cdr methods))])))
         (unless (pair? matches)
           (raise-syntax-error 'define/generic
                               (format "~.s is not a method of ~.s"
                                       (syntax-e #'ref)
                                       (syntax-e gen-id))
                               stx
                               #'ref))
         (when (pair? (cdr matches))
           (raise-syntax-error 'define/generic
                               (format "multiple methods match ~.s: ~.s"
                                       (syntax-e #'ref)
                                       (map syntax-e matches))
                               stx
                               #'ref))
         (with-syntax ([method (car matches)])
           #'(define bind method)))])))

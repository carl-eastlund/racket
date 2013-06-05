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
         (define i1 syntax-local-introduce)
         (define d1 (syntax-local-make-delta-introducer gen-id))
         (define s1 syntax-local-get-shadower)
         (define (i stxs) (map i1 stxs))
         (define (d stxs) (map d1 stxs))
         (define (s stxs) (map s1 stxs))
         (define ms (generic-info-methods gen-val))
         (with-syntax ([(m ...) ms]
                       [(im ...) (i ms)]
                       [(dm ...) (d ms)]
                       [(sm ...) (s ms)]
                       [(idm ...) (d (i ms))]
                       [(ism ...) (s (i ms))]
                       [(dim ...) (i (d ms))]
                       [(dsm ...) (s (d ms))]
                       [(sim ...) (i (s ms))]
                       [(sdm ...) (d (s ms))]
                       [(idsm ...) (s (d (i ms)))]
                       [(isdm ...) (d (s (i ms)))]
                       [(dism ...) (s (i (d ms)))]
                       [(dsim ...) (i (s (d ms)))]
                       [(sidm ...) (d (i (s ms)))]
                       [(sdim ...) (i (d (s ms)))])
           #'(define/generic-from bind ref
               ["m" m ...]
               ["im" im ...]
               ["dm" dm ...]
               ["sm" sm ...]
               ["idm" idm ...]
               ["ism" ism ...]
               ["dim" dim ...]
               ["dsm" dsm ...]
               ["sim" sim ...]
               ["sdm" sdm ...]
               ["idsm" idsm ...]
               ["isdm" isdm ...]
               ["dism" dism ...]
               ["dsim" dsim ...]
               ["sidm" sidm ...]
               ["sdim" sdim ...])))]))

  (define-syntax (define/generic-from stx)
    (define source-ids
      (let loop ([stx (stx-cdr stx)] [ids '()])
        (syntax-case stx ()
          [(a . b) (loop #'a (loop #'b ids))]
          [i (identifier? #'i) (cons #'i ids)]
          [_ ids])))
    (define all-ids
      (append source-ids (map syntax-local-introduce source-ids)))
    (define bound-pairs
      (let loop ([xs all-ids] [ys all-ids] [pairs '()])
        (cond
          [(null? xs) (reverse pairs)]
          [(null? ys) (loop (cdr xs) (cdr xs) pairs)]
          [else
           (define x (car xs))
           (define y (car xs))
           (if (and (bound-identifier=? x y)
                    (not (free-identifier=? x y)))
               (loop xs (cdr ys) (cons (list x y) pairs))
               (loop xs (cdr ys) pairs))])))
    (define free-pairs
      (let loop1 ([xs all-ids] [pairs '()])
        (cond
          [(null? xs) (reverse pairs)]
          [else
           (let loop2 ([x (car xs)] [pairs pairs])
             (define-values (value target)
               (syntax-local-value/immediate x
                 (lambda () (values #f #f))))
             (cond
               [(identifier? target)
                (if (free-identifier=? x target)
                    (loop2 target pairs)
                    (loop2 target (cons (list x target) pairs)))]
               [else (loop1 (cdr xs) pairs)]))])))
    (with-syntax ([([a b] ...) bound-pairs]
                  [([c d] ...) free-pairs])
      #'(bad-ids (#:bound-but-not-free [a b] ...)
                 (#:rename-but-not-free [c d] ...))))

  (define-syntax (bad-ids stx)
    (raise-syntax-error 'bad-ids "stop here!" stx)))

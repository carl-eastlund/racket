(module base "pre-base.rkt"

  (#%require "hash.rkt"
             "list.rkt" ; shadows `reverse', `mem{q,v,ber}'
             "string.rkt"
             "stxcase-scheme.rkt"
             "qqstx.rkt"
             "stx.rkt"
             "kw-file.rkt"
             "namespace.rkt"
             "struct.rkt"
             "cert.rkt"
             "submodule.rkt"
             "generic-interfaces.rkt"
             (for-syntax "stxcase-scheme.rkt"))

  (define (step-identifier id ctx proc-exn proc-value proc-syntax proc-rename)
    (define-values (value target)
      (with-handlers ([exn:fail? (lambda (e) (values #f 'error))])
        (syntax-local-value/immediate id (lambda () (values #f 'value)) ctx)))
    (cond
      [(identifier? target)
       (unless (or (syntax-property target 'not-free-identifier=?)
                 (free-identifier=? id target))
         (eprintf "~a:\n  ~e\n  ~e\n"
           "rename transformer but not free-identifier=?"
           id
           target))
       (proc-rename value target)]
      [(eq? target 'value) (proc-value)]
      [(eq? target 'error) (proc-exn)]
      [else (proc-syntax value)]))

  (define (chase-identifier id ctx proc-exn proc-value proc-syntax)
    (let loop ([id id])
      (step-identifier id ctx
        (lambda () (proc-exn))
        (lambda () (proc-value id))
        (lambda (value) (proc-syntax id value))
        (lambda (value target) (loop target)))))

  (define (syntax-local-value/immediate/validate id [fail #f] [ctx #f])
    (step-identifier id ctx
      (lambda ()
        (syntax-local-value/immediate id fail ctx))
      (lambda ()
        (cond
          [fail (fail)]
          [else (raise-syntax-error 'syntax-local-value/immediate
                  (format "not defined as syntax\n  identifier: ~e" id)
                  id)]))
      (lambda (value) (values value #f))
      (lambda (value target) (values value target))))

  (define (syntax-local-value/validate id [fail #f] [ctx #f])
    (chase-identifier id ctx
      (lambda ()
        (syntax-local-value id fail ctx))
      (lambda (id)
        (cond
          [fail (fail)]
          [else (raise-syntax-error 'syntax-local-value/immediate
                  (format "not defined as syntax\n  identifier: ~e" id)
                  id)]))
      (lambda (id value) value)))

  (define (chase-ok? phase)
    (and (syntax-transforming?)
      (equal? phase (syntax-local-phase-level))))

  (define (identifier-binding/validate id [phase (syntax-local-phase-level)])
    (cond
      [(chase-ok? phase)
       (chase-identifier id #f
         (lambda () (identifier-binding id phase))
         (lambda (id) (identifier-binding id phase))
         (lambda (id value) (identifier-binding id phase)))]
      [else (identifier-binding id phase)]))

  (define (identifier=?/validate a b phase)
    (define free=?
      (cond
        [(chase-ok? phase)
         (free-identifier=?
           (chase-identifier a #f
             (lambda () a)
             (lambda (id) id)
             (lambda (id value) id))
           (chase-identifier b #f
             (lambda () b)
             (lambda (id) id)
             (lambda (id value) id))
           phase)]
        [else (free-identifier=? a b phase)]))
    (define bound=?
      (bound-identifier=? a b phase))
    (when bound=?
      (unless free=?
        (eprintf "~a at phase ~a:\n  ~e\n  ~e\n"
          "bound-identifier=? but not free-identifier=?"
          phase
          a
          b)))
    (values free=? bound=?))

  (define (bound-identifier=?/validate a b [phase (syntax-local-phase-level)])
    (define-values (free=? bound=?)
      (identifier=?/validate a b phase))
    bound=?)

  (define (free-identifier=?/validate a b
            [a-phase (syntax-local-phase-level)]
            [b-phase a-phase])
    (cond
      [(equal? a-phase b-phase)
       (define-values (free=? bound=?)
         (identifier=?/validate a b a-phase))
       free=?]
      [else (free-identifier=? a b a-phase b-phase)]))

  (define (free-transformer-identifier=?/validate a b)
    (free-identifier=?/validate a b (add1 (syntax-local-phase-level))))
  (define (free-template-identifier=?/validate a b)
    (free-identifier=?/validate a b (sub1 (syntax-local-phase-level))))
  (define (free-label-identifier=?/validate a b)
    (free-identifier=?/validate a b #f))

  (#%provide (all-from-except "pre-base.rkt"
                              open-input-file
                              open-output-file
                              open-input-output-file
                              call-with-input-file
                              call-with-output-file
                              with-input-from-file
                              with-output-to-file
                              directory-list
                              regexp-replace*
                              new-apply-proc
                              syntax-local-value
                              syntax-local-value/immediate
                              identifier-binding
                              free-identifier=?
                              free-transformer-identifier=?
                              free-template-identifier=?
                              free-label-identifier=?
                              bound-identifier=?)
             (rename syntax-local-value/validate
                     syntax-local-value)
             (rename syntax-local-value/immediate/validate
                     syntax-local-value/immediate)
             (rename identifier-binding/validate
                     identifier-binding)
             (rename free-identifier=?/validate
                     free-identifier=?)
             (rename free-transformer-identifier=?/validate
                     free-transformer-identifier=?)
             (rename free-template-identifier=?/validate
                     free-template-identifier=?)
             (rename free-label-identifier=?/validate
                     free-label-identifier=?)
             (rename bound-identifier=?/validate
                     bound-identifier=?)
             struct
             (all-from "hash.rkt")
             (all-from "list.rkt")
             (all-from-except "string.rkt" 
                              -regexp-replace*)
             (rename -regexp-replace* regexp-replace*)
             identifier?
             (all-from-except "stxcase-scheme.rkt" datum datum-case with-datum)
             (all-from-except "qqstx.rkt" quasidatum undatum undatum-splicing)
             (all-from "namespace.rkt")
             (all-from "cert.rkt")
             (all-from "submodule.rkt")
             (all-from "generic-interfaces.rkt")
             (for-syntax syntax-rules syntax-id-rules ... _)
             (rename -open-input-file open-input-file)
             (rename -open-output-file open-output-file)
             (rename -open-input-output-file open-input-output-file)
             (rename -call-with-input-file call-with-input-file)
             (rename -call-with-output-file call-with-output-file)
             (rename -with-input-from-file with-input-from-file)
             (rename -with-output-to-file with-output-to-file)
             (rename -directory-list directory-list)
             call-with-input-file*
             call-with-output-file*))

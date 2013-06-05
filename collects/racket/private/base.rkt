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

  (define unique (gensym))

  (define (syntax-local-value/immediate/validate
            id
            [fail #f]
            [ctx #f])
    (define (fail/unique) (values #f unique))
    (define-values (value target)
      (syntax-local-value/immediate id fail/unique ctx))
    (cond
      [(eq? target unique)
       (cond
         [fail (fail)]
         [else (raise-syntax-error 'syntax-local-value/immediate
                 (format "not defined as syntax\n  identifier: ~e" id)
                 id)])]
      [(identifier? target)
       (unless (or (syntax-property target 'not-free-identifier=?)
                   (free-identifier=? id target))
         (eprintf "~a:\n  ~e\n  ~e\n"
           "rename transformer but not free-identifier=?"
           id
           target))
       (values value target)]
      [else
       (values value target)]))

  (define (syntax-local-value/validate id0 [fail #f] [ctx #f])
    (define (fail/immediate) (values #f unique))
    (let loop ([id id0])
      (define-values (value target)
        (syntax-local-value/immediate/validate id fail/immediate ctx))
      (cond
        [(eq? target unique)
         (cond
           [fail (fail)]
           [else (raise-syntax-error 'syntax-local-value
                   (format "not defined as syntax\n  identifier: ~e" id0)
                   id0)])]
        [(identifier? target) (loop target)]
        [else value])))

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
                              syntax-local-value/immediate)
             (rename syntax-local-value/validate syntax-local-value)
             (rename syntax-local-value/immediate/validate
                     syntax-local-value/immediate)
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

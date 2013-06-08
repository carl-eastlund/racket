#lang racket/base
(require (for-syntax racket/base
                     syntax/for-body)
         racket/serialize
         racket/pretty
         racket/contract/base
         racket/contract/combinator
         racket/generic
         racket/stream
         (only-in "private/for.rkt" prop:stream))

(provide

  gen:set set?

  set-count
  set-member?
  set->stream
  set-add
  set-remove
  set-clear
  set-add!
  set-remove!
  set-clear!

  set-clear-supported?
  set-add-supported?
  set-remove-supported?
  set-clear!-supported?
  set-add!-supported?
  set-remove!-supported?

  set-empty?
  set-first
  set-rest
  set-copy
  set-map
  set-for-each
  set->list
  set=?
  subset?
  proper-subset?
  set-union
  set-intersect
  set-subtract
  set-symmetric-difference
  simple-set-union
  simple-set-intersect
  simple-set-subtract
  simple-set-symmetric-difference
  set-union!
  set-intersect!
  set-subtract!
  set-symmetric-difference!
  simple-set-union!
  simple-set-intersect!
  simple-set-subtract!
  simple-set-symmetric-difference!

  in-set
  (rename-out [make-set-contract set/c])

  set-equal?
  set list->set for/set for*/set
  mutable-set list->mutable-set for/mutable-set for*/mutable-set
  weak-set list->weak-set for/weak-set for*/weak-set

  set-eq?
  seteq list->seteq for/seteq for*/seteq
  mutable-seteq list->mutable-seteq for/mutable-seteq for*/mutable-seteq
  weak-seteq list->weak-seteq for/weak-seteq for*/weak-seteq

  set-eqv?
  seteqv list->seteqv for/seteqv for*/seteqv
  mutable-seteqv list->mutable-seteqv for/mutable-seteqv for*/mutable-seteqv
  weak-seteqv list->weak-seteqv for/weak-seteqv for*/weak-seteqv

  make-custom-set
  make-custom-mutable-set
  make-custom-weak-set)

(define (supported? set sym)
  (cond
    [(list? set) #f]
    [else (hash-ref (set-supported set) sym)]))

(define (set-clear-supported? s)
  (or (list? s) (supported? 'set-clear)))

(define (set-add-supported? s)
  (or (list? s) (supported? s 'set-add)))

(define (set-remove-supported? s)
  (or (list? s) (supported? s 'set-remove)))

(define (set-clear!-supported? s)
  (and (not (list? s)) (supported? 'set-clear!)))

(define (set-add!-supported? s)
  (and (not (list? s)) (supported? s 'set-add!)))

(define (set-remove!-supported? s)
  (and (not (list? s)) (supported? s 'set-remove!)))

(define (set-empty?-fallback s)
  (stream-empty? (set->stream s)))

(define (set-first-fallback s)
  (stream-first (set->stream s)))

(define (set-rest-fallback s)
  (unless (set-remove-supported? s)
    (raise-argument-error 'set-rest "(and/c set? set-remove-supported?)" 0 s))
  (set-remove s (set-first s)))

(define (set-for-each-fallback set proc)
  (unless (and (procedure? proc)
               (procedure-arity-includes? proc 1))
    (raise-argument-error 'set-for-each "(any/c . -> . any/c)" 1 set proc))
  (for ([v (in-set set)])
    (proc v)))

(define (set-map-fallback set proc)
  (unless (and (procedure? proc)
               (procedure-arity-includes? proc 1))
    (raise-argument-error 'set-map "(any/c . -> . any/c)" 1 set proc))
  (for/list ([v (in-set set)])
    (proc v)))

(define (set->list-fallback set)
  (for/list ([v (in-set set)])
    v))

(define (subset?-fallback one two)
  (unless (set? two) (raise-argument-error 'subset? "set?" 1 one two))
  (for/and ([v (in-set one)])
    (set-member? two v)))

(define (set=?-fallback one two)
  (unless (set? two) (raise-argument-error 'set=? "set?" 1 one two))
  (and (subset? one two) (subset? two one)))

(define (proper-subset?-fallback one two)
  (unless (set? two) (raise-argument-error 'proper-subset? "set?" 1 one two))
  (and (subset? one two) (not (subset? two one))))

(define (set-copy-fallback s)
  (unless (and (set-clear-supported? s)
               (set-add!-supported? s))
    (define str "(and/c set? set-clear-supported? set-add!-supported?)")
    (raise-argument-error 'set-copy str 0 s))
  (define s2 (set-clear s))
  (for ([v (in-set s)])
    (set-add! s2 v))
  s2)

(define (simple-set-union-fallback one two)
  (unless (set-add-supported? one)
    (define str "(and/c set? set-add-supported?)")
    (raise-argument-error 'set-union str 0 one two))
  (unless (set? two) (raise-argument-error 'set-union "set?" 1 one two))
  (for/fold ([s one]) ([v (in-set two)])
    (set-add s v)))

(define set-union
  (case-lambda
    [(s)
     (unless (set? s) (raise-argument-error 'set-union "set?" 0 s))
     s]
    [(one two)
     (unless (set? one) (raise-argument-error 'set-union? "set?" 0 one two))
     (unless (set? two) (raise-argument-error 'set-union? "set?" 1 one two))
     (simple-set-union one two)]
    [(s . sets)
     (unless (set? s) (apply raise-argument-error 'set-union "set?" 0 s sets))
     (for/fold ([s1 s]) ([s2 (in-list sets)] [i (in-naturals 1)])
       (unless (set? s2)
         (apply raise-argument-error 'set-union "set?" i s sets))
       (simple-set-union s1 s2))]))

(define (simple-set-union!-fallback one two)
  (unless (set-add!-supported? one)
    (define str "(and/c set? set-add!-supported?)")
    (raise-argument-error 'set-union! str 0 one two))
  (unless (set? two) (raise-argument-error 'set-union! "set?" 1 one two))
  (for ([v (in-set two)])
    (set-add! one v)))

(define set-union!
  (case-lambda
    [(s)
     (unless (set? s) (raise-argument-error 'set-union! "set?" 0 s))
     s]
    [(one two)
     (unless (set? one) (raise-argument-error 'set-union!? "set?" 0 one two))
     (unless (set? two) (raise-argument-error 'set-union!? "set?" 1 one two))
     (simple-set-union! one two)]
    [(s . sets)
     (unless (set? s) (apply raise-argument-error 'set-union! "set?" 0 s sets))
     (for ([s2 (in-list sets)] [i (in-naturals 1)])
       (unless (set? s2)
         (apply raise-argument-error 'set-union! "set?" i s sets))
       (simple-set-union! s s2))]))

(define (simple-set-intersect-fallback one two)
  (unless (set-remove-supported? one)
    (define str "(and/c set? set-remove-supported?)")
    (raise-argument-error 'set-intersect str 0 one two))
  (unless (set? two) (raise-argument-error 'set-intersect "set?" 1 one two))
  (for/fold ([s one]) ([v (in-set one)])
    (if (set-member? two v)
        s
        (set-remove s v))))

(define set-intersect
  (case-lambda
    [(s)
     (unless (set? s) (raise-argument-error 'set-intersect "set?" 0 s))
     s]
    [(one two)
     (unless (set? one) (raise-argument-error 'set-intersect? "set?" 0 one two))
     (unless (set? two) (raise-argument-error 'set-intersect? "set?" 1 one two))
     (simple-set-intersect one two)]
    [(s . sets)
     (unless (set? s)
       (apply raise-argument-error 'set-intersect "set?" 0 s sets))
     (for/fold ([s1 s]) ([s2 (in-list sets)] [i (in-naturals 1)])
       (unless (set? s2)
         (apply raise-argument-error 'set-intersect "set?" i s sets))
       (simple-set-intersect s1 s2))]))

(define (simple-set-intersect!-fallback one two)
  (unless (set-remove!-supported? one)
    (define str "(and/c set? set-remove!-supported?)")
    (raise-argument-error 'set-intersect! str 0 one two))
  (unless (set? two) (raise-argument-error 'set-intersect! "set?" 1 one two))
  ;; Cannot remove from one as we traverse one,
  ;; as it might interfere with the iteration.
  (define to-remove
    (for/list ([v (in-set one)] #:unless (set-member? two v))
      v))
  (for ([v (in-list to-remove)])
    (set-remove! one v)))

(define set-intersect!
  (case-lambda
    [(s)
     (unless (set? s) (raise-argument-error 'set-intersect! "set?" 0 s))
     s]
    [(one two)
     (unless (set? one)
       (raise-argument-error 'set-intersect!? "set?" 0 one two))
     (unless (set? two)
       (raise-argument-error 'set-intersect!? "set?" 1 one two))
     (simple-set-intersect! one two)]
    [(s . sets)
     (unless (set? s)
       (apply raise-argument-error 'set-intersect! "set?" 0 s sets))
     (for ([s2 (in-list sets)] [i (in-naturals 1)])
       (unless (set? s2)
         (apply raise-argument-error 'set-intersect! "set?" i s sets))
       (simple-set-intersect! s s2))]))

(define (simple-set-subtract-fallback one two)
  (unless (set-remove-supported? one)
    (define str "(and/c set? set-remove-supported?)")
    (raise-argument-error 'set-subtract str 0 one two))
  (unless (set? two) (raise-argument-error 'set-subtract "set?" 1 one two))
  (for/fold ([s one]) ([v (in-set two)])
    (set-remove s v)))

(define set-subtract
  (case-lambda
    [(s)
     (unless (set? s) (raise-argument-error 'set-subtract "set?" 0 s))
     s]
    [(one two)
     (unless (set? one) (raise-argument-error 'set-subtract? "set?" 0 one two))
     (unless (set? two) (raise-argument-error 'set-subtract? "set?" 1 one two))
     (simple-set-subtract one two)]
    [(s . sets)
     (unless (set? s)
       (apply raise-argument-error 'set-subtract "set?" 0 s sets))
     (for/fold ([s1 s]) ([s2 (in-list sets)] [i (in-naturals 1)])
       (unless (set? s2)
         (apply raise-argument-error 'set-subtract "set?" i s sets))
       (simple-set-subtract s1 s2))]))

(define (simple-set-subtract!-fallback one two)
  (unless (set-remove!-supported? one)
    (define str "(and/c set? set-remove!-supported?)")
    (raise-argument-error 'set-subtract! str 0 one two))
  (unless (set? two) (raise-argument-error 'set-subtract! "set?" 1 one two))
  (for ([v (in-set two)])
    (set-remove! one v)))

(define set-subtract!
  (case-lambda
    [(s)
     (unless (set? s) (raise-argument-error 'set-subtract! "set?" 0 s))
     s]
    [(one two)
     (unless (set? one) (raise-argument-error 'set-subtract!? "set?" 0 one two))
     (unless (set? two) (raise-argument-error 'set-subtract!? "set?" 1 one two))
     (simple-set-subtract! one two)]
    [(s . sets)
     (unless (set? s)
       (apply raise-argument-error 'set-subtract! "set?" 0 s sets))
     (for ([s2 (in-list sets)] [i (in-naturals 1)])
       (unless (set? s2)
         (apply raise-argument-error 'set-subtract! "set?" i s sets))
       (simple-set-subtract! s s2))]))

(define (simple-set-symmetric-difference-fallback one two)
  (unless (and (set-add-supported? one)
               (set-remove-supported? one))
    (define str "(and/c set? set-add-supported? set-remove-supported?)")
    (raise-argument-error 'set-symmetric-difference str 0 one two))
  (unless (set? two)
    (raise-argument-error 'set-symmetric-difference "set?" 1 one two))
  (for/fold ([s one]) ([v (in-set two)])
    (if (set-member? s v)
        (set-remove s v)
        (set-add s v))))

(define set-symmetric-difference
  (case-lambda
    [(s)
     (unless (set? s)
       (raise-argument-error 'set-symmetric-difference "set?" 0 s))
     s]
    [(one two)
     (unless (set? one)
       (raise-argument-error 'set-symmetric-difference? "set?" 0 one two))
     (unless (set? two)
       (raise-argument-error 'set-symmetric-difference? "set?" 1 one two))
     (simple-set-symmetric-difference one two)]
    [(s . sets)
     (unless (set? s)
       (apply raise-argument-error 'set-symmetric-difference "set?" 0 s sets))
     (for/fold ([s1 s]) ([s2 (in-list sets)] [i (in-naturals 1)])
       (unless (set? s2)
         (apply raise-argument-error 'set-symmetric-difference "set?" i s sets))
       (simple-set-symmetric-difference s1 s2))]))

(define (simple-set-symmetric-difference!-fallback one two)
  (unless (and (set-add!-supported? one)
               (set-remove!-supported? one))
    (define str "(and/c set? set-add!-supported? set-remove!-supported?)")
    (raise-argument-error 'set-symmetric-difference! str 0 one two))
  (unless (set? two)
    (raise-argument-error 'set-symmetric-difference! "set?" 1 one two))
  (for ([v (in-set two)])
    (if (set-member? one v)
        (set-remove! one v)
        (set-add! one v))))

(define set-symmetric-difference!
  (case-lambda
    [(s)
     (unless (set? s)
       (raise-argument-error 'set-symmetric-difference! "set?" 0 s))
     s]
    [(one two)
     (unless (set? one)
       (raise-argument-error 'set-symmetric-difference!? "set?" 0 one two))
     (unless (set? two)
       (raise-argument-error 'set-symmetric-difference!? "set?" 1 one two))
     (simple-set-symmetric-difference! one two)]
    [(s . sets)
     (unless (set? s)
       (apply raise-argument-error 'set-symmetric-difference! "set?" 0 s sets))
     (for ([s2 (in-list sets)] [i (in-naturals 1)])
       (unless (set? s2)
         (define sym 'set-symmetric-difference!)
         (apply raise-argument-error sym "set?" i s sets))
       (simple-set-symmetric-difference! s s2))]))

(define-generics set
  #:defined-table set-supported
  (set-count set)
  (set-member? set x)
  (set-add set x)
  (set-remove set x)
  (set-clear set)
  (set-add! set x)
  (set-remove! set x)
  (set-clear! set)
  (set->stream set)
  (set-empty? set)
  (set-first set)
  (set-rest set)
  (set-copy set)
  (set-map set proc)
  (set-for-each set proc)
  (set->list set)
  (set=? set set2)
  (subset? set set2)
  (proper-subset? set set2)
  (simple-set-union set set2)
  (simple-set-intersect set set2)
  (simple-set-subtract set set2)
  (simple-set-symmetric-difference set set2)
  (simple-set-union! set set2)
  (simple-set-intersect! set set2)
  (simple-set-subtract! set set2)
  (simple-set-symmetric-difference! set set2)
  #:defaults
  ([list?
    (define set-count length)
    (define (set-member? lst x) (if (member x lst) #t #f))
    (define (set-add lst x) (if (member x lst) lst (cons x lst)))
    (define (set-remove lst x) (remove* (list x) lst))
    (define (set-clear lst) '())
    (define set->stream values)
    (define set-empty? null?)
    (define set-first car)
    (define set-rest cdr)
    (define (set-map lst proc) (map proc lst))
    (define (set-for-each lst proc) (for-each proc lst))
    (define set->list values)])
  #:fallbacks
  [(define set-empty? set-empty?-fallback)
   (define set-first set-first-fallback)
   (define set-rest set-rest-fallback)
   (define set-copy set-copy-fallback)
   (define set-map set-map-fallback)
   (define set-for-each set-for-each-fallback)
   (define set->list set->list-fallback)
   (define set=? set=?-fallback)
   (define subset? subset?-fallback)
   (define proper-subset? proper-subset?-fallback)
   (define simple-set-union simple-set-union-fallback)
   (define simple-set-intersect simple-set-intersect-fallback)
   (define simple-set-subtract simple-set-subtract-fallback)
   (define simple-set-symmetric-difference
     simple-set-symmetric-difference-fallback)
   (define simple-set-union! simple-set-union!-fallback)
   (define simple-set-intersect! simple-set-intersect!-fallback)
   (define simple-set-subtract! simple-set-subtract!-fallback)
   (define simple-set-symmetric-difference!
     simple-set-symmetric-difference!-fallback)])

(define (generic-set/c c)
  (define set-of-c (recursive-contract (make-set-contract c) #:chaperone))
  (set/c
    [set-count (-> set? exact-nonnegative-integer?)]
    [set-member? (-> set? c boolean?)]
    [set-add (-> set? c set-of-c)]
    [set-remove (-> set? c set-of-c)]
    [set-clear (-> set? set-of-c)]
    [set-add! (-> set? c void?)]
    [set-remove! (-> set? c void?)]
    [set-clear! (-> set? set-of-c)]
    [set->stream (-> set? stream?)]
    [set-first (-> set? c)]
    [set-rest (-> set? set-of-c)]
    [set-copy (-> set? set-of-c)]
    [set-map (-> set? (-> c any/c) list?)]
    [set-for-each (-> set? (-> c any) any)]
    [set->list (-> set? (listof c))]
    [set=? (-> set? set-of-c boolean?)]
    [subset? (-> set? set-of-c boolean?)]
    [proper-subset? (-> set? set-of-c boolean?)]
    [simple-set-union (-> set? set-of-c set-of-c)]
    [simple-set-intersect (-> set? set-of-c set-of-c)]
    [simple-set-subtract (-> set? set-of-c set-of-c)]
    [simple-set-symmetric-difference (-> set? set-of-c set-of-c)]
    [simple-set-union! (-> set? set-of-c void?)]
    [simple-set-intersect! (-> set? set-of-c void?)]
    [simple-set-subtract! (-> set? set-of-c void?)]
    [simple-set-symmetric-difference! (-> set? set-of-c void?)]))

(define-sequence-syntax in-set
  (lambda () #'in-set/proc)
  (lambda (stx)
    (syntax-case stx ()
      [[(id) (_ s)]
       #'[(id) (in-stream (set->stream s))]])))

(define in-set/proc
  (procedure-rename
    (lambda (s)
      (unless (set? s) (raise-argument-error 'in-set "set?" 0 s))
      (set->stream s))
    'in-set))

(define make-set-contract
  (procedure-rename
    (lambda (ctc #:cmp [cmp 'dont-care])
      (unless (memq cmp '(dont-care equal eq eqv))
        (raise-argument-error 'set/c 
                              "(or/c 'dont-care 'equal? 'eq? 'eqv)" 
                              cmp))
      (cond
        [(flat-contract? ctc)
         (flat-set/c ctc cmp)]
        [(chaperone-contract? ctc)
         (if (memq cmp '(eq eqv))
             (raise-arguments-error
               'set/c
               "given contract must be a flat contract if comparison is 'eq or 'eqv"
               "given contract" ctc
               "comparison" cmp)
             (chaperone-set/c ctc cmp))]
        [else
         (raise-argument-error 'set/c "chaperone-contract?" ctc)]))
    'set/c))

(struct set-contract [ctc cmp])

(define (set-contract-name c)
  (define ctc (set-contract-ctc c))
  (define cmp (set-contract-cmp c))
  (define suffix
    (case cmp
      [(eq) '(#:cmp (quote eq))]
      [(eqv) '(#:cmp (quote eqv))]
      [(equal) '(#:cmp (quote equal))]
      [(dont-care) '()]))
  (define prefix
    (contract-name ctc))
  `(set/c ,prefix ,@suffix))

(define (set-contract-first-order c)
  (define ctc (set-contract-ctc c))
  (define cmp (set-contract-cmp c))
  (define pred (contract-first-order ctc))
  (lambda (x)
    (and (set? x)
         (case cmp
           [(eq) (set-eq? x)]
           [(eqv) (set-eqv? x)]
           [(equal) (set-equal? x)]
           [(dont-care) #t])
         (for/and ([v (in-set x)])
           (pred v)))))

(define (set-contract-stronger one two)
  (define ctc1 (set-contract-ctc one))
  (define ctc2 (set-contract-ctc two))
  (define cmp1 (set-contract-cmp one))
  (define cmp2 (set-contract-cmp two))
  (and (or (eq? cmp1 cmp2) (eq? cmp2 'dont-care))
       (contract-stronger? ctc1 ctc2)))

(define (set-contract-projection c)
  (define ctc (set-contract-ctc c))
  (define cmp (set-contract-cmp c))
  (lambda (b)
    (define (check pred x msg)
      (unless (pred x)
        (raise-blame-error b x '(expected: "~a" given: "~v") msg x)))
    (lambda (x)
      (check set? x "a set")
      (case cmp
        [(eq) (check set-eq? x "an eq?-based set")]
        [(eqv) (check set-eqv? x "an eqv?-based set")]
        [(equal) (check set-equal? x "an equal?-based set")])
      (if (list? x)
          (((contract-projection (listof ctc)) b) x)
          (((contract-projection (generic-set/c ctc)) b) x)))))

(define (set-contract-flat-projection c)
  (define ctc (set-contract-ctc c))
  (define cmp (set-contract-cmp c))
  (lambda (b)
    (define (check pred x msg)
      (unless (pred x)
        (raise-blame-error b x '(expected: "~a" given: "~v") msg x)))
    (lambda (x)
      (check set? x "a set")
      (case cmp
        [(eq) (check set-eq? x "an eq?-based set")]
        [(eqv) (check set-eqv? x "an eqv?-based set")]
        [(equal) (check set-equal? x "an equal?-based set")])
      (if (list? x)
          (((contract-projection (listof ctc)) b) x)
          (begin
            (for ([v (in-set x)])
              (((contract-projection ctc) b) v))
            x)))))

(struct flat-set/c set-contract []
  #:property prop:flat-contract
  (build-flat-contract-property
    #:name set-contract-name
    #:first-order set-contract-first-order
    #:stronger set-contract-stronger
    #:projection set-contract-flat-projection))

(struct chaperone-set/c set-contract []
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
    #:name set-contract-name
    #:first-order set-contract-first-order
    #:stronger set-contract-stronger
    #:projection set-contract-projection))

(define (ht-set-custom-write s port mode)
  (define ht (ht-set-table s))
  (define recur-print (cond
                       [(not mode) display]
                       [(integer? mode) (lambda (p port) (print p port mode))]
                       [else write]))
  (define (print-prefix port)
    (cond
      [(equal? 0 mode)
       (write-string "(" port)
       (print-prefix-id port)]
      [else
       (write-string "#<" port)
       (print-prefix-id port)
       (write-string ":" port)]))
  (define (print-prefix-id port)
    (cond
      [(immutable? ht) (write-string "set" port)]
      [else (write-string "mutable-set" port)])
    (cond
      [(hash-equal? ht) (void)]
      [(hash-eqv? ht) (write-string "eqv" port)]
      [(hash-eq? ht) (write-string "eq" port)]))
  (define (print-suffix port)
    (if (equal? 0 mode)
        (write-string ")" port)
        (write-string ">" port)))
  (define (print-one-line port)
    (print-prefix port)
    (for ([e (in-hash-keys ht)]) 
      (write-string " " port)
      (recur-print e port))
    (print-suffix port))
  (define (print-multi-line port)
    (let-values ([(line col pos) (port-next-location port)])
      (print-prefix port)
      (for ([e (in-hash-keys ht)]) 
        (pretty-print-newline port (pretty-print-columns))
        (write-string (make-string (add1 col) #\space) port)
        (recur-print e port))
      (print-suffix port)))
  (cond
   [(and (pretty-printing)
         (integer? (pretty-print-columns)))
    ((let/ec esc
       (letrec ([tport (make-tentative-pretty-print-output-port
                        port
                        (- (pretty-print-columns) 1)
                        (lambda () 
                          (esc
                           (lambda ()
                             (tentative-pretty-print-port-cancel tport)
                             (print-multi-line port)))))])
         (print-one-line tport)
         (tentative-pretty-print-port-transfer tport port))
       void))]
   [else (print-one-line port)]))

(define (ht-set-count set)
  (hash-count (ht-set-table set)))

(define (ht-set-member? set x)
  (hash-ref (ht-set-table set) x #f))

(define (ht-set-add set x)
  (immutable-ht-set (hash-set (ht-set-table set) x #t)))

(define (ht-set-remove set x)
  (immutable-ht-set (hash-remove (ht-set-table set) x)))

(define (ht-set-clear set)
  (define ht1 (ht-set-table set))
  (define ht2 (ht-clear ht1))
  (if (immutable? ht2)
      (immutable-ht-set ht2)
      (mutable-ht-set ht2)))

(define (ht-clear ht)
  (if (immutable? ht)
    (cond
      [(hash-eq? ht) (hasheq)]
      [(hash-eqv? ht) (hasheqv)]
      [(hash-equal? ht) (hash)])
    (cond
      [(hash-eq? ht) (make-hasheq)]
      [(hash-eqv? ht) (make-hasheqv)]
      [(hash-equal? ht) (make-hash)])))

(define (ht-set-add! set x)
  (hash-set! (ht-set-table set) x #t))

(define (ht-set-remove! set x)
  (hash-remove! (ht-set-table set) x))

(define (ht-set-clear! set)
  (define ht (ht-set-table set))
  (let loop ([n (hash-count ht)])
    (unless (zero? n)
      (for/first ([k (in-hash-keys ht)])
        (hash-remove! ht k))
      (loop (sub1 n)))))

(define (ht-set->stream set)
  (sequence->stream (in-hash-keys (ht-set-table set))))

(define (ht-set-empty? set)
  (zero? (hash-count (ht-set-table set))))

(define (hash-first-key ht)
  (hash-iterate-key ht (hash-iterate-first ht)))

(define (ht-set-first set)
  (hash-first-key (ht-set-table set)))

(define (ht-set-rest set)
  (define ht (ht-set-table set))
  (immutable-ht-set (hash-remove ht (hash-first-key ht))))

(define (ht-set-copy set)
  (define ht1 (ht-set-table set))
  (cond
    [(immutable? ht1) set]
    [else
     (define ht2 (ht-clear ht1))
     (for ([(k) (in-hash-keys ht1)])
       (hash-set! ht2 k #t))
     (mutable-ht-set ht2)]))

(define (ht-set-map set proc)
  (for/list ([x (in-hash-keys (ht-set-table set))])
    (proc x)))

(define (ht-set-for-each set proc)
  (for ([x (in-hash-keys (ht-set-table set))])
    (proc x)))

(define (ht-set->list set)
  (for/list ([x (in-hash-keys (ht-set-table set))])
    x))

(define (ht-set=? set1 set2)
  (and (ht-set? set2)
    (equal? (ht-set-table set1) (ht-set-table set2))))

(define (ht-subset? set1 set2)
  (check-ht-set/equiv? 'subset? set1 set2)
  (define ht1 (ht-set-table set1))
  (define ht2 (ht-set-table set2))
  (and (<= (hash-count ht1) (hash-count ht2))
    (for/and ([x (in-hash-keys ht1)])
      (hash-ref ht2 x #f))))

(define (ht-proper-subset? set1 set2)
  (check-ht-set/equiv? 'proper-subset? set1 set2)
  (define ht1 (ht-set-table set1))
  (define ht2 (ht-set-table set2))
  (and (< (hash-count ht1) (hash-count ht2))
    (for/and ([x (in-hash-keys ht1)])
      (hash-ref ht2 x #f))))

(define (ht-set-union set1 set2)
  (check-ht-set/equiv? 'set-union set1 set2)
  (define ht1 (ht-set-table set1))
  (define ht2 (ht-set-table set2))
  (define-values (large small)
    (if (>= (hash-count ht1) (hash-count ht2))
        (values ht1 ht2)
        (values ht2 ht1)))
  (immutable-ht-set
    (for/fold ([ht large]) ([x (in-hash-keys small)])
      (hash-set ht x #t))))

(define (ht-set-union! set1 set2)
  (check-ht-set/equiv? 'set-union! set1 set2)
  (define ht1 (ht-set-table set1))
  (define ht2 (ht-set-table set2))
  (for ([k (in-hash-keys ht2)])
    (hash-set! ht1 k #t)))

(define (ht-set-intersect set1 set2)
  (check-ht-set/equiv? 'set-intersect set1 set2)
  (define ht1 (ht-set-table set1))
  (define ht2 (ht-set-table set2))
  (define-values (large small)
    (if (>= (hash-count ht1) (hash-count ht2))
        (values ht1 ht2)
        (values ht2 ht1)))
  (immutable-ht-set
    (for/fold ([ht small]) ([x (in-hash-keys small)])
      (if (hash-ref large x #f)
          ht
          (hash-remove ht x)))))

(define (ht-set-intersect! set1 set2)
  (check-ht-set/equiv? 'set-intersect! set1 set2)
  (define ht1 (ht-set-table set1))
  (define ht2 (ht-set-table set2))
  ;; Cannot mutate ht1 while traversing it.
  (define to-remove
    (for/list ([k (in-hash-keys ht1)] #:unless (hash-ref ht2 k #f))
      k))
  (for ([k (in-list to-remove)])
    (hash-remove! ht1 k)))

(define (ht-set-subtract set1 set2)
  (check-ht-set/equiv? 'set-subtract set1 set2)
  (define ht1 (ht-set-table set1))
  (define ht2 (ht-set-table set2))
  (immutable-ht-set
    (for/fold ([ht ht1]) ([x (in-hash-keys ht2)])
      (hash-remove ht x))))

(define (ht-set-subtract! set1 set2)
  (check-ht-set/equiv? 'set-subtract! set1 set2)
  (define ht1 (ht-set-table set1))
  (define ht2 (ht-set-table set2))
  (for ([k (in-hash-keys ht2)])
    (hash-remove! ht1 k)))

(define (ht-set-symmetric-difference set1 set2)
  (check-ht-set/equiv? 'set-symmetric-difference set1 set2)
  (define ht1 (ht-set-table set1))
  (define ht2 (ht-set-table set2))
  (define-values (large small)
    (if (>= (hash-count ht1) (hash-count ht2))
        (values ht1 ht2)
        (values ht2 ht1)))
  (immutable-ht-set
    (for/fold ([ht large]) ([x (in-hash-keys small)])
      (if (hash-ref large x #f)
          (hash-remove ht x)
          (hash-set ht x #t)))))

(define (ht-set-symmetric-difference! set1 set2)
  (check-ht-set/equiv? 'set-symmetric-difference! set1 set2)
  (define ht1 (ht-set-table set1))
  (define ht2 (ht-set-table set2))
  (for ([k (in-hash-keys ht2)])
    (if (hash-ref ht1 k #f)
        (hash-remove! ht1 k)
        (hash-set! ht1 k #t))))

(define (check-ht-set/equiv? who set1 set2)
  (define ht (ht-set-table set1))
  (unless (cond
            [(hash-eq? ht) (set-eq? set2)]
            [(hash-eqv? ht) (set-eqv? set2)]
            [(hash-equal? ht) (set-equal? set2)])
    (raise-arguments-error
      who
      "second set's equivalence predicate is not the same as the first set's"
      "first set" set1
      "second set" set2)))

(define (ht-set-equal? one two [rec equal?])
  (rec (ht-set-table one) (ht-set-table two)))

(define (ht-set-code set [rec equal-hash-code])
  (rec (ht-set-table set)))

(serializable-struct ht-set [table]
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write ht-set-custom-write
  #:property prop:equal+hash (list ht-set-equal? ht-set-code ht-set-code)
  #:property prop:sequence ht-set->stream
  #:property prop:stream (vector ht-set-empty? ht-set-first ht-set-rest))

(serializable-struct immutable-ht-set ht-set []
  #:reflection-name 'set
  #:methods gen:set
  [(define set-count ht-set-count)
   (define set-member? ht-set-member?)
   (define set-add ht-set-add)
   (define set-remove ht-set-remove)
   (define set->stream ht-set->stream)
   (define set-clear ht-set-clear)
   (define set-copy ht-set-copy)
   (define set-empty? ht-set-empty?)
   (define set-first ht-set-first)
   (define set-rest ht-set-rest)
   (define set-map ht-set-map)
   (define set-for-each ht-set-for-each)
   (define set->list ht-set->list)
   (define set=? ht-set=?)
   (define subset? ht-subset?)
   (define proper-subset? ht-proper-subset?)
   (define simple-set-union ht-set-union)
   (define simple-set-intersect ht-set-intersect)
   (define simple-set-subtract ht-set-subtract)
   (define simple-set-symmetric-difference ht-set-symmetric-difference)])

(serializable-struct mutable-ht-set ht-set []
  #:reflection-name 'mutable-set
  #:methods gen:set
  [(define set-count ht-set-count)
   (define set-member? ht-set-member?)
   (define set-add! ht-set-add!)
   (define set-remove! ht-set-remove!)
   (define set-clear! ht-set-clear!)
   (define set-clear ht-set-clear)
   (define set-copy ht-set-copy)
   (define set->stream ht-set->stream)
   (define set-empty? ht-set-empty?)
   (define set-first ht-set-first)
   (define set-rest ht-set-rest)
   (define set-map ht-set-map)
   (define set-for-each ht-set-for-each)
   (define set->list ht-set->list)
   (define set=? ht-set=?)
   (define subset? ht-subset?)
   (define proper-subset? ht-proper-subset?)
   (define simple-set-union! ht-set-union!)
   (define simple-set-intersect! ht-set-intersect!)
   (define simple-set-subtract! ht-set-subtract!)
   (define simple-set-symmetric-difference! ht-set-symmetric-difference!)])

(define (set-equal? x)
  (unless (set? x) (raise-argument-error 'set-equal? "set?" 0 x))
  (and (ht-set? x) (hash-equal? (ht-set-table x))))
(define (set-eqv? x)
  (unless (set? x) (raise-argument-error 'set-eqv? "set?" 0 x))
  (and (ht-set? x) (hash-eqv? (ht-set-table x))))
(define (set-eq? x)
  (unless (set? x) (raise-argument-error 'set-eq? "set?" 0 x))
  (and (ht-set? x) (hash-eq? (ht-set-table x))))

(define (set . elems)
  (immutable-ht-set
   (make-immutable-hash (map (lambda (k) (cons k #t)) elems))))
(define (seteq . elems)
  (immutable-ht-set
   (make-immutable-hasheq (map (lambda (k) (cons k #t)) elems))))
(define (seteqv . elems)
  (immutable-ht-set
   (make-immutable-hasheqv (map (lambda (k) (cons k #t)) elems))))

(define (mutable-set . elems)
  (mutable-ht-set (make-hash (map (lambda (k) (cons k #t)) elems))))
(define (mutable-seteq . elems)
  (mutable-ht-set (make-hasheq (map (lambda (k) (cons k #t)) elems))))
(define (mutable-seteqv . elems)
  (mutable-ht-set (make-hasheqv (map (lambda (k) (cons k #t)) elems))))

(define (list->set elems)
  (unless (list? elems) (raise-argument-error 'list->set "list?" 0 elems))
  (apply set elems))
(define (list->seteq elems)
  (unless (list? elems) (raise-argument-error 'list->seteq "list?" 0 elems))
  (apply seteq elems))
(define (list->seteqv elems)
  (unless (list? elems) (raise-argument-error 'list->seteqv "list?" 0 elems))
  (apply seteqv elems))

(define (list->mutable-set elems)
  (unless (list? elems)
    (raise-argument-error 'list->mutable-set "list?" 0 elems))
  (apply mutable-set elems))
(define (list->mutable-seteq elems)
  (unless (list? elems)
    (raise-argument-error 'list->mutable-seteq "list?" 0 elems))
  (apply mutable-seteq elems))
(define (list->mutable-seteqv elems)
  (unless (list? elems)
    (raise-argument-error 'list->mutable-seteqv "list?" 0 elems))
  (apply mutable-seteqv elems))

(define-syntax-rule (define-for for/fold/derived for/set hash)
  (...
    (define-syntax (for/set stx)
      (syntax-case stx ()
        [(_ bindings . body)
         (with-syntax ([((pre-body ...) post-body) (split-for-body stx #'body)]
                       [source stx])
           (syntax/loc stx
             (immutable-ht-set
              (for/fold/derived source ([ht (hash)]) bindings
                pre-body ...
                (hash-set ht (let () . post-body) #t)))))]))))

(define-for for/fold/derived for/set hash)
(define-for for*/fold/derived for*/set hash)
(define-for for/fold/derived for/seteq hasheq)
(define-for for*/fold/derived for*/seteq hasheq)
(define-for for/fold/derived for/seteqv hasheqv)
(define-for for*/fold/derived for*/seteqv hasheqv)

(define-syntax-rule (define-for-mutable for/fold/derived for/set make-hash)
  (...
    (define-syntax (for/set stx)
      (syntax-case stx ()
        [(_ bindings . body)
         (with-syntax ([((pre-body ...) post-body) (split-for-body stx #'body)]
                       [source stx])
           (syntax/loc stx
             (let ([ht (make-hash)])
               (for/fold/derived source () bindings
                 pre-body ...
                 (hash-set! ht (let () . post-body) #t)
                 (values))
               (mutable-ht-set ht))))]))))

(define-for-mutable for/fold/derived for/mutable-set make-hash)
(define-for-mutable for*/fold/derived for*/mutable-set make-hash)
(define-for-mutable for/fold/derived for/mutable-seteq make-hasheq)
(define-for-mutable for*/fold/derived for*/mutable-seteq make-hasheq)
(define-for-mutable for/fold/derived for/mutable-seteqv make-hasheqv)
(define-for-mutable for*/fold/derived for*/mutable-seteqv make-hasheqv)

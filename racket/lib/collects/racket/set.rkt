#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
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
  make-mutable-custom-set
  make-weak-custom-set)

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

;; Easily enabled/disabled debug printing:
(define-syntax dprintf
  #;(make-rename-transformer #'eprintf)
  (lambda (stx) #'(begin)))

(define (implement-ht-set name ;; e.g. 'mutable-seteqv
                          desc ;; e.g. "(and/c set? set-eq?)"
                          make-ht get-wrap get-unwrap
                          s? s-ht update-s set-s-ht!
                          alt1-s? alt1-s-ht alt2-s? alt2-s-ht)

  (define (s-custom-write s port mode)
    (define ht (s-ht s))
    (define unwrap (get-unwrap s))
    (define print-elem
      (cond
        [(not mode) display]
        [(integer? mode) (lambda (e port) (print e port mode))]
        [else write]))
    (define (print-prefix port)
      (cond
        [(equal? mode 0)
         (write-string "(" port)
         (display name port)]
        [else
         (write-string "#<" port)
         (display name port)
         (write-string ":" port)]))
    (define (print-suffix port)
      (cond
        [(equal? mode 0)
         (write-string ")" port)]
        [else
         (write-string ">" port)]))
    (define (print-one-line port)
      (print-prefix port)
      (for ([k (in-hash-keys ht)])
        (write-string " " port)
        (print-elem (unwrap k) port))
      (print-suffix port))
    (define (print-multi-line port)
      (define-values (line col pos) (port-next-location port))
      (print-prefix port)
      (for ([k (in-hash-keys ht)])
        (pretty-print-newline port (pretty-print-columns))
        (write-string (make-string (add1 col) #\space) port)
        (print-elem (unwrap k) port))
      (print-suffix port))
    (cond
      [(and (pretty-printing) (integer? (pretty-print-columns)))
       (define proc
         (let/ec esc
           (define tport
             (make-tentative-pretty-print-output-port
              port
              (- (pretty-print-columns) 1)
              (lambda ()
                (esc
                 (lambda ()
                   (tentative-pretty-print-port-cancel tport)
                   (print-multi-line port))))))
           (print-one-line tport)
           (tentative-pretty-print-port-transfer tport port)
           void))
       (proc)]
      [else (print-one-line port)]))

  (define (s-hash-code s [rec equal-hash-code])
    (dprintf "(equal-hash-code ~v)\n" s)
    (rec (s-ht s)))
  (define (s-count s)
    (dprintf "(set-count ~v)\n" s)
    (hash-count (s-ht s)))
  (define (s-member? s e)
    (dprintf "(set-member? ~v ~v)\n" s e)
    (hash-ref (s-ht s) ((get-wrap s) e) #f))
  (define (s-add s e)
    (dprintf "(set-add ~v ~v)\n" s e)
    (update-s s (hash-set (s-ht s) ((get-wrap s) e) #t)))
  (define (s-add! s e)
    (dprintf "(set-add! ~v ~v)\n" s e)
    (hash-set! (s-ht s) ((get-wrap s) e) #t))
  (define (s-remove s e)
    (dprintf "(set-remove ~v ~v)\n" s e)
    (update-s s (hash-remove (s-ht s) ((get-wrap s) e))))
  (define (s-remove! s e)
    (dprintf "(set-remove! ~v ~v)\n" s e)
    (hash-remove! (s-ht s) ((get-wrap s) e)))
  (define (s->stream s)
    (dprintf "(set->stream ~v)\n" s)
    (stream-map (get-unwrap s) (sequence->stream (in-hash-keys (s-ht s)))))
  (define (s-empty? s)
    (dprintf "(set-empty? ~v)\n" s)
    (zero? (hash-count (s-ht s))))
  (define (s-first s)
    (dprintf "(set-first ~v)\n" s)
    (define ht (s-ht s))
    (define pos (hash-iterate-first ht))
    (unless pos
      (raise-argument-error 's-first "(and/c set? (not/c set-empty?))" s))
    (hash-iterate-key ht pos))
  (define (s-rest s)
    (dprintf "(set-rest ~v)\n" s)
    (define ht (s-ht s))
    (define pos (hash-iterate-first ht))
    (unless pos
      (raise-argument-error 's-rest "(and/c set? (not/c set-empty?))" s))
    (update-s s (hash-remove ht (hash-iterate-key ht pos))))
  (define (s-rest! s)
    (dprintf "(set-rest ~v)\n" s)
    (stream-map (get-unwrap s)
                (stream-rest (sequence->stream (in-hash-keys (s-ht s))))))
  ;; There is no separate set-copy and set-copy!, just set-copy.
  ;; The two copies here implement set-copy for immutable and mutable sets.
  ;; Immutable sets don't need to do anything in s-copy.
  ;; Mutable sets need to copy every binding in s-copy!.
  (define (s-copy s)
    (dprintf "(set-copy ~v)\n" s)
    s)
  (define (s-copy! s)
    (dprintf "(set-copy ~v)\n" s)
    (update-s s (hash-copy (s-ht s))))
  (define (s-clear s)
    (dprintf "(set-clear ~v)\n" s)
    (update-s s (make-ht)))
  (define (s-clear! s)
    (dprintf "(set-clear! ~v)\n" s)
    (set-s-ht! s (make-ht)))

  (define (s-map s f)
    (dprintf "(set-map ~v ~v)\n" s f)
    ;; The order of set elements is unspecified.
    ;; Therefore we can build the list in one pass,
    ;; and not reverse at the end to preserve left-to-right order.
    (define unwrap (get-unwrap s))
    (for/fold ([lst '()]) ([k (in-hash-keys (s-ht s))])
      (cons (f (unwrap k)) lst)))
  (define (s->list s)
    (dprintf "(set->list ~v)\n" s)
    ;; As with s-map above, we don't have to preserve element order.
    (define unwrap (get-unwrap s))
    (for/fold ([lst '()]) ([k (in-hash-keys (s-ht s))])
      (cons (unwrap k) lst)))
  (define (s-for-each s f)
    (dprintf "(set-for-each ~v ~v)\n" s f)
    (define unwrap (get-unwrap s))
    (for ([k (in-hash-keys (s-ht s))])
      (f (unwrap k))))

  (define (s-equal? s1 s2 [rec equal?])
    (cond
      [(s? s2)
       (define ht1 (s-ht s1))
       (define ht2 (s-ht s2))
       (rec ht1 ht2)]
      [else #f]))
  (define (s=? s1 s2)
    (dprintf "(set=? ~v ~v)\n" s1 s2)
    (define ht1 (s-ht s1))
    (define ht2 (alt-s-ht #f s1 s2))
    (and ht2
         (= (hash-count ht1) (hash-count ht2))
         (for/and ([k (in-hash-keys ht1)])
           (hash-ref ht2 k #f))))
  (define (s-subset? s1 s2)
    (dprintf "(subset? ~v ~v)\n" s1 s2)
    (define ht1 (s-ht s1))
    (define ht2 (alt-s-ht 'subset? s1 s2))
    (and (<= (hash-count ht1) (hash-count ht2))
         (for/and ([k (in-hash-keys ht1)])
           (hash-ref ht2 k #f))))
  (define (s-proper-subset? s1 s2)
    (dprintf "(proper-subset? ~v ~v)\n" s1 s2)
    (define ht1 (s-ht s1))
    (define ht2 (alt-s-ht 'proper-subset? s1 s2))
    (and (< (hash-count ht1) (hash-count ht2))
         (for/and ([k (in-hash-keys ht1)])
           (hash-ref ht2 k #f))))

  (define (s-union s1 s2)
    (dprintf "(set-union ~v ~v)\n" s1 s2)
    (define ht1 (s-ht s1))
    (define ht2 (alt-s-ht 'set-union s1 s2))
    (define-values (target source)
      (if (and (s? s2) (< (hash-count ht1) (hash-count ht2)))
          (values ht2 ht1)
          (values ht1 ht2)))
    (define ht
      (for/fold ([ht target]) ([k (in-hash-keys source)])
        (hash-set ht k #t)))
    (update-s s1 ht))
  (define (s-intersect s1 s2)
    (dprintf "(set-intersect ~v ~v)\n" s1 s2)
    (define ht1 (s-ht s1))
    (define ht2 (alt-s-ht 'set-intersect s1 s2))
    (define-values (target source)
      (if (and (s? s2) (> (hash-count ht1) (hash-count ht2)))
          (values ht2 ht1)
          (values ht1 ht2)))
    (define ht
      (for/fold
          ([ht target])
          ([k (in-hash-keys target)]
           #:unless (hash-ref source k #f))
        (hash-remove ht k)))
    (update-s s1 ht))
  (define (s-subtract s1 s2)
    (dprintf "(set-subtract ~v ~v)\n" s1 s2)
    (define ht1 (s-ht s1))
    (define ht2 (alt-s-ht 'set-subtract s1 s2))
    (define ht
      (for/fold ([ht ht1]) ([k (in-hash-keys ht2)])
        (hash-remove ht k)))
    (update-s s1 ht))
  (define (s-symm-diff s1 s2)
    (dprintf "(set-symmetric-difference ~v ~v)\n" s1 s2)
    (define ht1 (s-ht s1))
    (define ht2 (s-ht s2))
    (define-values (target source)
      (if (and (s? s2) (< (hash-count ht1) (hash-count ht2)))
          (values ht2 ht1)
          (values ht1 ht2)))
    (define ht
      (for/fold ([ht target]) ([k (in-hash-keys source)])
        (if (hash-ref ht k #f)
            (hash-remove ht k)
            (hash-set ht k #t))))
    (update-s s1 ht))

  (define (s-union! s1 s2)
    (dprintf "(set-union! ~v ~v)\n" s1 s2)
    (define ht1 (s-ht s1))
    (define ht2 (alt-s-ht 'set-union! s1 s2))
    (for ([k (in-hash-keys ht2)])
      (hash-set! ht1 k #t)))
  (define (s-intersect! s1 s2)
    (dprintf "(set-intersect! ~v ~v)\n" s1 s2)
    (define ht1 (s-ht s1))
    (define ht2 (alt-s-ht 'set-intersect! s1 s2))
    ;; We cannot mutate the hash table while iterating through it.
    ;; Therefore we must record the values to remove, then remove them.
    ;; Order does not matter, so we can use for/fold instead of for/list.
    (define to-remove
      (for/fold ([lst '()]) ([k (in-hash-keys ht1)])
        (if (hash-ref ht2 k #f)
            lst
            (cons k lst))))
    (for ([k (in-list to-remove)])
      (hash-remove! ht1 k)))
  (define (s-subtract! s1 s2)
    (dprintf "(set-subtract! ~v ~v)\n" s1 s2)
    (define ht1 (s-ht s1))
    (define ht2 (alt-s-ht 'set-subtract! s1 s2))
    (for ([k (in-hash-keys ht2)])
      (hash-remove! ht1 k)))
  (define (s-symm-diff! s1 s2)
    (dprintf "(set-symmetric-difference! s1 s2)\n" s1 s2)
    (define ht1 (s-ht s1))
    (define ht2 (alt-s-ht 'set-symmetric-difference! s1 s2))
    (for ([k (in-hash-keys ht2)])
      (if (hash-ref ht1 k #f)
          (hash-remove! ht1 k)
          (hash-set! ht1 k #t))))

  (define (alt-s-ht op s1 s2)
    (cond
      [(s? s2) (s-ht s2)]
      [(alt1-s? s1 s2) (alt1-s-ht s2)]
      [(alt2-s? s1 s2) (alt2-s-ht s2)]
      [op (raise-argument-error op desc 1 s1 s2)]
      [else #f]))

  (values s-custom-write s-hash-code s-hash-code
          s-count s-member? s-add s-remove s-add! s-remove!
          s->stream s-empty? s-first s-rest s-rest!
          s-copy s-copy! s-clear s-clear!
          s-map s-for-each s->list
          s-equal? s=? s-subset? s-proper-subset?
          s-union s-intersect s-subtract s-symm-diff
          s-union! s-intersect! s-subtract! s-symm-diff!))

(define (implement-immutable-ht-set name desc
                                    make-ht get-wrap get-unwrap
                                    s? s-ht update-s
                                    alt1-s? alt1-s-ht alt2-s? alt2-s-ht)

  (define-values [s-custom-write s-hash-code1 s-hash-code2
                  s-count s-member? s-add s-remove s-add! s-remove!
                  s->stream s-empty? s-first s-rest s-rest!
                  s-copy s-copy! s-clear s-clear!
                  s-map s-for-each s->list
                  s-equal? s=? s-subset? s-proper-subset?
                  s-union s-intersect s-subtract s-symm-diff
                  s-union! s-intersect! s-subtract! s-symm-diff!]
    (implement-ht-set name desc
                      make-ht get-wrap get-unwrap
                      s? s-ht update-s void
                      alt1-s? alt1-s-ht alt2-s? alt2-s-ht))

  (values s-custom-write s-hash-code1 s-hash-code2
          s-count s-member? s-add s-remove
          s->stream s-empty? s-first s-rest
          s-copy s-clear
          s-map s-for-each s->list
          s-equal? s=? s-subset? s-proper-subset?
          s-union s-intersect s-subtract s-symm-diff))

(define (implement-mutable-ht-set name desc
                                  make-ht get-wrap get-unwrap
                                  s? s-ht update-s set-s-ht!
                                  alt1-s? alt1-s-ht alt2-s? alt2-s-ht)

  (define-values [s-custom-write s-hash-code1 s-hash-code2
                  s-count s-member? s-add s-remove s-add! s-remove!
                  s->stream s-empty? s-first s-rest s-rest!
                  s-copy s-copy! s-clear s-clear!
                  s-map s-for-each s->list
                  s-equal? s=? s-subset? s-proper-subset?
                  s-union s-intersect s-subtract s-symm-diff
                  s-union! s-intersect! s-subtract! s-symm-diff!]
    (implement-ht-set name desc
                      make-ht get-wrap get-unwrap
                      s? s-ht update-s set-s-ht!
                      alt1-s? alt1-s-ht alt2-s? alt2-s-ht))

  (values s-custom-write s-hash-code1 s-hash-code2
          s-count s-member? s-add! s-remove!
          s->stream s-empty? s-first s-rest!
          s-copy! s-clear s-clear!
          s-map s-for-each s->list
          s-equal? s=? s-subset? s-proper-subset?
          s-union! s-intersect! s-subtract! s-symm-diff!))

(define-syntax (declare-ht-sets stx)
  (syntax-case stx ()
    [(_ declare-struct base-name [field ...] desc-str
        make-immutable-ht
        make-mutable-ht
        make-weak-ht
        make-wrap
        make-unwrap)
     ;; This macro is for use in this file only,
     ;; so we don't need to do thorough checking of inputs.
     (let ()
       (define base-id #'base-name)
       (define (derived fmt . args)
         (apply format-id base-id #:source base-id fmt base-id args))

       ;; Visible names and quoted symbols:
       (define/with-syntax s? (derived "ht-~a?"))
       (define/with-syntax is-name (derived "~a"))
       (define/with-syntax ms-name (derived "mutable-~a"))
       (define/with-syntax ws-name (derived "weak-~a"))
       (define/with-syntax is-struct (derived "immutable-ht-~a"))
       (define/with-syntax ms-struct (derived "mutable-ht-~a"))
       (define/with-syntax ws-struct (derived "weak-ht-~a"))
       (define/with-syntax is? (derived "immutable-ht-~a?"))
       (define/with-syntax ms? (derived "mutable-ht-~a?"))
       (define/with-syntax ws? (derived "weak-ht-~a?"))
       (define/with-syntax make-is (derived "make-immutable-ht-~a"))
       (define/with-syntax make-ms (derived "make-mutable-ht-~a"))
       (define/with-syntax make-ws (derived "make-weak-ht-~a"))
       (define/with-syntax is-table (derived "immutable-ht-~a-table"))
       (define/with-syntax ms-table (derived "mutable-ht-~a-table"))
       (define/with-syntax ws-table (derived "weak-ht-~a-table"))
       (define/with-syntax set-ms-table! (derived "set-mutable-ht-~a-table!"))
       (define/with-syntax set-ws-table! (derived "set-weak-ht-~a-table!"))
       (define/with-syntax [is-field ...]
         (for/list ([field-id (in-list (syntax->list #'(field ...)))])
           (derived "immutable-ht-~a-~a" field-id)))
       (define/with-syntax [ms-field ...]
         (for/list ([field-id (in-list (syntax->list #'(field ...)))])
           (derived "mutable-ht-~a-~a" field-id)))
       (define/with-syntax [ws-field ...]
         (for/list ([field-id (in-list (syntax->list #'(field ...)))])
           (derived "weak-ht-~a-~a" field-id)))

       #'(begin
           (define (s? x)
             (or (is? x) (ms? x) (ws? x)))
           (define-values [is-custom-write is-code1 is-code2
                           is-count is-member? is-add is-remove
                           is->stream is-empty? is-first is-rest
                           is-clear is-copy
                           is-map is-for-each is->list
                           is-equal? is=? is-subset? is-proper-subset?
                           is-union is-intersect is-subtract is-symm-diff]
             (implement-immutable-ht-set 'is-name 'desc-str
               make-immutable-ht
               (lambda (s) (make-wrap (is-field s) ...))
               (lambda (s) (make-unwrap (is-field s) ...))
               (lambda (s) (is? s))
               (lambda (s) (is-table s))
               (lambda (s x) (make-is x (is-field s) ...))
               (lambda (s1 s2)
                 (and (ms? s2)
                      (equal? (is-field s1) (ms-field s2))
                      ...))
               (lambda (s) (ms-table s))
               (lambda (s1 s2)
                 (and (ws? s2)
                      (equal? (is-field s1) (ws-field s2))
                      ...))
               (lambda (s) (ws-table s))))
           (define-values [ms-custom-write ms-code1 ms-code2
                           ms-count ms-member? ms-add! ms-remove!
                           ms->stream ms-empty? ms-first ms-rest
                           ms-copy ms-clear ms-clear!
                           ms-map ms-for-each ms->list
                           ms-equal? ms=? ms-subset? ms-proper-subset?
                           ms-union! ms-intersect! ms-subtract! ms-symm-diff!]
             (implement-mutable-ht-set 'ms-name 'desc-str
               make-mutable-ht
               (lambda (s) (make-wrap (ms-field s) ...))
               (lambda (s) (make-unwrap (ms-field s) ...))
               (lambda (s) (ms? s))
               (lambda (s) (ms-table s))
               (lambda (s x) (make-ms x (ms-field s) ...))
               (lambda (s ht) (set-ms-table! s ht))
               (lambda (s1 s2)
                 (and (is? s2)
                      (equal? (ms-field s1) (is-field s2))
                      ...))
               (lambda (s) (is-table s))
               (lambda (s1 s2)
                 (and (ws? s2)
                      (equal? (ms-field s1) (ws-field s2))
                      ...))
               (lambda (s) (ws-table s))))
           (define-values [ws-custom-write ws-code1 ws-code2
                           ws-count ws-member? ws-add! ws-remove!
                           ws-clear! ws-clear ws-copy
                           ws->stream ws-empty? ws-first ws-rest
                           ws-map ws-for-each ws->list
                           ws-equal? ws=? ws-subset? ws-proper-subset?
                           ws-union! ws-intersect! ws-subtract! ws-symm-diff!]
             (implement-mutable-ht-set 'ws-name 'desc-str
               make-weak-ht
               (lambda (s) (make-wrap (ws-field s) ...))
               (lambda (s) (make-unwrap (ws-field s) ...))
               (lambda (s) (ws? s))
               (lambda (s) (ws-table s))
               (lambda (s x) (make-ws x (ws-field s) ...))
               (lambda (s ht) (set-ws-table! s ht))
               (lambda (s1 s2)
                 (and (is? s2)
                      (eq? (ws-field s1) (is-field s2))
                      ...))
               (lambda (s) (is-table s))
               (lambda (s1 s2)
                 (and (ms? s2)
                      (equal? (ws-field s1) (ms-field s2))
                      ...))
               (lambda (s) (ms-table s))))
           (declare-struct is-struct [table field ...]
             #:omit-define-syntaxes
             #:reflection-name 'is-name
             #:constructor-name make-is
             #:property prop:custom-print-quotable 'never
             #:property prop:custom-write is-custom-write
             #:property prop:equal+hash (list is-equal? is-code1 is-code2)
             #:property prop:sequence is->stream
             #:property prop:stream (vector is-empty? is-first is-rest)
             #:methods gen:set
             [(define set-count is-count)
              (define set-member? is-member?)
              (define set-add is-add)
              (define set-remove is-remove)
              (define set->stream is->stream)
              (define set-clear is-clear)
              (define set-copy is-copy)
              (define set-empty? is-empty?)
              (define set-first is-first)
              (define set-map is-map)
              (define set-for-each is-for-each)
              (define set->list is->list)
              (define set=? is=?)
              (define subset? is-subset?)
              (define proper-subset? is-proper-subset?)
              (define simple-set-union is-union)
              (define simple-set-intersect is-intersect)
              (define simple-set-subtract is-subtract)
              (define simple-set-symmetric-difference is-symm-diff)])
           (declare-struct ms-struct [table field ...]
             #:mutable
             #:omit-define-syntaxes
             #:reflection-name 'ms-name
             #:constructor-name make-ms
             #:property prop:custom-print-quotable 'never
             #:property prop:custom-write ms-custom-write
             #:property prop:equal+hash (list ms-equal? ms-code1 ms-code2)
             #:property prop:sequence ms->stream
             #:property prop:stream (vector ms-empty? ms-first ms-rest)
             #:methods gen:set
             [(define set-count ms-count)
              (define set-member? ms-member?)
              (define set-add! ms-add!)
              (define set-remove! ms-remove!)
              (define set-clear! ms-clear!)
              (define set-clear ms-clear)
              (define set-copy ms-copy)
              (define set->stream ms->stream)
              (define set-empty? ms-empty?)
              (define set-first ms-first)
              (define set-map ms-map)
              (define set-for-each ms-for-each)
              (define set->list ms->list)
              (define set=? ms=?)
              (define subset? ms-subset?)
              (define proper-subset? ms-proper-subset?)
              (define simple-set-union! ms-union!)
              (define simple-set-intersect! ms-intersect!)
              (define simple-set-subtract! ms-subtract!)
              (define simple-set-symmetric-difference! ms-symm-diff!)])
           (declare-struct ws-struct [table field ...]
             #:mutable
             #:omit-define-syntaxes
             #:reflection-name 'ws-name
             #:constructor-name make-ws
             #:property prop:custom-print-quotable 'never
             #:property prop:custom-write ws-custom-write
             #:property prop:equal+hash (list ws-equal? ws-code1 ws-code2)
             #:property prop:sequence ws->stream
             #:property prop:stream (vector ws-empty? ws-first ws-rest)
             #:methods gen:set
             [(define set-count ws-count)
              (define set-member? ws-member?)
              (define set-add! ws-add!)
              (define set-remove! ws-remove!)
              (define set-clear! ws-clear!)
              (define set-clear ws-clear)
              (define set-copy ws-copy)
              (define set->stream ws->stream)
              (define set-empty? ws-empty?)
              (define set-first ws-first)
              (define set-map ws-map)
              (define set-for-each ws-for-each)
              (define set->list ws->list)
              (define set=? ws=?)
              (define subset? ws-subset?)
              (define proper-subset? ws-proper-subset?)
              (define simple-set-union! ws-union!)
              (define simple-set-intersect! ws-intersect!)
              (define simple-set-subtract! ws-subtract!)
              (define simple-set-symmetric-difference! ws-symm-diff!)])))]))

(define (default-wrap) values)
(define (default-unwrap) values)

(declare-ht-sets serializable-struct set [] "(and/c set? set-equal?)"
  make-immutable-hash
  make-hash
  make-weak-hash
  default-wrap
  default-unwrap)

(declare-ht-sets serializable-struct seteqv [] "(and/c set? set-eqv?)"
  make-immutable-hasheqv
  make-hasheqv
  make-weak-hasheqv
  default-wrap
  default-unwrap)

(declare-ht-sets serializable-struct seteq [] "(and/c set? set-eq?)"
  make-immutable-hasheq
  make-hasheq
  make-weak-hasheq
  default-wrap
  default-unwrap)

(struct custom-wrapper [contents])

(define (custom-wrap wrapper) wrapper)
(define (custom-unwrap wrapper) custom-wrapper-contents)

(declare-ht-sets struct custom-set [wrapper] "a custom set"
  make-immutable-hash
  make-hash
  make-weak-hash
  custom-wrap
  custom-unwrap)

(define (custom-set-procs who =? hc hc2)
  (unless (and (procedure? =?)
               (or (procedure-arity-includes? =? 2)
                   (procedure-arity-includes? =? 3)))
    (raise-argument-error who
                          "a procedure of 2 or 3 arguments"
                          0 =? hc hc2))
  (when hc
    (unless (and (procedure? hc)
                 (or (procedure-arity-includes? hc 1)
                     (procedure-arity-includes? hc 2)))
      (raise-argument-error who
                            "#f or a procedure of 1 or 2 arguments"
                            1 =? hc hc2)))
  (when hc2
    (unless (and (procedure? hc2)
                 (or (procedure-arity-includes? hc2 1)
                     (procedure-arity-includes? hc2 2)))
      (raise-argument-error who
                            "#f or a procedure of 1 or 2 arguments"
                            1 =? hc hc2)))
  (define =?-proc
    (cond
      [(procedure-arity-includes? =? 3) =?]
      [else (lambda (x y rec) (=? x y))]))
  (define hc-proc
    (cond
      [(not hc) (lambda (x rec) 1)]
      [(procedure-arity-includes? hc 2) hc]
      [else (lambda (x rec) (hc x))]))
  (define hc2-proc
    (cond
      [(not hc2) (lambda (x rec) 1)]
      [(procedure-arity-includes? hc2 2) hc2]
      [else (lambda (x rec) (hc2 x))]))
  (values =?-proc hc-proc hc2-proc))

(define (make-custom-set =? [hc #f] [hc2 #f])
  (define-values (=?-proc hc-proc hc2-proc)
    (custom-set-procs 'make-custom-set =? hc hc2))
  (struct hash-wrapper custom-wrapper []
    #:property prop:equal+hash (list =?-proc hc-proc hc2-proc))
  (make-immutable-ht-custom-set (make-immutable-hash) hash-wrapper))

(define (make-mutable-custom-set =? [hc #f] [hc2 #f])
  (define-values (=?-proc hc-proc hc2-proc)
    (custom-set-procs 'make-mutable-custom-set =? hc hc2))
  (struct hash-wrapper custom-wrapper []
    #:property prop:equal+hash (list =?-proc hc-proc hc2-proc))
  (make-mutable-ht-custom-set (make-hash) hash-wrapper))

(define (make-weak-custom-set =? [hc #f] [hc2 #f])
  (define-values (=?-proc hc-proc hc2-proc)
    (custom-set-procs 'make-mutable-custom-set =? hc hc2))
  (struct hash-wrapper custom-wrapper []
    #:property prop:equal+hash (list =?-proc hc-proc hc2-proc))
  (make-weak-ht-custom-set (make-weak-hash) hash-wrapper))

(define-syntax-rule (define-for for/fold/derived for/set
                                make-hash make-set)
  (...
    (define-syntax (for/set stx)
      (syntax-case stx ()
        [(_ bindings . body)
         (with-syntax ([((pre-body ...) post-body) (split-for-body stx #'body)]
                       [source stx])
           (syntax/loc stx
             (make-set
              (for/fold/derived source ([ht (make-hash)]) bindings
                pre-body ...
                (hash-set ht (let () . post-body) #t)))))]))))

(define-for for/fold/derived for/set
            make-immutable-hash make-immutable-ht-set)
(define-for for*/fold/derived for*/set
            make-immutable-hash make-immutable-ht-set)
(define-for for/fold/derived for/seteq
            make-immutable-hasheq make-immutable-ht-seteq)
(define-for for*/fold/derived for*/seteq
            make-immutable-hasheq make-immutable-ht-seteq)
(define-for for/fold/derived for/seteqv
            make-immutable-hasheqv make-immutable-ht-seteqv)
(define-for for*/fold/derived for*/seteqv
            make-immutable-hasheqv make-immutable-ht-seteqv)

(define-syntax-rule (define-for-mutable for/fold/derived for/set
                                        make-hash make-set)
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
               (make-set ht))))]))))

(define-for-mutable for/fold/derived for/mutable-set
                    make-hash make-mutable-ht-set)
(define-for-mutable for*/fold/derived for*/mutable-set
                    make-hash make-mutable-ht-set)
(define-for-mutable for/fold/derived for/mutable-seteq
                    make-hasheq make-mutable-ht-seteq)
(define-for-mutable for*/fold/derived for*/mutable-seteq
                    make-hasheq make-mutable-ht-seteq)
(define-for-mutable for/fold/derived for/mutable-seteqv
                    make-hasheqv make-mutable-ht-seteqv)
(define-for-mutable for*/fold/derived for*/mutable-seteqv
                    make-hasheqv make-mutable-ht-seteqv)

(define-for-mutable for/fold/derived for/weak-set
                    make-weak-hash make-weak-ht-set)
(define-for-mutable for*/fold/derived for*/weak-set
                    make-weak-hash make-weak-ht-set)
(define-for-mutable for/fold/derived for/weak-seteq
                    make-weak-hasheq make-weak-ht-seteq)
(define-for-mutable for*/fold/derived for*/weak-seteq
                    make-weak-hasheq make-weak-ht-seteq)
(define-for-mutable for/fold/derived for/weak-seteqv
                    make-weak-hasheqv make-weak-ht-seteqv)
(define-for-mutable for*/fold/derived for*/weak-seteqv
                    make-weak-hasheqv make-weak-ht-seteqv)

(define (set-equal? x)
  (unless (set? x) (raise-argument-error 'set-equal? "set?" 0 x))
  (ht-set? x))
(define (set-eqv? x)
  (unless (set? x) (raise-argument-error 'set-eqv? "set?" 0 x))
  (ht-seteqv? x))
(define (set-eq? x)
  (unless (set? x) (raise-argument-error 'set-eq? "set?" 0 x))
  (ht-seteq? x))

(define (set . elems) (for/set ([e (in-list elems)]) e))
(define (seteq . elems) (for/seteq ([e (in-list elems)]) e))
(define (seteqv . elems) (for/seteqv ([e (in-list elems)]) e))
(define (mutable-set . elems) (for/mutable-set ([e (in-list elems)]) e))
(define (mutable-seteq . elems) (for/mutable-seteq ([e (in-list elems)]) e))
(define (mutable-seteqv . elems) (for/mutable-seteqv ([e (in-list elems)]) e))
(define (weak-set . elems) (for/weak-set ([e (in-list elems)]) e))
(define (weak-seteq . elems) (for/weak-seteq ([e (in-list elems)]) e))
(define (weak-seteqv . elems) (for/weak-seteqv ([e (in-list elems)]) e))

(define (list->set elems)
  (unless (list? elems)
    (raise-argument-error 'list->set "list?" elems))
  (for/set ([e (in-list elems)]) e))
(define (list->seteq elems)
  (unless (list? elems)
    (raise-argument-error 'list->seteq "list?" elems))
  (for/seteq ([e (in-list elems)]) e))
(define (list->seteqv elems)
  (unless (list? elems)
    (raise-argument-error 'list->seteqv "list?" elems))
  (for/seteqv ([e (in-list elems)]) e))

(define (list->mutable-set elems)
  (unless (list? elems)
    (raise-argument-error 'list->mutable-set "list?" elems))
  (for/mutable-set ([e (in-list elems)]) e))
(define (list->mutable-seteq elems)
  (unless (list? elems)
    (raise-argument-error 'list->mutable-seteq "list?" elems))
  (for/mutable-seteq ([e (in-list elems)]) e))
(define (list->mutable-seteqv elems)
  (unless (list? elems)
    (raise-argument-error 'list->mutable-seteqv "list?" elems))
  (for/mutable-seteqv ([e (in-list elems)]) e))

(define (list->weak-set elems)
  (unless (list? elems)
    (raise-argument-error 'list->weak-set "list?" elems))
  (for/weak-set ([e (in-list elems)]) e))
(define (list->weak-seteq elems)
  (unless (list? elems)
    (raise-argument-error 'list->weak-seteq "list?" elems))
  (for/weak-seteq ([e (in-list elems)]) e))
(define (list->weak-seteqv elems)
  (unless (list? elems)
    (raise-argument-error 'list->weak-seteqv "list?" elems))
  (for/weak-seteqv ([e (in-list elems)]) e))

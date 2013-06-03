(load-relative "loadtest.rktl")

(Section 'sets)
(require scheme/set)

;; ----------------------------------------

(test #t set? (set))
(test #t set-empty? (set))
(test #t set? (set 1 2 3))
(test #f set-empty? (set 1 2 3))
(test #t set? (seteq))
(test #t set-empty? (seteq))
(test #t set? (seteq 1 2 3))
(test #f set-empty? (seteq 1 2 3))
(test #t set? (seteqv))
(test #t set-empty? (seteqv))
(test #t set? (seteqv 1 2 3))
(test #f set-empty? (seteqv 1 2 3))
(test #t set? (mutable-set))
(test #t set-empty? (mutable-set))
(test #t set? (mutable-set 1 2 3))
(test #f set-empty? (mutable-set 1 2 3))
(test #t set? (mutable-seteq))
(test #t set-empty? (mutable-seteq))
(test #t set? (mutable-seteq 1 2 3))
(test #f set-empty? (mutable-seteq 1 2 3))
(test #t set? (mutable-seteqv))
(test #t set-empty? (mutable-seteqv))
(test #t set? (mutable-seteqv 1 2 3))
(test #f set-empty? (mutable-seteqv 1 2 3))
(test #t set? (list))
(test #t set-empty? (list))
(test #t set? (list 1 2 3))
(test #f set-empty? (list 1 2 3))

(test #f set-eq? (set 1 2 3))
(test #f set-eqv? (set 1 2 3))
(test #t set-equal? (set 1 2 3))
(test #t set-eq? (seteq 1 2 3))
(test #f set-eqv? (seteq 1 2 3))
(test #f set-equal? (seteq 1 2 3))
(test #f set-eq? (seteqv 1 2 3))
(test #t set-eqv? (seteqv 1 2 3))
(test #f set-equal? (seteqv 1 2 3))
(test #f set-eq? (mutable-set 1 2 3))
(test #f set-eqv? (mutable-set 1 2 3))
(test #t set-equal? (mutable-set 1 2 3))
(test #t set-eq? (mutable-seteq 1 2 3))
(test #f set-eqv? (mutable-seteq 1 2 3))
(test #f set-equal? (mutable-seteq 1 2 3))
(test #f set-eq? (mutable-seteqv 1 2 3))
(test #t set-eqv? (mutable-seteqv 1 2 3))
(test #f set-equal? (mutable-seteqv 1 2 3))
(test #f set-eq? (list 1 2 3))
(test #f set-eqv? (list 1 2 3))
(test #f set-equal? (list 1 2 3))

(test 3 set-count (set (string #\a) "b" "c" (string #\a)))
(test 4 set-count (seteqv (string #\a) "b" "c" (string #\a)))
(test 4 set-count (seteq (string #\a) "b" "c" (string #\a)))
(test 3 set-count (mutable-set (string #\a) "b" "c" (string #\a)))
(test 4 set-count (mutable-seteqv (string #\a) "b" "c" (string #\a)))
(test 4 set-count (mutable-seteq (string #\a) "b" "c" (string #\a)))
(test 4 set-count (list (string #\a) "b" "c" (string #\a)))

(test #t set-member? (set 1 2 3) 1)
(test #t set-member? (set 1 2 3) 2)
(test #t set-member? (set 1 2 3) 3)
(test #f set-member? (set 1 2 3) 4)

(test #t set-member? (seteq 1 2 3) 1)
(test #t set-member? (seteq 1 2 3) 2)
(test #t set-member? (seteq 1 2 3) 3)
(test #f set-member? (seteq 1 2 3) 4)

(test #t set-member? (seteqv 1 2 3) 1)
(test #t set-member? (seteqv 1 2 3) 2)
(test #t set-member? (seteqv 1 2 3) 3)
(test #f set-member? (seteqv 1 2 3) 4)

(test #t set-member? (mutable-set 1 2 3) 1)
(test #t set-member? (mutable-set 1 2 3) 2)
(test #t set-member? (mutable-set 1 2 3) 3)
(test #f set-member? (mutable-set 1 2 3) 4)

(test #t set-member? (mutable-seteq 1 2 3) 1)
(test #t set-member? (mutable-seteq 1 2 3) 2)
(test #t set-member? (mutable-seteq 1 2 3) 3)
(test #f set-member? (mutable-seteq 1 2 3) 4)

(test #t set-member? (mutable-seteqv 1 2 3) 1)
(test #t set-member? (mutable-seteqv 1 2 3) 2)
(test #t set-member? (mutable-seteqv 1 2 3) 3)
(test #f set-member? (mutable-seteqv 1 2 3) 4)

(test #t set-member? (list 1 2 3) 1)
(test #t set-member? (list 1 2 3) 2)
(test #t set-member? (list 1 2 3) 3)
(test #f set-member? (list 1 2 3) 4)

(test #t stream? (set 1 2 3))
(test (set-first (set 1 2 3)) set-first (set 1 2 3))
(test (set-remove (set 1 2 3) (set-first (set 1 2 3))) set-rest (set 1 2 3))

(test #t stream? (seteq 1 2 3))
(test (set-first (seteq 1 2 3)) set-first (seteq 1 2 3))
(test (set-remove (seteq 1 2 3) (set-first (seteq 1 2 3))) set-rest (seteq 1 2 3))

(test #t stream? (seteqv 1 2 3))
(test (set-first (seteqv 1 2 3)) set-first (seteqv 1 2 3))
(test (set-remove (seteqv 1 2 3) (set-first (seteqv 1 2 3))) set-rest (seteqv 1 2 3))

(test #t stream? (mutable-set 1 2 3))
(test (set-first (mutable-set 1 2 3)) set-first (mutable-set 1 2 3))

(test #t stream? (mutable-seteq 1 2 3))
(test (set-first (mutable-seteq 1 2 3)) set-first (mutable-seteq 1 2 3))

(test #t stream? (mutable-seteqv 1 2 3))
(test (set-first (mutable-seteqv 1 2 3)) set-first (mutable-seteqv 1 2 3))

(test #t stream? (list 1 2 3))
(test (set-first (list 1 2 3)) set-first (list 1 2 3))
(test (set-remove (list 1 2 3) (set-first (list 1 2 3))) set-rest (list 1 2 3))

(let ([s (set 1 2 3)])
  (test #t equal? s (set-add (set-add (set-add (set) 1) 2) 3))
  (test #t equal? (seteq 1 2 3) (seteq 1 2 3))
  (test #t equal? (seteq 1 2 3) (seteq 3 2 1))
  (test #t equal? (seteqv 1 2 3) (seteqv 1 2 3))
  (test #f equal? s (seteq 1 2 3))
  (test #f equal? s (seteqv 1 2 3))
  (test #f equal? (seteq 1 2 3) (seteqv 1 2 3))

  (test #t set-member? (set-add s 5) 3)
  (test #t set-member? (set-add s 5) 5)
  (test #f set-member? (set-add s 5) 4)

  (test #t set-member? (set-remove s 5) 3)
  (test #f set-member? (set-remove s 3) 3)

  (test #t subset? (set 1 3) s)
  (test #t subset? (set 1 2 3) s)
  (test #f subset? (set 1 4) s)
  (test #t subset? (set) s)

  (test 3 set-count (set-union s))
  (test 6 set-count (set-union s (set 3 4 5 6)))
  (test 6 set-count (set-union (set 3 4 5 6) s))
  (test 8 set-count (set-union (set 3 4 5 6) s (set 1 10 100)))

  (test (seteq 1 2 3) set-union (seteq 1 2) (seteq 3))
  (test (seteqv 1 2 3) set-union (seteqv 1 2) (seteqv 3))

  (test s set-intersect s)
  (test (set 3) set-intersect s (set 5 4 3 6))
  (test (set 3) set-intersect (set 5 4 3 6) s)
  (test (seteq 3) set-intersect (seteq 5 4 3 6) (seteq 1 2 3))
  (test (seteqv 3) set-intersect (seteqv 5 4 3 6) (seteqv 1 2 3))
  (test (set 3 2) set-intersect s (set 5 2 3))
  (test (seteq 3 2) set-intersect (seteq 1 2 3) (seteq 5 2 3))
  (test (set 2) set-intersect s (set 5 2 3) (set 2 20 200))
  (test (seteq 2) set-intersect (seteq 1 2 3) (seteq 5 2 3) (seteq 2 20 200))

  (test s set-subtract s)
  (test (set) set-subtract s s)
  (test s set-subtract s (set 100))
  (test (set 1 3) set-subtract s (set 2 100))
  (test (seteq 100) set-subtract (seteq 2 100) (seteq 1 2 3))
  (test (seteq 9 100) set-subtract (seteq 2 100 1000 9) (seteq 1 2 3) (seteq 1000 5))

  (let ([try-mismatch (lambda (set-op)
                        (err/rt-test (set-op (seteqv 1 2) (set 3)))
                        (err/rt-test (set-op (seteqv 1 2) (seteq 3)))
                        (err/rt-test (set-op (set 1 2) (seteq 3)))
                        (err/rt-test (set-op (set 1 2) (set 4) (seteq 3)))
                        (err/rt-test (set-op (set 1 2) (seteq 3) (set 4)))
                        (err/rt-test (set-op (seteq 3) (set 1 2) (set 4))))])
    (try-mismatch set-union)
    (try-mismatch set-intersect)
    (try-mismatch set-subtract))

  (test #t andmap negative? (set-map s -))
  (test 3 length (set-map s +))

  (let ([v 0])
    (set-for-each s (lambda (n) (set! v (+ v n))))
    (test 6 values v))

  (test '(1 2 3) sort (for/list ([v s]) v) <)
  (test '(1 2 3) sort (for/list ([v (in-set s)]) v) <)
  (test '(1 2 3) sort (let ([seq (in-set s)]) (for/list ([v seq]) v)) <)
  ;; Optimized
  (test '(1) sort (for/list ([v (in-set (set 1))]) v) <)
  (test #t values (let ([noset #t])
                    (for ([v (in-set (set))]) (set! noset #f))
                    noset))
        

  (void))

(let ()

  (define (test=? result s1 s2)
    (test result equal? s1 s2)
    (test result set=? s1 s2))

  (define (t mset-A mset-B mset-C set-A set-B set-C)

    (define (t1 ms s subs just-elems just-supers)

      ;; Construct sets for comparison:

      (define elems (append subs just-elems))
      (define supers (append elems just-supers))
      (define not-subs (append just-elems just-supers))
      (define msA (apply mset-A elems))
      (define msB (apply mset-B elems))
      (define msC (apply mset-C elems))
      (define sA (apply set-A elems))
      (define sB (apply set-B elems))
      (define sC (apply set-C elems))
      (define ms-sub (apply mset-A subs))
      (define ms-super (apply mset-A supers))
      (define ms-not-sub (apply mset-A not-subs))
      (define s-sub (apply set-A subs))
      (define s-super (apply set-A supers))
      (define s-not-sub (apply set-A not-subs))

      ;; Test equality:

      (test=? #true ms msA)
      (test=? #false ms msB)
      (test=? #false ms msC)
      (test=? #false ms sA)
      (test=? #false ms sB)
      (test=? #false ms sC)
      (test=? #true ms ms)
      (test=? (null? just-elems) ms ms-sub)
      (test=? (null? just-supers) ms ms-super)
      (test=? (and (null? subs) (null? just-supers)) ms ms-not-sub)

      (test=? #false s msA)
      (test=? #false s msB)
      (test=? #false s msC)
      (test=? #true s sA)
      (test=? #false s sB)
      (test=? #false s sC)
      (test=? #true s s)
      (test=? (null? just-elems) s s-sub)
      (test=? (null? just-supers) s s-super)
      (test=? (and (null? subs) (null? just-supers)) s s-not-sub)

      ;; Test membership:

      (for ([elem (in-list elems)])
        (test #true set-member? ms elem)
        (test #true set-member? s elem))

      (for ([elem (in-list just-supers)])
        (test #false set-member? ms elem)
        (test #false set-member? s elem))

      ;; Test subset:

      (test #true subset? ms ms)

      (test #true subset? ms msA)
      (test #false subset? ms ms-sub)
      (test #true subset? ms ms-super)
      (test (pair? subs) subset? ms ms-not-sub)

      (test #true subset? ms sA)
      (test #false subset? ms s-sub)
      (test #true subset? ms s-super)
      (test (pair? subs) subset? ms s-not-sub)

      (err/rt-test (subset? ms msB))
      (err/rt-test (subset? ms msC))
      (err/rt-test (subset? ms sB))
      (err/rt-test (subset? ms sC))

      (test #true subset? s s)

      (test #true subset? s msA)
      (test #false subset? s ms-sub)
      (test #true subset? s ms-super)
      (test (pair? subs) subset? s ms-not-sub)

      (test #true subset? s sA)
      (test #false subset? s s-sub)
      (test #true subset? s s-super)
      (test (pair? subs) subset? s s-not-sub)

      (err/rt-test (subset? s msB))
      (err/rt-test (subset? s msC))
      (err/rt-test (subset? s sB))
      (err/rt-test (subset? s sC))

      ;; Test proper subset:

      (test #false proper-subset? ms ms)

      (test #false proper-subset? ms msA)
      (test #false proper-subset? ms ms-sub)
      (test #true proper-subset? ms ms-super)
      (test #false proper-subset? ms ms-not-sub)

      (test #false proper-subset? ms sA)
      (test #false proper-subset? ms s-sub)
      (test #true proper-subset? ms s-super)
      (test #false proper-subset? ms s-not-sub)

      (err/rt-test (proper-subset? ms msB))
      (err/rt-test (proper-subset? ms msC))
      (err/rt-test (proper-subset? ms sB))
      (err/rt-test (proper-subset? ms sC))

      (test #false proper-subset? s s)

      (test #false proper-subset? s msA)
      (test #false proper-subset? s ms-sub)
      (test #true proper-subset? s ms-super)
      (test #false proper-subset? s ms-not-sub)

      (test #false proper-subset? s sA)
      (test #false proper-subset? s s-sub)
      (test #true proper-subset? s s-super)
      (test #false proper-subset? s s-not-sub)

      (err/rt-test (proper-subset? s msB))
      (err/rt-test (proper-subset? s msC))
      (err/rt-test (proper-subset? s sB))
      (err/rt-test (proper-subset? s sC))

      (void))

    (define ms (mset-A 1 2 3))
    (define s0 (set-A 1 2 3))
    (t1 ms s0 '(1 2) '(3) '(4))

    (set-remove! ms 3)
    (define s1 (set-remove s0 3))
    (t1 ms s1 '(1) '(2) '(3 4))

    (set-add! ms 4)
    (define s2 (set-add s1 4))
    (t1 ms s2 '(1) '(2 4) '(3))

    (set-clear! ms)
    (define s3 (set-clear s2))
    (t1 ms s3 '() '() '(1 2 3 4))

    (set-union! ms (mset-A 1 2) (mset-A 2 3))
    (define s4 (set-union s3 (set-A 1 2) (set-A 2 3)))
    (t1 ms s4 '(2) '(1 3) '(4))

    (set-intersect! ms (mset-A 1 2) (mset-A 2 3))
    (define s5 (set-intersect s4 (set-A 1 2) (set-A 2 3)))
    (t1 ms s5 '(2) '() '(1 3 4))
    (t1 ms s5 '() '(2) '(1 3 4))

    (set-symmetric-difference! ms (mset-A 1 2) (mset-A 2 3))
    (define s6 (set-symmetric-difference s5 (set-A 1 2) (set-A 2 3)))
    (t1 ms s6 '(1 3) '(2) '(4))

    (set-subtract! ms (mset-A 1 4) (mset-A 2 4))
    (define s7 (set-subtract s6 (set-A 1 4) (set-A 2 4)))
    (t1 ms s7 '(3) '() '(1 2 4))
    (t1 ms s7 '() '(3) '(1 2 4))

    (void))

  (t mutable-set mutable-seteqv mutable-seteq set seteqv seteq)
  (t mutable-seteqv mutable-seteq mutable-set seteqv seteq set)
  (t mutable-seteq mutable-set mutable-seteqv seteq set seteqv))

(test "#<set: 1>" 
      'print-set1
      (let ([sp (open-output-string)])
        (write (set 1) sp)
        (get-output-string sp)))

(test "#<seteq: 1>" 
      'print-set1
      (let ([sp (open-output-string)])
        (write (seteq 1) sp)
        (get-output-string sp)))

(test "#<seteqv: 1>" 
      'print-set1
      (let ([sp (open-output-string)])
        (write (seteqv 1) sp)
        (get-output-string sp)))

;; ----------------------------------------

(test (set 1 2 3) 'for/set (for/set ([i '(0 1 2)]) (add1 i)))

(test (set 1 2 3) 'for/set (for/set ([i '(0 1 2 3 4)]) 
                                    #:break (= i 3)
                                    (add1 i)))

(test (set 1 2 3) 'for/set (for/set ([i '(0 1 2 3 4)]) 
                                    #:final (= i 2)
                                    (add1 i)))

(test (mutable-set 1 2 3)
      'for/mutable-set
      (for/mutable-set ([i '(0 1 2)]) (add1 i)))

(test (mutable-set 1 2 3)
      'for/mutable-set
      (for/mutable-set ([i '(0 1 2 3 4)]) 
                       #:break (= i 3)
                       (add1 i)))

(test (mutable-set 1 2 3)
      'for/mutable-set
      (for/mutable-set ([i '(0 1 2 3 4)]) 
                       #:final (= i 2)
                       (add1 i)))

;; ----------------------------------------

(report-errs)

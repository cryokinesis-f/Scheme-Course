#lang scheme
(define root car)
(define left cadr)
(define right caddr)
;task1
(define (graph-reverser graph)
  (define (spisok-rebra G n res)
    (define (iter Gr)
      (if (empty? Gr) '()
          (cons (list n (car Gr)) (iter (cdr Gr)))))
    (if (empty? G) (cons n res)
        (spisok-rebra (cdr G) (+ n 1) (append (iter (car G)) res))))
  (define promres (spisok-rebra graph 0 '()))
  (define vert (car promres))
  (define rebras (cdr promres))
  (define (general-iter rebra k result)
    (define (iter-for1 rebr ostr res)
      (if (empty? rebr) (cons ostr res)
          (if (equal? (cadr (car rebr)) k) (iter-for1 (cdr rebr) ostr (cons (caar rebr) res))
              (iter-for1 (cdr rebr) (cons (car rebr) ostr) res))))
    (if (= k -1) result
        (let* ((promres (iter-for1 rebra '() '())) (newrebra (car promres)) (resu (cdr promres)))
          (general-iter newrebra (- k 1) (cons resu result)))))
  (if (equal? graph '((0))) '((0)) (take (general-iter rebras vert '()) vert)))

;task2
(define (comb G)
  (define vert (length G))
  (define (i1 a resa)
    (define (i2 b resb)
      (define (i3 c resc)
        (cond ((= c vert) (i2 (+ b 1) (append resb resc)))
              ((and (member b (list-ref G a)) (member c (list-ref G a)) (member c (list-ref G b))) (i3 (+ c 1) (cons (list a b c) resc)))
              (else (i3 (+ c 1) resc))))
      (cond ((= b (- vert 1)) (i1 (+ 1 a) (append resa resb)))
            (else (i3 (+ b 1) '()))))
    (cond ((= a (- vert 2)) resa)
          (else (i2 (+ a 1) '()))))
  (define result (i1 0 '()))
  (if (empty? result) #f result))
        

;task3 binary-tree --> graph
(define (z3 t)
  (define (tree->graph tree predroot)
    (if (empty? tree) '()
        (cond ((and (empty? (left tree)) (empty? (right tree))) (list (list (root tree) predroot)))
              ((empty? (left tree)) (if (empty? predroot) (append (list (list (root tree) (root (right tree)))) (tree->graph (right tree) (root tree)))
                                        (append (list (list (root tree) predroot (root (right tree)))) (tree->graph (right tree) (root tree)))))
              ((empty? (right tree)) (if (empty? predroot) (append (list (list (root tree) (root (left tree)))) (tree->graph (left tree) (root tree)))
                                         (append (list (list (root tree) predroot (root (left tree)))) (tree->graph (left tree) (root tree)))))
              (else (if (empty? predroot) (append  (list (list (root tree) (root (left tree)) (root (right tree)))) (tree->graph (left tree) (root tree)) (tree->graph (right tree) (root tree)))
                        (append (list (list (root tree) predroot (root (left tree)) (root (right tree)))) (tree->graph (left tree) (root tree)) (tree->graph (right tree) (root tree))))))))
  (define (quick-sort lst)
    (cond ((empty? lst) empty)
          (else (append (quick-sort (filter (λ (a) (char<? (car a) (caar lst))) lst))
                        (filter (λ (a) (equal? (car a) (caar lst))) lst)
                        (quick-sort (filter (λ (a) (char>? (car a) (caar lst))) lst ))))))
  (define graph (quick-sort (tree->graph t '())))
  (write graph)
  (define (ij dict lst n)
    (if (empty? lst) dict
        (ij (dict-set dict (caar lst) n) (cdr lst) (+ n 1))))
  (define dic (ij '() graph 0))
  (if (empty? t) '()
      (if (and (empty? (left t)) (empty? (right t))) (list (list (root t)))
          (map (λ (x) (cons (car x) (map (λ (y) (dict-ref dic y)) (cdr x)))) graph))))
          



                      
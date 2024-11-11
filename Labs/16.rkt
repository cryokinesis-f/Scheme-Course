#lang scheme
;16
;helpers
(define (left tree)
  (if (or (empty? tree) (not tree)) tree (car tree)))

(define (right tree)
  (if (or (empty? tree) (not tree)) tree (cdr tree)))

(define (leaf? tree)
  (equal? (left tree) 'leaf))

(define (make-leaf symbol)
  (cons 'leaf symbol))

;task1
(define (prefixed-or-suffixed? tab)
  (if (or (prefixed? tab) (suffixed? tab)) #t #f))

(define (prefixed? table)
    (define (tree-make symbol route tree)
      (cond ((leaf? tree) #f)
            ((empty? route) (if (empty? tree) (make-leaf symbol) #f))
            ((= 0 (car route))
             (let ((l (tree-make symbol (cdr route) (left tree))))
               (if l (cons l (right tree)) #f)))
            ((= 1 (car route))
             (let ((r (tree-make symbol (cdr route) (right tree))))
               (if r (cons (left tree) r) #f)))
            (else #f)))
    (foldl tree-make '() (map car table) (map cdr table)))

(define (suffixed? table2)
    (prefixed? (map (λ (x) (cons (car x) (reverse (cdr x)))) table2)))


;task2
(define (z2 tab)
  (define (tree_p table)
    (prefixed? table))
  (define (tree_s table)
      (suffixed? table))
  (define (plus-symbol symbol code tree)
    (if (empty? code) (cons 'leaf symbol)
        (if (= 0 (car code))
            (cons (plus-symbol symbol (cdr code) (if (empty? tree) '() (left tree)))
                  (if (empty? tree) '() (right tree)))
            (cons (if (empty? tree) '() (left tree))
                  (plus-symbol symbol (cdr code) (if (empty? tree) '() (right tree)))))))
   (let ((p (tree_p tab))) (if p (foldl (λ (x res) (plus-symbol (car x) (cdr x) res)) '() tab)
                              (let ((s (tree_s tab)))
                                (if s
                                    (foldl (λ (x res) (plus-symbol (car x) (cdr x) res)) '() (map (λ (x) (cons (car x) (reverse (cdr x)))) tab)) #f)))))

  
  
;task3
(define (z3 tab)
  (define (tree_p table)
    (prefixed? table))
  (define (tree_s table)
      (suffixed? table))
  (define (func tree)
    (if (empty? tree) #f
        (if (leaf? tree) #f
            (if (and (leaf? (left tree))(leaf? (right tree))) #f
        (if (and (leaf? (left tree)) (not (leaf? (right tree)))) (cdr (left tree))
            (if (and (leaf? (right tree)) (not (leaf? (left tree)))) (cdr (right tree))
                (or (func (left tree)) (func (right tree)))))))))
  (let ((p (tree_p tab))) (if p (func p)
                              (let ((s (tree_s tab))) (if s (func s) #f)))))















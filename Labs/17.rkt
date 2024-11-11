#lang scheme
;17
(define left cadr)
(define right caddr)
(define root car)
;task1
(define (tree-map func tree)
  (if (empty? tree) '()
      (cons  (func (car tree)) (list (tree-map func (left tree)) (tree-map func (right tree))))))

;task2
(define (fold-tree func res tree)
  (if (empty? tree) res
      (fold-tree func (func (root tree) (fold-tree func res (left tree))) (right tree))))
;task3
(define (rezak tree)
  (define (z3 tree)
  (cond ((and (empty? (left tree)) (empty? (right tree))) (list (car tree) '() '()))
        ((and (empty? (left tree)) (not (empty? (right tree)))) (z3 (right tree)))
        ((and (empty? (right tree)) (not (empty? (left tree)))) (z3 (left tree)))
        (else (list (car tree) (z3 (left tree)) (z3 (right tree))))))
  
  (cond ((empty? tree) '())
        ((and (empty? (left tree)) (empty? (right tree))) (list (car tree) '()'()))
        ((and (not (empty? (left tree))) (empty? (right tree))) (list (car tree) (z3 (left tree)) '()))
        ((and (not (empty? (right tree))) (empty? (left tree))) (list (car tree) '() (z3 (right tree))))
        (else (list (car tree) (z3 (left tree)) (z3 (right tree))))))


(define (rezak.v2 tree)
  (define (z3 tree)
    (cond ((and (empty? (left tree)) (empty? (right tree))) (list (car tree) '() '()))
          ((and (empty? (left tree)) (not (empty? (right tree)))) (z3 (right tree)))
          ((and (empty? (right tree)) (not (empty? (left tree)))) (z3 (left tree)))
          (else (list (car tree) (z3 (left tree)) (z3 (right tree))))))
  
  (cond ((empty? tree) '())
        (else (z3 tree))))

;task4
;(define (newl lst)
  ;  (define symbol (car (filter (λ (x) (= (car x) (peresech lst))) lst)))
   ; (append (list symbol) (remove symbol lst)))

(define (build-tree lst0)
  
  (define (peresech lst)
    (define para (foldl (λ (x res) (cons (cons (car x) (car res)) (cons (cdr x) (cdr res)))) (cons '() '()) lst))
    (define first (car para))
    (define second (cdr para))
    (define (one first second)
      (if (member (car first) second) (one (remove (car first) first) second)
          (car first)))(one first second))

  (define (dictionary lst dic)
    (if (empty? lst) dic
        (dictionary (cdr lst) (dict-set dic (car (car lst)) (cdr (car lst))))))

  (define (make-tree dict tree)

    (if (empty? dict) (list (root tree) '() '())
        (let ((d1 (dict-ref dict (root tree) '())))
          (let ((dict1 (dict-remove dict (root tree))))
            (let ((d2 (dict-ref dict1 (root tree) '())))
              (let ((dict2 (dict-remove dict1 (root tree))))
                (list (root tree) (if (empty? d1) '() (make-tree dict2 (list d1 '()'()))) (if (empty? d2) '() (make-tree dict2 (list d2 '()'()))))))))))
  (if (empty? lst0) '() (make-tree (dictionary lst0 '()) (list (peresech lst0) '() '()))))
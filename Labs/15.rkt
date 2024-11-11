#lang scheme
;15

(define left cadr)
(define right caddr)
(define uzel car)
;task1
(define (divis tree)
  (if (empty? tree) #f
      (if (and (empty? (left tree)) (empty? (right tree))) #f
          (if (empty? (left tree))
              (let ((root (car tree)) (r (car (right tree))))
                (cond ((and (not (= 0 r)) (= 0 (remainder root r))) root)
                      (else (or (divis (right tree)) (divis (left tree))))))
              (let ((root (car tree)) (l (car (left tree))))
                (cond ((and (not (= 0 l)) (= 0 (remainder root l))) root)
                      (else (or (divis (left tree)) (divis (right tree))))))))))

(define (divis.v2 tree)
  (if (empty? tree) #f
      (if (and (empty? (left tree)) (empty? (right tree))) #f
          (if (empty? (left tree))
              (let ((root (car tree)) (r (car (right tree))))
                (cond ((and (not (= 0 r)) (= 0 (remainder root r))) root)
                      (else (or (divis.v2 (right tree)) (divis.v2 (left tree))))))
              (if (empty? (right tree))
                  (let ((root (car tree)) (l (car (left tree))))
                    (cond ((and (not (= 0 l)) (= 0 (remainder root l))) root)
                          (else (or (divis.v2 (left tree)) (divis.v2 (right tree))))))
                  (let ((root (car tree)) (l (car (left tree))) (r (car (right tree))))
                    (cond ((and (and (not (= 0 l)) (not (= 0 r))) (or (= 0 (remainder root r)) (= 0 (remainder root l)))) root)
                          ((and (not (= 0 l)) (= 0 (remainder root l))) root)
                          ((and (not (= 0 r)) (= 0 (remainder root r))) root)
                          (else (or (divis.v2 (left tree)) (divis.v2 (right tree)))))))))))
              
;task2
(define (inverse-tree tree)
  (if (empty? tree) '()
      (if (and (empty? (left tree)) (empty? (right tree))) (list (car tree) '() '())
          (append (list (car tree)) (list (inverse-tree (right tree))) (list (inverse-tree (left tree)))))))
;task3
(define (count-left tree)
  (if (empty? tree) 0
      (if (and (empty? (left tree)) (empty? (right tree))) 0
          (cond ((empty? (left tree)) (+ (count-left (right tree))))
                (else (+ 1 (count-left (left tree)) (count-left (right tree))))))))

;task4
(define (min_of_leaves t)
  (define (func tree)
  (if (empty? tree) +inf.0
      (if (and (empty? (left tree)) (empty? (right tree))) (car tree)
          (min (func (left tree)) (func (right tree))))))
  (if (empty? t) +inf.0 (exact->inexact (func t))))

;task5
(define (skleyshik lst1 lst2)
  (if (and (empty? lst1) (empty? lst2)) '()
      (cond ((empty? lst1) (append (list (car lst2)) (skleyshik lst1 (cdr lst2))))
            ((empty? lst2) (append (list (car lst1)) (skleyshik (cdr lst1) lst2)))
            (else (append (list (append (car lst1) (car lst2))) (skleyshik (cdr lst1) (cdr lst2)))))))
(define (z5 t)
  (define (fact tree)
    (if (empty? tree) '()
        (if (not (and (empty? (left tree)) (empty? (right tree))))
            
            (skleyshik
             (if (empty? (left tree)) (fact (left tree))
                 (append (list (list (car (left tree)))) (fact (left tree))))
             (if (empty? (right tree)) (fact (right tree))
                 (append (list (list (car (right tree)))) (fact (right tree)))))
            
            (skleyshik (fact (left tree)) (fact (right tree))))))
  (if (empty? t) #f (cons (list (car t)) (fact t))))















      


         
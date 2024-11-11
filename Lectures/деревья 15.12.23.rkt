#lang scheme

;lecture 15.12.23
(define entry car)

(define left cadr)

(define right caddr)

(define make-tree list)

;наличие элемента в дереве

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left set)))
        ((> x (entry set))
         (element-of-set? x (right set)))))

;добавление элемента в дерево

(define (add-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) (add-set x (left set)) (right set)))
        (else
         (make-tree (entry set) (left set) (add-set x (right set))))))
  
;удаление элемента из дерева

(define (del-set x set)
  (define (find-del-min set)
    (if (null? (left set))
        (cons (entry set) (right set))
        (let ((res (find-del-min (left set))))
          (cons (car res) (make-tree (entry set) (cdr res) (right set))))))
  (cond ((null? set) set)
        ((= x (entry set))
         (if (null? (right set))
             (left set)
             (if (null? (left set))
                 (right set)
                 (let ((res (find-del-min (right set))))
                       (make-tree (car res)
                                  (left set)
                                  (cdr res))))))
        ((< x (entry set))
         (make-tree (entry set) (del-set x (left set))
                    (right set)))
        (else
         (make-tree (entry set)
                    (left set)
                    (del-set x (right set))))))
         
;создание дерева из списков
;балансировка дерева описанным ниже способом не является качественной, foldl не лучший выбор.
(define (list->set lst)
  (foldl add-set '() lst))

;несколько слов о ormap и andmap
;единичная матрица размера n на n
(define (EdM n)
  (build-list n
              (λ (i) (build-list n (λ (j) (if (= i j) 1 0))))))
;транспоирование
(define (transponirovat A)
  (apply map list A))







              

        
             
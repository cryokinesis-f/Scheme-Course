#lang scheme
;14

(define left cadr)
(define right caddr)

;task1

(define (troyki lst)
  (define (steki stack0 stack1 stack2 ostlst)
    (if (empty? ostlst) (list stack0 stack1 stack2)
        (let ((ind (remainder (car ostlst) 3)))
        (cond ((= ind 0) (steki (cons (car ostlst) stack0) stack1 stack2 (cdr ostlst)))
              ((= ind 1) (steki stack0 (cons (car ostlst) stack1) stack2 (cdr ostlst)))
              ((= ind 2) (steki stack0 stack1 (cons (car ostlst) stack2) (cdr ostlst)))))))
  (define promres (steki '() '() '() lst))
  (define (troyka-maker lst0 lst1 lst2 res)
    (if (or (empty? lst0) (empty? lst1) (empty? lst2)) res
        (troyka-maker (cdr lst0) (cdr lst1) (cdr lst2) (cons (list (car lst0) (car lst1) (car lst2)) res))))
  (troyka-maker (car promres) (cadr promres) (caddr promres) '()))

;task2

(define (unlimited_member ZNACH LS)
  (let ((a (member ZNACH LS)))
    (if a (unlimited_member ZNACH (if a (cdr a) (cdr LS))) LS)))


(define (katalogi lst)
  (define (root-dropper ostlst)
    (if (or (empty? ostlst) (not (equal? (cdr (string->list (car ostlst))) '()))) ostlst
        (root-dropper (cdr ostlst))))
  
  (define spisok (root-dropper lst))
  
  (define (rec ostlst)
    (if (empty? ostlst) '()
    (cons (car ostlst) (rec (unlimited_member (car ostlst) (cdr ostlst))))))
  
  (define promres (if (empty? spisok) #f
      (cons (car spisok) (rec (cdr spisok)))))

  (define (route lst)
    (if (empty? (cdr lst)) (car lst)
        (if (and (not (empty? (cdr (string->list (car lst))))) (equal? (cadr (string->list (car lst))) #\:))
            (string-append (car lst) (route (cdr lst)))
            (string-append (car lst) "\\" (route (cdr lst))))))
  (if promres (route promres) #f))



;task3
(define (maximum t)
  (define (maxel tree)
  (if (empty? tree) (cons -inf.0 -inf.0)
      (let ((l (maxel (left tree))) (r (maxel (right tree))))
        (cond ((= (car tree) (car l) (car r)) (cons (car tree) (+ 1 (cdr l) (cdr r))))
              ((> (car tree) (max (car l) (car r))) (cons (car tree) 1))
              ((< (car tree) (max (car l) (car r))) (cond ((> (car l) (car r)) l)
                                                          ((< (car l) (car r)) r)
                                                          ((= (car l) (car r)) (cons (car l) (+ (cdr l) (cdr r))))))
              ((= (car tree) (car l)) (cons (car tree) (+ 1 (cdr l))))
              ((= (car tree) (car r)) (cons (car tree) (+ 1 (cdr r))))))))
  (if (empty? t) 0 (cdr (maxel t))))
;task4

(define (leaves tree)
  (if (empty? tree) 0
      (if (and (empty? (left tree)) (empty? (right tree))) 1
          (+ (leaves (right tree)) (leaves (left tree))))))
;task5

(define (potomki tree)
  (if (empty? tree) #t
      (if (and (empty? (left tree)) (empty? (right tree))) #t
          (if (or (empty? (left tree)) (empty? (right tree))) #f
              (and (potomki (left tree)) (potomki (right tree)))))))

(define (potomki.v2 tree)
  (cond ((empty? tree) #t)
        ((and (empty? (left tree)) (empty? (right tree))) #t)
        ((or (empty? (left tree)) (empty? (right tree))) #f)
        (else (and (potomki.v2 (left tree)) (potomki.v2 (right tree))))))

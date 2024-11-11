#lang scheme
;task3
(define (quicker-sort lst)
  (define (razbienie ostlst a lev chislo prav)
    (if (empty? ostlst) (list lev chislo prav)
        (cond ((< (car ostlst) a) (razbienie (cdr ostlst) a (cons (car ostlst) lev) chislo prav))
              ((= (car ostlst) a) (razbienie (cdr ostlst) a lev (cons (car ostlst) chislo) prav))
              ((> (car ostlst) a) (razbienie (cdr ostlst) a lev chislo (cons (car ostlst) prav))))))
  (if (empty? lst) empty
      (let ((peremennaya (razbienie lst (car lst) '() '() '())))
        (append (quicker-sort (car peremennaya)) (cadr peremennaya) (quicker-sort (caddr peremennaya))))))



(define (dop3 Gr)
  (define G (map quicker-sort Gr))
  (define maxv (- (length G) 1))
  (define (iskl number-of-string lst1 k n)
    (if (> k n) '()
        (if (empty? lst1)
            (if (= k number-of-string) (iskl number-of-string lst1 (+ k 1) n)
                (cons k (iskl number-of-string lst1 (+ k 1) n)))
            (cond ((= k number-of-string (car lst1)) (cons k (iskl number-of-string (cdr lst1) (+ k 1) n)))
                  ((= k (car lst1)) (iskl number-of-string (cdr lst1) (+ k 1) n))
                  ((= k number-of-string) (iskl number-of-string lst1 (+ k 1) n))
                  (else (cons k (iskl number-of-string lst1 (+ k 1) n)))))))
  (define (rec l ostg)
    (if (empty? ostg) '()
        (append (list (iskl l (car ostg) 0 maxv)) (rec (+ l 1) (cdr ostg)))))
  (rec 0 G))
;dfs
(define (DFS a b G)
  (define (iter prosm stack l)
    (if (> (list-ref prosm a) 0) l
        (if (empty? stack) #f
            (let* ((pos (car stack))
                   (next (foldl (λ (x y) (if (equal? y #f)
                                             (if (= 0 (list-ref prosm x))x #f)
                                             y))
                                #f (list-ref G pos)))
                   (step (list-ref prosm pos)))
              (if (equal? next #f) (iter prosm (cdr stack) (cons next l))
                  (iter (append (take prosm next)
                                (cons (+ step 1) (drop prosm (+ next 1))))
                        (cons next stack) (cons next l)))))))
  (iter (build-list (length G) (λ (i) (if (= i b) 1 0))) (list b)'()))
;task4

   


        
       
  
        
        





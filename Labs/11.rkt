#lang scheme
;scheme 11

;task1

(define (z1 a b)
  
  (define (kratn? chislo q)
    (define koren (sqrt chislo))
    (if (= 0 (remainder chislo (* q q))) #t
          (if (> q koren) #f
          (kratn? chislo (+ q 1)))))
  
  (define (func now res)
    (if (> now b) res
        (func (+ now 1) (if (kratn? now 2) (cons now res) res))))
  
  (func a '()))

;task1.v2

(define (z1.v2 a b)
  
  (define (kratn? chislo q)
    (define koren (sqrt chislo))
    (if (= 0 (remainder chislo (* q q))) #t
          (if (> q koren) #f
          (kratn? chislo (+ q 1)))))
  
  (define (func now res)
    (if (< now a) res
        (func (- now 1) (if (kratn? now 2) (cons now res) res))))
  
  (func b '()))

;task2
(define (z2 lst)
  
  (define (fiblist ostlst fib1 fib2 nomer_v_spiske sum)
    (if (empty? ostlst) sum
        (if (> (max fib1 fib2) nomer_v_spiske)
            (fiblist (cdr ostlst) fib1 fib2  (+ 1 nomer_v_spiske) sum)
            (fiblist (cdr ostlst) fib2 (+ fib1 fib2) (+ 1 nomer_v_spiske) (+ sum (car ostlst))))))
  
  (fiblist lst 1 1 0 0))

;task3 ; не работает
(define (z3 lst)
  
  (define (sum_del x)
    (define (f a sum)
      (if (> a (sqrt x))
          (- sum x)
          (if (= 0 (remainder x a))
              (f (+ 1 a) (+ a (/ x a) sum))
              (f (+ 1 a) sum))))
    (f 1 0))

  (define (func ostlst res) ;начальное res #f
    (define pos (sum_del (car ostlst)))
    (define fir (car ostlst))
    (define sec (cdr ostlst))
    (if (and (not (empty? (cdr sec))) (equal? res #f))
        (func sec (if (ormap (λ (x) (and (= pos x) (= fir (sum_del x)))) sec)
                               (cons (car ostlst) (findf (λ (x) (= pos (sum_del x))) sec))
                               res))
        res))
  
  (if (or (empty? lst) (empty? (cdr lst))) #f (func lst #f)))

;task3.v2

(define (z3.v2 lst)
  
  (define (sum_del x)
    (define (f a sum)
      (if (> a (sqrt x))
          (- sum x)
          (if (= 0 (remainder x a))
              (f (+ 1 a) (+ a (/ x a) sum))
              (f (+ 1 a) sum))))
    (f 1 0))

  (define (func ostlst res)
    (define fir (car ostlst))
    (define pos (sum_del (car ostlst)))
    (if (and (not (empty? (cdr ostlst))) (equal? res '(#f)))
        (func (cdr ostlst) (foldl (λ (x y) (if (and (= pos x) (= fir (sum_del x))) (cons (cons (car ostlst) x) y) y)) res (cdr ostlst)))
        res))
  
  (if (or (empty? lst) (empty? (cdr lst))) #f (car (func lst '(#f)))))

;task5 (andmap (λ (a) (or (= a 0) (= a 1) (= a 2) (= a 3) (= a 4) (= a 5) (= a 6) (= a 7) (= a 8) (= a 9)))

(define (z5 matrix)
  
  (define (func ostmatrix res)
    (if (empty? ostmatrix) (if (empty? res) #f (car res))
        (func (cdr ostmatrix) (if (andmap (λ (a) (or (equal? a 0) (equal? a 1) (equal? a 2) (equal? a 3) (equal? a 4)
                                                     (equal? a 5) (equal? a 6) (equal? a 7) (equal? a 8) (equal? a 9))) (car ostmatrix))
                               (let ((p (foldl * 1 (car ostmatrix)))) (if (<= p (cdr res)) (cons (car ostmatrix) p) res)) res))))
  
  (if (empty? matrix) #f (func matrix (cons '() +inf.0))))

;task4
(define (z4 lst)
  
  (define (troyka x1 x2 x3)
    (or (and (not (or (= x2 0) (= x1 0))) (= (/ x3 x2) (/ x2 x1))) (= x1 x2 x3 0)))
  
  (define (func ostlst a1 a2 a3 res)
    (if (empty? ostlst)
        (if (troyka a1 a2 a3)
            (cons (cons a1 (cons a2 (cons a3 '()))) res)
            res)
        (if (troyka a1 a2 a3)
            (if (or (empty? (cdr ostlst)) (empty? (cddr ostlst)))
                (if (troyka a1 a2 a3)
            (cons (cons a1 (cons a2 (cons a3 '()))) res)
            res)
                        (func (cdddr ostlst) (car ostlst) (cadr ostlst) (caddr ostlst) (cons (cons a1 (cons a2 (cons a3 '()))) res)))
            (func (cdr ostlst) a2 a3 (car ostlst) res))))
  
  (if (or (empty? lst) (empty? (cdr lst)) (empty? (cddr lst))) '()
      (func (cdddr lst) (car lst) (cadr lst) (caddr lst) '())))

;task4.v2

(define (z4.v2 lst)
  
  (define (troyka x1 x2 x3)
    (or (and (not (or (= x2 0) (= x1 0))) (= (/ x3 x2) (/ x2 x1))) (= x2 x3 0)))

  (define (func ostlst a1 a2 a3 res)
    (if (empty? ostlst)
      (if (troyka a1 a2 a3)
            (cons (cons a1 (cons a2 (cons a3 '()))) res)
            res)
      (func (cdr ostlst) a2 a3 (car ostlst) (if (troyka a1 a2 a3)
            (cons (cons a1 (cons a2 (cons a3 '()))) res)
            res))))
  
  (if (or (empty? lst) (empty? (cdr lst)) (empty? (cddr lst))) '()
      (func (cdddr lst) (car lst) (cadr lst) (caddr lst) '())))
  
  
  








  

               
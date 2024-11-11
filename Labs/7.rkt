#lang scheme
;task1
(define (del_na_nomer lst)
  (define (nomera lst)
    (if (empty? lst) '()
    (reverse (foldl (λ (x y) (cons (cons x (+ 1 (cdr (car y)))) y)) (list (cons (car lst) 1)) (cdr lst)))))
  (define lst1 (nomera lst))
  (filter (λ (x) (if (empty? lst) #t
                     (or (= 1 (cdr x)) (not (equal? (gcd (car x) (cdr x)) 1))))) lst1))

;task2  

(define (sum_treh lst)
  (foldl (λ (x y) (if (and (> 1000 x) (> x 99)) (+ y x) (+ y 0))) 0 lst))
  
;task3

(define (resultat list_of_numbers list_of_operations)
      (if (empty? list_of_numbers) (error "function RESULTAT can't be used without any arguments")
          (if (empty? list_of_operations) (car list_of_numbers)
  (foldl (λ (x y z) (eval (list y z x))) ((eval (car list_of_operations)) (car list_of_numbers)) (cdr list_of_numbers) list_of_operations))))

;task4

(define (f lst)
  (define g (reverse lst))
  (if (empty? lst)
      '()
      (foldl (λ (x lst1) (cons (cons x (car lst1)) lst1)) (list (list (car g))) (cdr g))))


;скалярное произведение

(define (scalar lst1 lst2)
  (foldl (λ (x y z) (+ (* x y) z)) 0 lst1 lst2))

(define (scalar_v0 lst1 lst2)
  (foldl + 0 (map * lst1 lst2)))

(define (scalar_v3 lst1 lst2)
  (apply + (map * lst1 lst2)))

;список с числами, делаем список из минимальных цифр первоначального числа
(define (mincifri lst)
  (define (f otvet ostx)
    (if (= 0 ostx) otvet
        (f (if (< otvet (remainder ostx 10)) otvet (remainder ostx 10)) (quotient ostx 10))))
  (reverse (foldl (λ (x y) (cons (f 9 x) y)) '() lst)))

;на вход число натуральное n, делаем список из n элементов, где стоят остатки от деления n на номер
;build-list  <number> <values>(func)

(define (func n)
  (define numbers (build-list (+ 1 n) values))
  (reverse (foldl (λ (x y) (cons (remainder n x) y)) '() (cdr numbers))))


;дают начальный элемент и количесвто элементов
; четные /2
; нечетные *3+1
(define (sir n colvo)
  (define nomera (cdr (build-list (+ 1 colvo) values)))
  (reverse (foldl (λ (y x) (if (= 0 (remainder (car x) 2))
                      (cons (quotient (car x) 2) x)
                      (cons (+ 1 (* 3 (car x))) x))) (list n) (cdr nomera))))

;make-list 5 3
;проверяет есть ли нули в списке
(define (nuli? lst1)
  (ormap zero? lst1))
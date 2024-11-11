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

;task5

(define (final начальное_состояние процентная_ставка протокол_банковских_операций)
  ;протокол банковских операций -- список списков вида ((3) (+) (2500))
  ;то есть (номер дня в месяце, увеличение или уменьшение значения, сумма на которую происходят изменения
  (define множитель (+ 1 (* процентная_ставка 0.01)))
  (define исходник (* начальное_состояние (expt множитель 30)))
  (define (f lst)
    (define data (car lst))
    (* ((eval (cadr lst)) (eval (caddr lst))) (if (and (> data 0) (>= 30 data)) (expt множитель (- 30 data)) 0)))
  (define (g a b)
    (+ (f a) b))
  (define изменения_по_счету (foldl g 0 протокол_банковских_операций))
  (+ исходник изменения_по_счету))
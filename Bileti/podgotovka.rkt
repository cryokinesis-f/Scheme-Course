#lang scheme
(define (posledovatel n)
  (define (f n1 res)
    (define (chetn n2)
      (if (= 0 (remainder n2 2)) (- 0 (/ 1 n2)) (/ 1 n2)))
    (if (= n1 0) res
        (f (- n1 1) (+ res (chetn n1)))))
  (f n 0))

(define (position k lst)
  (define majoranta (expt 10 k))
  (define minoranta (quotient majoranta 10))
  (define res (cdr (foldl
                    (λ (x y) (if (and (<= minoranta x) (< x majoranta))
                                 (cons (+ (car y) 1) (car y))
                                 (cons (+ (car y) 1) (cdr y))))
         (cons 0 -100) lst)))
  (if (>= res 0) res #f))

(define (prostoe x)
  (define (iter div)
    (cond ((= x 1) #f)
          ((> div (sqrt x)) #t)
          ((zero? (modulo x div)) #f)
          (else (iter (+ div 1)))))
  (iter 2))




  

(define (factorizacia n1)
  
  (define p (+ 1 (quotient n1 2)))
  
  (define (perehod_k_prostomu a)
    (define (f a1)
      (if (prostoe a1) a1
          (f (+ 1 a1))))(f (+ 1 a)))
  
  (define (kolvo_vhozhdeniy del1 chislo)
    (define (f ch k)
    (if (> (remainder ch del1) 0) k
        (f (quotient ch del1) (+ k 1))))(f chislo 0))
  
  (define (del_na_kratnost chis delitel)
    (if (> (remainder chis delitel) 0) chis
        (del_na_kratnost (quotient chis delitel) delitel)))
  
  (define (f n del res)
    (if (and (< del p) (= n 1)) res
        (if (> (remainder n del) 0)
            (f n (perehod_k_prostomu del) res)
            (f (del_na_kratnost n del) (perehod_k_prostomu del) (cons (cons del (kolvo_vhozhdeniy del n)) res)))))
  (if (prostoe n1) (f 1 2 (list (cons n1 1)))(f n1 2 '())))

(define (perehod_k_prostomu a)
    (define (f a1)
      (if (prostoe a1) a1
          (f (+ 1 a1))))(f (+ 1 a)))


(define (kanonicheskoe chislo)
  (define (f ch res)
    (if (= ch 1) (+ (* res 10) (remainder ch 10))
        (f (quotient ch 10) (+ (* res 10) (remainder ch 10)))))(f chislo 0))

(define (sxema x k1 . list_of_koefficentov)
  (define (iter lst_of_kef res)
    (if (empty? lst_of_kef) res
        (iter (cdr lst_of_kef) (+ (* x res) (car lst_of_kef)))))(iter list_of_koefficentov k1))
  
  



















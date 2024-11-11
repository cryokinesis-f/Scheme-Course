#lang scheme
;13.1
;task1
(define (der pol)
  (define p (- (length pol) 1))
  (define (func ex ostpol)
    (if (empty? (cdr ostpol)) '()
        (cons (* ex (car ostpol)) (func (- ex 1) (cdr ostpol)))))
    (if (empty? (cdr pol)) '(0) (func p pol)))

;task2
(define (summa pol1 pol2)
  (define p1 (reverse pol1))
  (define p2 (reverse pol2))
  (define (iter 0p1 0p2 res)
    (cond ((and (empty? 0p1) (empty? 0p2)) res)
          ((empty? 0p1) (iter 0p1 (cdr 0p2) (cons (car 0p2) res)))
          ((empty? 0p2) (iter (cdr 0p1) 0p2 (cons (car 0p1) res)))
          (else (iter (cdr 0p1) (cdr 0p2) (cons (+ (car 0p1) (car 0p2)) res)))))
  (define (joter p)
      (if (empty? p) '(0)
    (if (not (= (car p) 0)) p
        (joter (cdr p)))))
  (joter (iter p1 p2 '())))

;task3
(define (pol_mult pol1 pol2)
  (define (iter 0p1 res)
    (if (empty? 0p1) res
        (iter (cdr 0p1) (if (empty? (cdr 0p1))
                            (summa (map (λ (x) (* x (car 0p1))) pol2) res)
                            (append (summa (map (λ (x) (* x (car 0p1))) pol2) res) '(0))))))
  (iter pol1 '()))


;task4
(define  (crossbasement k . num-list)

  (define (polinom-maker x)
    (define (iter chislo res)
      (if (= chislo 0) res
          (iter (quotient chislo 10) (cons (remainder chislo 10) res))))
    (iter x '()))

  (define (evaluate pol0 res)
    (if (empty? pol0) res
        (evaluate (cdr pol0) (+ (car pol0) (* k res)))))

  (define (converter_from_10 numeral)
    (define (iter i ostnum)
      (if (< ostnum k) (* i ostnum)
          (+ (* i (remainder ostnum k)) (iter (* i 10) (quotient ostnum k)))))
    (iter 1 numeral))
  
  (converter_from_10 (foldl (λ (x res) (+ (evaluate (polinom-maker x) 0) res)) 0 num-list)))


;(define (converter_from_10_another k numeral)
 ;   (define (iter i ostnum)
  ;    (if (< ostnum k) (* (expt 10 i) ostnum)
   ;       (+ (* (expt 10 i) (remainder ostnum k)) (iter (+ i 1) (quotient ostnum k)))))
    ;(iter 0 numeral))

;task5

(define (utochn x1 pol)
  
  (define x0 (exact->inexact x1))
  
  (define f-strih (der pol)) 

  (define (evaluation p koren)
    (define (rec p0 res)
    (if (empty? p0) res
        (rec (cdr p0) (+ (* res koren) (car p0)))))
    (rec p 0))

  (define (next-x xn)
    (define f-ot-xn (evaluation pol xn))
    (define f-strih-ot-xn (evaluation f-strih xn))
    (- xn (/ f-ot-xn f-strih-ot-xn)))

  (define (func xn xn1)
    (if (< (abs (- xn xn1)) 0.0000000000000001) xn1
        (func xn1 (- xn1 (/ (evaluation pol xn1) (evaluation f-strih xn1))))))
    (exact->inexact (func x0 (- x0 (/ (evaluation pol x0) (evaluation f-strih x0))))))
    
  

  

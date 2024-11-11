#lang scheme
(define (permutations lst)
  (define r '())
  (define (help lst1 lst2)
    (define (iter lst)
      (cond ((and (not (empty? (cdr lst))) (not (equal? (car lst) (cadr lst)))) (help (remove (cadr lst) lst1) (cons (cadr lst) lst2))))
      (cond ((not (empty? (cdr lst))) (iter (cdr lst)))))
    (if (empty? lst1) (set! r (cons (reverse lst2) r))
        (iter (cons #f lst1))))
  (and (help (sort lst <) '()) r))

;task1
(define (best-lst lst)
  (define (sumgcd lsto);(pred_chislo . summa)
    (cdr (foldl (λ (x res) (cons x (+ (cdr res) (gcd x (car res))))) (cons (car lsto) 0) (cdr lsto))))

  (define (my-max r l)
    (if (equal? r l) r
        (let ((x (sumgcd l)) (y (sumgcd r)))
          (if (> y x) r l))))
  
  (define (func lst1 lst2)
    (if (empty? lst1) lst2
        (foldl (λ (x res) (my-max res (func (remove x lst1) (append (list x) lst2)))) '(0) lst1)))

  (func lst '()))
        
;task3
(define (generate-chain lst n)
  (define (max-len lst1 lst2)
    (if (> (length lst1) (length lst2)) lst1 lst2))
  (define (iter lst tek-max res)
    (if (empty? lst)
        (reverse (max-len tek-max res))
        (if (= (gcd (car tek-max) (car lst)) 1)
            (if (= (length tek-max) 1)
                (iter (cdr lst) tek-max res)
                (iter lst (list n) (max-len tek-max res)))
            (iter (cdr lst) (cons (car lst) tek-max) res))))
  (define (f l r)
    (if (empty? l) (cdr r)
        (let ((x (length (car l))))
          (if (> x (car r)) (f (cdr l) (cons x (car l)))
              (f (cdr l) r)))))
  (f (foldl (λ (x y) (cons (iter x (list n) (list n)) y)) '() (permutations lst)) '(0 . 0)))
  

;task2
(define (subset lst)
    (define r '())
    (define (tmp lst rez)
      (if (empty? lst) (set! r (cons rez r))
          (and (tmp (cdr lst) rez)
               (tmp (cdr lst) (cons (car lst) rez)) r)))
    (and (tmp lst '()) r))

(define (center dots);ixed - список с абциссами
    (let ((len (length dots)))
      (if (= len 0) '(0 . 0)
          (cons (my-round (/ (apply + (map car dots)) len)) (my-round (/ (apply + (map cdr dots)) len))))))
(define (my-round num)
      (/ (round (* 100 (exact->inexact num))) 100))


(define (z2 lst)
  (define (my-round num)
      (/ (round (* 100 (exact->inexact num))) 100))
  (define subsets (subset lst))
  (define (dlina-from-dot-to-dot A B)
    (let ((xa (car A)) (ya (cdr A)) (xb (car B)) (yb (cdr B)))
      (my-round (sqrt (+ (sqr (- xa xb)) (sqr (- ya yb)))))))
  
  (define (summa-do-centra C lst)
    (foldl (λ (x res) (+ res (dlina-from-dot-to-dot x C))) 0 lst))
  
  (define (center dots);ixed - список с абциссами
    (let ((len (length dots)))
      (if (= len 0) '(0 . 0)
          (cons (my-round (/ (apply + (map car dots)) len)) (my-round (/ (apply + (map cdr dots)) len))))))
  (define (my-max re l)
    (if (> (car re) (car l)) re
        (if (< (car re) (car l)) l
            (append (list re) (list l)))))
  (cdr (foldl (λ (y res) (my-max res (cons (summa-do-centra (center y) y) y))) '(0 '()) subsets)))

(define (z2.new lst)
  (define (my-round num)
      (/ (round (* 100 (exact->inexact num))) 100))
  (define subsets (subset lst))
  (define (dlina-from-dot-to-dot A B)
    (let ((xa (car A)) (ya (cdr A)) (xb (car B)) (yb (cdr B)))
      (my-round (sqrt (+ (sqr (- xa xb)) (sqr (- ya yb)))))))
  (define (my-max l r)
    (if (> (car r) (car l)) r l))
  
  (define (summa-do-centra C lst)
    (foldl (λ (x res) (+ res (dlina-from-dot-to-dot x C))) 0 lst))
  
  (define (center dots);ixed - список с абциссами
    (let ((len (length dots)))
      (if (= len 0) '(0 . 0)
          (cons (my-round (/ (apply + (map car dots)) len)) (my-round (/ (apply + (map cdr dots)) len))))))
  (define max-len (car (foldl (λ (y res) (my-max res (cons (summa-do-centra (center y) y) y))) '(0 . '()) subsets)))
  (map cdr (filter (λ (x) (= (car x) max-len)) (map (λ (x) (let ((y (center x))) (cons (summa-do-centra y x) x))) subsets))))
  
#lang scheme
;task1
(define (generate-pol n)
  (define (my-round y)
    (/ (round (* 1000000000 y)) 1000000000))
  (build-list n (λ (x) (let ((ug (/ (* 2 pi x) n))) (cons (my-round (cos ug)) (my-round (sin ug)))))))

;task2
(define (triangulate pol)
  (define svert (car pol))
  (define ost (cdr pol))
  (define (iter actual next ostp)
    (if (empty? ostp) (cons (list svert actual next) '())
        (cons (list svert actual next) (iter next (car ostp) (cdr ostp)))))
  (iter (car ost) (cadr ost) (cddr ost)))

;task4
;Векторное произведение
(define (vp a b)
  (- (* (car a) (cdr b)) (* (cdr a) (car b))))

(define (composition o1 o2)
  (define (composition1 pol1 pol2)
  (define minx-from1 (apply min (map car pol1)))
  (define verts-from1 (filter (λ (x) (= (car x) minx-from1)) pol1))
  (define miny-from1 (apply min (map cdr verts-from1)))
  (define v1 (cons minx-from1  miny-from1))
  (define minx-from2 (apply min (map car pol2)))
  (define verts-from2 (filter (λ (x) (= (car x) minx-from2)) pol2))
  (define miny-from2 (apply min (map cdr verts-from2)))
  (define v2 (cons minx-from2 miny-from2))
  (define (iteration p1 res)
    (cond ((empty? p1) (reverse res))
          ((equal? (car p1) v1) (append p1 (reverse res)))
          (else (iteration (cdr p1) (cons (car p1) res)))))
  (define (iteration2 p2 res)
    (cond ((empty? p2) (reverse res))
          ((equal? (car p2) v2) (append p2 (reverse res)))
          (else (iteration2 (cdr p2) (cons (car p2) res)))))
  (define newpol1 (iteration pol1 '()))
  (define newpol2 (iteration2 pol2 '()))

  (define (storoni-maker a b)
    (cons (- (car b) (car a)) (- (cdr b) (cdr a))))
  (define (dlina a)
    (sqrt (+ (sqr (car a)) (sqr (cdr a)))))
  (define (iter1 poli1 poli2 koef)
    (if (empty? (cdr poli1)) #t
        (let* ((s1 (storoni-maker (car poli1) (cadr poli1))) (s2 (storoni-maker (car poli2) (cadr poli2))) (k (if (= (dlina s2) 0) koef (/ (dlina s1) (dlina s2)))))
          (and (= 0 (vp s1 s2)) (or (equal? -inf.0 koef) (= koef k)) (iter1 (cdr poli1) (cdr poli2) k)))))
  (iter1 (append newpol1 (list (car newpol1))) (append newpol2 (list (car newpol2))) -inf.0))
  (or (composition1 o1 o2) (composition1 (reverse o1) o2)))
    
  
  

;task5
(define (center-of-circle vert1 vert2 vert3)
  (define x1 (car vert1))
  (define x2 (car vert2))
  (define x3 (car vert3))
  (define y1 (cdr vert1))
  (define y2 (cdr vert2))
  (define y3 (cdr vert3))
  (define v1 (+ (sqr x1) (sqr y1)))
  (define v2 (+ (sqr x2) (sqr y2)))
  (define v3 (+ (sqr x3) (sqr y3)))
  (define y12 (- y1 y2))
  (define y23 (- y2 y3))
  (define y31 (- y3 y1))
  (define x12 (- x1 x2))
  (define x23 (- x2 x3))
  (define x31 (- x3 x1))
  (define vx (+ (* y12 v3) (* y23 v1) (* y31 v2)))
  (define vy (+ (* x12 v3) (* x23 v1) (* x31 v2)))
  (define v (- (* x12 y31) (* y12 x31)))
  (define a (exact->inexact (- 0 (/ vx (* 2 v)))))
  (define b (exact->inexact (/ vy (* 2 v))))
  (cons a b))
  
 
#lang scheme
;bilet3
; Алгоритм двоичного поиска
(define (bin-search a x)
  (define (iter l r)
    (if (= l r)
        (if (= x (vector-ref a l)) l #f)
    (let ((c (quotient (+ l r) 2)))
      (if (<= x (vector-ref a c))
          (iter l c) (iter (+ c 1) r)))))
  (iter 0 (- (vector-length a) 1)))
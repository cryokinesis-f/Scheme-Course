#lang scheme
;task1
(define (summa m1 m2)
  (define (f lst1 lst2)
    (map + lst1 lst2))
  (map f m1 m2))

;task2

(define (neotricatelnost m)
  (define (az el)
    (<= 0 el))
  (foldl (λ (x y) (and (equal? #t (andmap az x)) (equal? #t y))) #t m))


;task3
(define (nizhny m)
  (andmap (λ (stroka i)
            (andmap (λ (x j) (or (>= i j) (= x 0))) stroka
            (build-list (length stroka) values)))
          m (build-list (length m) values)))

;task4

(define (diagonal m)
  (define len (length m))
  (define numerals (build-list len values))
  (define s (foldl (λ (stroka number res)
                     (define el (list-ref stroka number))
                     (define absedel (abs el))
                     (define strbezdiag (- (foldl (λ (x y) (+ (abs x) y)) 0 stroka) absedel))
                     (if (> absedel strbezdiag) (cons (+ 1 (car res)) (cdr res))
                         (if (= absedel strbezdiag) (cons (+ 1 (car res)) (+ 1 (cdr res))) res)))
                   (cons 0 0) m numerals))
  (and (= (car s) len)
      (< (cdr s) len)))    
 
;task5
(define (special n)
  (define laststr (append (cddr (build-list n values)) '(1)))
  (define (strmaker str)
           (map (λ x (if (= (car x) (- n 1)) 1 (+ (car x) 1))) str))
  (foldl (λ (x y) (if (= x (- n 1)) y (cons (strmaker (car y)) y))) (cons laststr '()) (cdr (build-list n values))))
 
  
;task6 
(define (zero-str? m)
  (foldl (λ (x y) (if (equal? #t (andmap zero? x)) (+ y 1) y)) 0 m))


 
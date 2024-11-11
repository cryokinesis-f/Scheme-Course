#lang scheme
;task1сортировка Хоара с одним проходом по списку для его деления
;(define (quick-sort lst)
;  (cond
;    ((empty? lst) empty)
;    (else (append (quick-sort (filter (λ (a) (< a (car lst))) lst))
;                  (filter (λ (a) (= a (car lst))) lst)
;                  (quick-sort (filter (λ (a) (> a (car lst))) lst ))))))
;выше как пример из лекции
(define (quicker-sort lst)
  (define (razbienie ostlst a lev chislo prav)
    (if (empty? ostlst) (list lev chislo prav)
        (cond ((< (car ostlst) a) (razbienie (cdr ostlst) a (cons (car ostlst) lev) chislo prav))
              ((= (car ostlst) a) (razbienie (cdr ostlst) a lev (cons (car ostlst) chislo) prav))
              ((> (car ostlst) a) (razbienie (cdr ostlst) a lev chislo (cons (car ostlst) prav))))))
  (if (empty? lst) empty
      (let ((peremennaya (razbienie lst (car lst) '() '() '())))
        (append (quicker-sort (car peremennaya)) (cadr peremennaya) (quicker-sort (caddr peremennaya))))))

;task2
;сортировка включением обычная
;(define (sort lst)
;  (cond ((empty? lst) empty)
;      ((cons? lst) (insert (car lst) (sort (cdr lst))))))
  
;(define (insert n lst)
; (cond ((empty? lst) (list n))
;       (else (cond ((<= n (car lst))(cons n lst))
;                     ((> n (car lst)) (cons (car lst) (insert n (cdr lst))))))))
;сортировка включением в порядке убывания суммы цифр
(define (insert-sort lst)
  (define (sum_of_digits number)
    (if (= number 0) 0
        (+ (abs (remainder number 10)) (sum_of_digits (quotient number 10)))))
  (define (insert n lst)
    (if (empty? lst) (list n)
        (let ((sn (sum_of_digits n)) (cars (sum_of_digits (car lst))))
          (cond ((> sn cars) (cons n lst))
                ((<= sn cars) (cons (car lst) (insert n (cdr lst))))))))
  (define (sort lst)
    (cond ((empty? lst) empty)
          ((cons? lst) (insert (car lst) (sort (cdr lst))))))
  (sort lst))

;task3 (integers "task3-in.txt" "task3-out.txt") 
(define (integers file-in file-out)
    
   
  (define in (open-input-file file-in))
  (define out (open-output-file file-out #:exists 'truncate))
  (define (iter sum kolvo tek state k)
    
    
    
    (define c (read-char in))
    (if (equal? c eof) (if (equal? tek "") (and (if (> k 0) (write-char #\newline out) 1) (display sum out)
                                                (write-char #\newline out) (display kolvo out) (close-output-port out))
                           (and (if (> k 0) (write-char #\newline out) 1) (display (string->number tek) out) (write-char #\newline out) (display (+ sum (string->number tek)) out)
                                                (write-char #\newline out) (display (+ kolvo 1) out) (close-output-port out)))
        (cond ((= 0 state) (cond ((<= 48 (char->integer c) 57) (iter sum kolvo (string-append tek (string c)) 1 k))
                                 ((equal? c #\-) (iter sum kolvo "" 2 k))
                                 (else (iter sum kolvo "" 0 k))))
              ((= 2 state) (cond ((<= 48 (char->integer c) 57) (iter sum kolvo (string-append "-" tek (string c)) 1 k))
                                 ((equal? c #\-) (iter sum kolvo "" 2 k))
                                  (else (iter sum kolvo "" 0 k))))
              ((= 1 state) (cond ((<= 48 (char->integer c) 57) (iter sum kolvo (string-append tek (string c)) 1 k))
                                 ((equal? #\- c) (and (if (> k 0) (write-char #\newline out) 1)
                                            (display (string->number tek) out) 
                                            (iter (+ sum (if (string->number tek) (string->number tek) 0)) (+ kolvo 1) "" 2 (+ k 1))))
                                 (else (and (if (> k 0) (write-char #\newline out) 1)
                                            (display (string->number tek) out) 
                                            (iter (+ sum (if (string->number tek) (string->number tek) 0)) (+ kolvo 1) "" 0 (+ k 1)))))))))
  (iter 0 0 "" 0 0))
                                                     
                           

;task4 (anti-perenos "task4-in.txt" "task4-out.txt")
;< - > - тире
;<-> - дефис
;<-newline> - знак переноса
(define (anti-perenos file-in file-out)
  (define in (open-input-file file-in))
  (define out (open-output-file file-out #:exists 'truncate))
  (define (func k state)
    (define c (read-char in))
    (if (equal? c eof) (close-output-port out)
        (cond ((= 0 state) (cond ((char-alphabetic? c) (and (display c out) (func k 0)))
                                 ((equal? c #\space) (and (display c out) (func k 1)))
                                 ((equal? c #\-) (func k 2))
                                 ((equal? c #\newline) (write-char #\newline out) (func k 0))
                                 (else (display c out) (func k 0))))
              ((= 1 state) (cond ((equal? c #\-) (and (display c out) (display #\space out) (read-char in) (func k 0)))
                                 (else (and (display c out) (func k 0)))))
              ((= 2 state) (cond ((<= 48 (char->integer c) 57) (and (display #\- out) (display c out) (func k 0)))
                                 ((equal? #\- c) (and (display c out) (func k 2)))
                                 ((equal? c #\newline) (func k 5))
                                 (else (and (display #\- out) (display c out) (func k 3)))))
              ((= 3 state) (cond ((char-alphabetic? c) (and (display c out) (func k 3)))
                                 ((equal? c #\-) (func k 4))
                                 (else (and  (display c out)) (func k 0))))
              ((= 4 state) (cond ((equal? c #\newline) (func k 3))
                                 (else (and (display #\- out) (display c out) (func k 3)))))
              ((= 5 state) (cond ((equal? c #\-) (func k 2))
                                 ((equal? c #\space) (and (write-char #\newline out) (func k 0)))
                                 (else (and (display c out) (func k 5))))))))
                               
  (func "" 0))
        



;task5 (translator "task5-in.txt" "task5-out.txt" "dictionary.txt")
(define (translator file-in file-out dictionary)
  (define in (open-input-file file-in))
  (define dictionaryy (open-input-file dictionary))
  (define out (open-output-file file-out #:exists 'truncate))
  
  (define (make-dict tek pair dic state)
    (define c (read-char dictionaryy))
    (if (equal? c eof) (dict-set dic pair tek)
        (cond ((= state 1) (cond ((char-alphabetic? c) (make-dict (string-append tek (string c)) pair dic 1))
                                 ((equal? c #\space) (make-dict "" tek dic 2))))
              ((= state 2) (cond ((char-alphabetic? c) (make-dict (string-append tek (string c)) pair dic 2))
                                 ((equal? c #\space) (make-dict (string-append tek "") pair dic 3))
                                 ((equal? c #\newline) (make-dict "" '() (dict-set dic pair tek) 1))))
              ((= state 3) (cond ((equal? c #\space) (make-dict tek pair (dict-set dic pair tek) 3))
                                 ((equal? c #\newline) (make-dict "" 0 (dict-set dic pair tek) 1))
                                 (else (make-dict (string-append tek " " (string c)) pair dic 2)))))))
  (define dic (make-dict "" 0 '() 1))
  (define (translate-text tek state)
    (define c (read-char in))
    (if (equal? c eof) (if (equal? tek "") (close-output-port out) (and (display (dict-ref dic tek) out) (close-output-port out)))
        (cond ((= state 0) (cond ((char-alphabetic? c) (translate-text (string-append tek (string c)) 1))
                                 (else (and (display c out) (translate-text "" 0)))))
              ((= state 1) (cond ((char-alphabetic? c)  (translate-text (string-append tek (string c)) 1))
                                 (else (and (display (dict-ref dic tek) out) (display c out) (translate-text "" 0))))))))
  (translate-text "" 0))
         
  
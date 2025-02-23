#lang scheme
(define (razd? x) (or (equal? x #\space) (equal? x #\tab)))
;task1
(define (z1 file-in)
  (with-output-to-file file-in #:exists 'append
    (lambda ()
      (write-char #\newline)))
  (define in (open-input-file file-in))
  (define (iter num-str num-stol state k)
    (define c (read-char in))
    (cond ((equal? c eof) k)
          ((= state 1) (cond ((equal? c #\space) (iter num-str (+ 1 num-stol) 1 k))
                              ((equal? c #\newline) (iter (+ 1 num-str) 0 3 (+ k 1)))
                              ((equal? c #\1) (if (= num-str num-stol) (iter num-str num-stol 1 k)
                                                  (iter num-str num-stol 2 k)))
                              (else (iter num-str num-stol 1 k))))
          ((= state 2) (cond ((equal? c #\newline) (iter (+ 1 num-str) 0 3 k))
                             (else (iter num-str 0 2 k))))
          ((= state 3) (cond ((or (equal? c #\newline ) (razd? c)) k)
                             ((equal? c #\1) (iter num-str num-stol (if (= num-str num-stol) 1 2) k))
                             (else (iter num-str num-stol 1 k)))))) (iter 0 0 1 0))

(define (z11 file-in)
  (define in (open-input-file file-in))
  (define (iter num-str num-stol state k)
    (define c (read-char in))
    (cond ((equal? c eof) k)
          ((= state 1) (cond ((equal? c #\space) (iter num-str (+ 1 num-stol) 1 k))
                              ((or (equal? c eof) (equal? c #\newline)) (iter (+ 1 num-str) 0 1 (+ k 1)))
                              ((equal? c #\1) (if (= num-str num-stol) (iter num-str num-stol 1 k)
                                                  (iter num-str num-stol 2 k)))
                              (else (iter num-str num-stol 1 k))))
          ((= state 2) (cond ((or (equal? c eof) (equal? c #\newline)) (iter (+ 1 num-str) 0 1 k))
                             (else (iter num-str 0 2 k)))))) (iter 0 0 1 0))
;task 2
(define (z2 file-in)
  (define in (open-input-file file-in))
  (define (f x)
    (define c (read-char in))
    (if (equal? c #\newline)
        x
        (f (+ (* x 10) (- (char->integer c) 48)))))
  (define num (- (f 0) 1))
  (display num) (display " ")
  (define (iter2 state y)
    (define c (read-char in))
    (if (equal? c eof) #t
        (cond ((= state 1) (if (equal? c #\space)
                               (if (= num y) (iter2 2 0) #f)
                               (iter2 state (+ (* y 10) ( - (char->integer c) 48)))))
              ((= state 2) (iter2 (if (equal? c #\newline) 1 2) 0)))))
    (iter2 1 0))



;task 4
(define (make table)
  (define (stroke num_of_str num_of_stolb lst)
    (if (empty? lst) '()
        (if (> num_of_str num_of_stolb) (stroke num_of_str (+ 1 num_of_stolb) (cdr lst))
            (if (= (car lst) 0) (stroke num_of_str (+ 1 num_of_stolb) (cdr lst))
                (append (make-list (car lst) (list num_of_str num_of_stolb)) (stroke num_of_str (+ 1 num_of_stolb) (cdr lst)))))))
  
  (define (iter n osttable)
    (if (empty? osttable) '()
        (append (stroke n 0 (car osttable)) (iter (+ 1 n) (cdr osttable)))))(iter 0 table))
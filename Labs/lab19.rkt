#lang scheme

;task1 (shablon "l19-task1-in.txt" 


(define (z1 input-file . p)
  (define (dictionary dic ostp k)
    (if (empty? ostp) dic
        (dictionary (dict-set dic k (car ostp)) (cdr ostp) (+ k 1))))
  (define dic (dictionary '() p 1))
  (display dic)
  (define in (open-input-file input-file))
  (define output-file "19-task1-out.txt")
  (define out (open-output-file output-file #:exists 'truncate))
  (define (iter dic state par)
    (define c (read-char in))
    (if (equal? c eof) (close-output-port out)
        (cond ((= state 0) (cond ((equal? c #\%) (iter dic 1 ""))
                                 (else (display c out) (iter dic 0 ""))))
              ((= state 1) (cond ((equal? c #\%) (display c out) (iter dic 0 ""))
                                 (else (iter dic 2 (string-append par (string c))))))
              ((= state 2) (cond ((equal? c #\%) (display (dict-ref dic (string->number par)) out) (iter dic 0 ""))
                                 (else (iter dic 2 (string-append par (string c)))))))))
  (iter dic 0 ""))

  

;task2 (summi "l19-task2-in.txt")
(define (summi input-file)
  (define in (open-input-file input-file))
  (define output-file "19-task2-out.txt")
  (define out (open-output-file output-file #:exists 'truncate))
  (define (chisla ch1 ch2)
    (+ (string->number ch1) (string->number ch2)))
  
  (define (iter s1 s2 spisok state)
    (define x (read-char in))
    (if (equal? x eof) (if (equal? s1 "") spisok (let ((s (chisla s1 s2))) (if (apply char<? (string->list (number->string s)))
                                        (cons s spisok) spisok)))
        (cond ((= state 0) (cond ((equal? x #\+) (iter s1 "" spisok 1))
                                 (else (iter (string-append s1 (string x)) "" spisok 0))))
              ((= state 1) (cond ((equal? x #\newline)
                                  (let ((s (chisla s1 s2)))
                                    (iter "" "" (if (apply char<? (string->list (number->string s)))
                                        (cons s spisok) spisok) 0)))
                                 (else (iter s1 (string-append s2 (string x)) spisok 1)))))))
  
  (define (quick-sort lst)
    (cond
      ((empty? lst) empty)
      (else (append (quick-sort (filter (λ (a) (> a (car lst))) lst))
                    (filter (λ (a) (= a (car lst))) lst)
                    (quick-sort (filter (λ (a) (< a (car lst))) lst ))))))
  (define sorted (quick-sort (iter "" "" '() 0)))

  (define (iteration ostlst)
    (if (empty? ostlst) (close-output-port out)
        (and (display (car ostlst) out) (display #\newline out) (iteration (cdr ostlst)))))
  (iteration sorted))
  
        
  


;task3 (z3 "l19-task3-in.txt")
(define (z3. file-in)
  (define in (open-input-file file-in))
  (define (iter str sum state k)
    (define x (read-char in))
    (if (equal? x eof) sum
        (cond ((= state 0) (cond ((equal? x #\() (iter "" sum 1 0))
                                 ((equal? x #\") (iter "" sum 2 0))
                                 ((equal? x #\') (iter "" sum 3 0))
                                 ((equal? x #\;) (iter "" sum 4 0))
                                 (else (iter "" sum 0 0))))
              ((= state 1 ) (cond ((or (equal? x #\space) (equal? x #\tab)
                     (equal? x #\newline)) (iter "" sum (if (equal? "define" str) 5 1) 0))
                                  (else (iter (string-append str (string x)) sum 1 0))))
              ((= state 2) (cond ((equal? x #\") (iter "" sum 0 0))
                                 (else (iter "" sum 2 0))))
              ((= state 3) (cond ((equal? x #\r) (iter "" sum 0 0))
                                 ((equal? x #\() (iter "" sum 3 (+ k 1)))
                                 ((equal? x #\)) (iter "" sum 3 (- k 1)))
                                 ((and (or (equal? x #\space) (equal? x #\tab)
                     (equal? x #\newline)) (= k -1)) (iter "" sum 0 0))
                                 (else (iter "" sum 3 k))))
              ((= state 4) (cond ((equal? x #\newline) (iter "" sum 0 0))
                                 (else (iter "" sum 4 0))))
              ((= state 5) (cond ((equal? x #\() (iter "" (+ sum 1) 0 0))
                                 ((or (equal? x #\space) (equal? x #\tab)
                     (equal? x #\newline)) (iter "" sum 5 0))
                                 (else (iter "" sum 0 0)))))))
  (iter "" 0 0 0))

(define (z3 input-file)
  (define in (open-input-file input-file))
  (define (razd? x)
    (or (equal? x #\space) (equal? x #\newline) (equal? x #\tab)))
  (define (iter str sum state k)
    (define c (read-char in))
    (if (equal? c eof) sum
        (cond ((= state 0) (cond ((equal? c #\() (iter "" sum 1 0))
                                 ((equal? c #\") (iter "" sum 2 0))
                                 ((equal? c #\')
                                  (let ((x (read-char in))) (if (equal? x #\() (iter "" sum 3 1)
                                                                (iter "" sum 0 0))))
                                 ((equal? c #\;) (iter "" sum 4 0))
                                 (else (iter "" sum 0 0))))
              ((= state 1) (cond ((and (equal? str "") (razd? c)) (iter str sum 1 0))
                                 ((char-alphabetic? c) (iter (string-append str (string c)) sum 1 0))
                                 ((equal? str "define") (iter "" sum 5 0))
                                 (else (iter "" sum 0 0))))
              ((= state 2) (cond ((equal? c #\") (iter "" sum 0 0))
                                 (else (iter "" sum 2 0))))
              ((= state 3) (cond ((= k 0) (iter "" sum 0 0))
                                 ((equal? c #\() (iter "" sum 3 (+ k 1)))
                                 ((equal? c #\)) (iter "" sum 3 (- k 1)))
                                 (else (iter "" sum 3 k))))
              ((= state 4) (cond ((equal? c #\newline) (iter "" sum 0 0))
                                 (else (iter "" sum 4 0))))
              ((= state 5) (cond ((equal? c #\() (iter "" (+ sum 1) 0 0))
                                 ((razd? c) (iter str sum 5 0))
                                 (else (iter "" sum 0 0 )))))))
  (iter "" 0 0 0))
              
                                 
                           
                                   
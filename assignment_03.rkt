#lang racket

(define (fst xs) (car xs))
(define (snd xs) (car (cdr xs)))
(define (thd xs) (car (cdr (cdr xs))))

(define (t1 n) (cons n (cons (* 2 n) (cons (* 3 n) '()))))
(define (t2 n) (list 1 n (* n n)))
(define (t3 n) (list (- n 1) n (+ n 1)))

(define (len xs) (cond ((null? xs) 0)
                       (else (+ 1 (len (cdr xs))))))

(define (dbls xs) (if (null? xs)
                      '()
                      (cons(* 2 (car xs)) (dbls (cdr xs)))))

(define (pos xs) (cond ((null? xs)
                        '())
                       ((>= (car xs) 0)
                        (cons (car xs) (pos (cdr xs))))
                       (else
                        (pos (cdr xs)))))

(define (poly x xs) (cond ((null? xs)
                           0)
                          (else (+ (car xs)
                                   (* x (poly x (cdr xs)))))))

(define (downto x n) (if (= n 0)
                         (cons x '())
                         (cons (+ x n) (downto x (- n 1)))))

(define (upto x n) (if (= n 0)
                       (cons (+ x n) '())
                       (cons x (upto (+ x 1) (- n 1)))))

(define (apnd xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (apnd (cdr xs) ys))))

(define (rev xs) (if (null? xs)
                     '()
                     (apnd (rev (cdr xs))
                           (cons (car xs) '()))))

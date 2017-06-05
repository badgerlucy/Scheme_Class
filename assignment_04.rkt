#lang racket

(define t1 (list '(0) '(1 2) '()))

(define t2 (cons '(0) (cons '(1 2) (cons '() '()))))

(define t3 (append '(0) (cons '(1 2) (cons '() '()))))

(define (conc xs) (if (null? xs)
                      '()
                      (append (car xs) (conc (cdr xs)))))

(define (tails xs) (if (null? xs)
                       '(())
                       (cons xs (tails (cdr xs)))))





(define (combi1 x xs) (if (null? xs)
                        '()
                        (cons (cons x (car xs))
                              (combi1 x (cdr xs)))                              
 ))


(define (combi xs) (if (null? xs)
                       '(())
                       (append (combi (cdr xs))
                               (combi1 (car xs)
                                       (combi (cdr xs))))))

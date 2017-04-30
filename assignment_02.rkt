#lang racket

(define (fact n) (if (= n 0)
                     1
                     (* n (fact (- n 1)))))

(define (fib n) (cond ((= n 0) 0)
                      ((= n 1) 1)
                      (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (pow n m) (cond ((= m 0) 1)
                        ((= n 0) 1)
                        (else (* n (pow n (- m 1))))))

(define (fpow n k) (cond ((= k 0) 1)
                         ((= n 0) 1)
                         ((= (modulo k 2) 1)
                          (* n
                             (pow n (quotient k 2))
                             (pow n (quotient k 2))))
                         (else (* (pow n (quotient k 2))
                                  (pow n (quotient k 2))))))

(define (log2 n) (cond ((= n 1) 0)
                       (else (+ (log2 (quotient n 2)) 1))))

(define (binom n k) (cond ((= k 1) n)
                          ((= n k) 1)
                          (else (+ (binom (- n 1) k)
                                   (binom (- n 1) (- k 1))))))

(define (quad x) (cond ((= x 1) 1)
                       ((= x 0) 0)
                       ((> x 0) (+ (quad (- x 1))
                                   x
                                   x
                                   (- 1)))
                       (else (+ (quad (+ x 1))
                                (- x)
                                (- x)
                                (- 1)))))
                                      

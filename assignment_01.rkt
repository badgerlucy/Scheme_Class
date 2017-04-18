#lang racket

(define (pow2 x) (* x x))

(define (maximum x y) (if (<= x y) y x))

(define (minimum x y) (if (<= x y) x y))

(define (cmp2 x y) (cond ((< x y) 1)
                         ((> x y) -1)
                         ((= x y) 0)))

(define (iseven x) (if (= (modulo x 2) 0) #t #f))

(define (bar y)(if (= (modulo y 2) 0) 1 -1))

(define (pow4 x) (pow2 (pow2 x)))

(define (pow6 x) (* (pow4 x)(pow2 x)))

(define (pow10 x) (* (pow6 x)(pow4 x)))

(define (ps x)(+ 1 x (pow2 x) (* x (pow2 x)) (pow4 x) (* x (pow4 x)) (pow6 x)
                 (* x (pow6 x)) (* (pow2 x) (pow6 x)) (* x (pow2 x) (pow6 x))))

(define (dom x y z) (if (>= (+ (if (< x 0) 1 0) (if (< y 0) 1 0)
          (if (< z 0) 1 0)) (+ (if (>= x 0) 1 0) (if (>= y 0) 1 0)
          (if (>= z 0) 1 0))) (+ (if (< x 0) 1 0) (if (< y 0) 1 0)
          (if (< z 0) 1 0)) (+ (if (>= x 0)1 0) (if (>= y 0) 1 0)
          (if (>= z 0) 1 0))))

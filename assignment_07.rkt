#lang racket

(define (forall p xs)
  (if (null? xs)
      `()
      (if (p (car xs))
          (if (forall p (cdr xs))
               #t
               #f)
          #f)))

(define (filt p xs)
  (if (null? xs)
      `()
      (if (p (car xs))
          (cons (car xs) (filt p (cdr xs)))
          (filt p (cdr xs)))))

(define (pos xs)
  (if (null? xs)
      `()
      (filt (lambda (x) (>= x 0)) xs)))



(define (nonulls xss)
  (if (null? xss)
      '()
      (filt (lambda (xs) (not (null? xs))) xss)))



(define (gmap p xs)
  (lambda (p xs)
    (foldr (lambda (a b) (cons (p a) b)) '() xs)))


(define (gforall p xs)
  (if (null? xs)
      '()
      (foldr (lambda (a b) (and (p a) b) #t xs))))
             

(define (gfilter p xs)
  (if (null? xs)
      `()
      (if (p (car xs))
          (foldr + 0 (car xs))
          (gfilter p (cdr xs)))))

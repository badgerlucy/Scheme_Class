#lang racket

(define EST -4)
(define CEST 2)
(define JST 9)
(define (CEST->JST a) (let ((b (+ a (- JST CEST))))
                        (if (> b 24)
                            (- b 24)
                            b)))

(define (JST->EST a) (let ((b (- a (- JST EST)))) ;괄호한번더 써야함 
                       (if (< b 0)
                           (+ 24 b)                          
                           b)))

(define (EST->CEST a) (let ((b (+ a (- CEST EST))))
                        (if (> b 24)
                            (- b 24)
                            b)))

(define (EST->JST a) (let ((b (+ a (- JST EST))))
                       (if (> b 24)
                           (- b 24)
                           b)))
                       
(define (CEST->EST a) (let ((b (- a (- CEST EST))))
                        (if (< b 0)
                            (+ 24 b)
                            b)))
                        
(define (JST->CEST a) (let ((b (- a (- JST CEST))))
                        (if (< b 0)
                            (+ 24 b)
                            b)))


                        
(define (qsolve a b c)
  (let* ((bb (* b b))
         (4ac (- 4 a c))
         (e (- bb 4ac))
         (2a (* 2 a)))
    (cond ((< e 0)
           '())
          ((= e 0)
           (/ (+ (- b) (sqrt e)) 2a))
          (else (cons (/ (+ (- b) (sqrt e)) 2a)
                      (/ (- (- b) (sqrt e)) 2a))))))  ;소수고치기




(define (invm a b c d)
  (let* ((ad (* a d))
         (bc (* b c))
         (e (- ad bc)))
    (cond ((< (abs e) 1.0e-15)             ;유동소수점이니까 0이 안될 수도 있음. 절댓값이 0에 가까워진다고 봐야함
           #f)
          (else (let ((a_ (/ d e))
                      (b_ (/ (- b) e))
                      (c_ (/ (- c) e))
                      (d_ (/ a e)))
                  (list a_ b_ c_ d_))))))


(define (pow n m)
  (let* ((k (quotient m 2))
         (nk2 (expt (expt n k) 2)))
    (cond ((= (modulo m 2) 0)
           nk2)
          (else (* n nk2)))))

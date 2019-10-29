#lang scheme
(define (abs x)
  (if (< x 0)
      (- x)
      x))
(define (average a b)
  (/ (+ a b) 2))
(define (guess function try precision)
  (if
   (< (abs (- (function try) try)) precision)
   try
   (let
       (
        (next (average (function try) try)))
     (newline)
     (display next)
     (guess function next precision))))

(define (sq a)
  (guess (lambda (x) (/ a x)) 2 0.1))

(define (cont-frac n d k)
  (define (iter n d k i)
    (if (= i k)
        (/ (n k) (d k))
        (/ (n i) (+ (d i) (iter n d k (+ 1 i))))))
  (iter n d k 1))


(define (square x)
  (* x x))

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (- 0 (* x x))))
             (lambda (i) (- (* 2 i) 1))
             k))

      


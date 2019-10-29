#lang scheme
(define (make-rat x y)
  (let
      ((g (gcd x y)))
    (if (< y 0) (cons (/ (- x) g) (/ (- y) g)) (cons (/ x g) (/ y g)))))

(define numer car)
(define denom cdr)

(define (add x y)
  (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))
(define (sub x y)
  (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))
(define (mul x y)
  (make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))
(define (divide x y)
  (make-rat (* (numer x) (denom y)) (* (denom x) (numer y))))
(define (equal-rat? x y)
  (and (= (numer x) (numer y)) (= (denom x) (denom y))))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))




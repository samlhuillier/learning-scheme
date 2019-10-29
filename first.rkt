#lang scheme


        
(define (smallest-divisor n)
  (define (iter count)
    (if (= (remainder n count) 0) count (iter (+ 1 count))))
  (iter 2))

(define (next a)
  (if (= 2 a) (+ 1 a) (+ 2 a)))
(define (faster-smallest n)
  (define (iter to-check)
    (if (= (remainder n to-check) 0) to-check (iter (next to-check))))
  (iter 2))

(define (timeittakes funcname testnum start-time)
  (if (funcname testnum)
      (- (current-milliseconds) start-time)
      ("wrong")))

  
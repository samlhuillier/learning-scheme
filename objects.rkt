#lang sicp

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount)) balance)
      "insuficient funds"))


(define new-withdraw
  (let
      ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount)) balance)
          "insuficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "insufficient funds")))
(define (call-the-cops) (display "cops"))
(define (make-acount balance password no-of-attempts)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "insufficient funds"))
  (define (deposit amount)
    (begin (set! balance (+ balance amount)) balance))
  (define (dispatch password-attempt m)
    (if (<= no-of-attempts 7)
        (if (eq? password-attempt password)
            (cond
              ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "what")))
            (set! no-of-attempts (+ no-of-attempts 1)))
        call-the-cops))
        
    
  dispatch)

(define rand
  (let
      ((x random-init))
    (lambda () (set! x (rand-update x)) x)))

    

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))
    


(define (monte-carlo trials experiment)
  (define (iter trials-left passes)
    (if (= trials-left 0)
        (/ passes trials)
    (if (experiment)
        (iter (- trials-left 1) (+ passes 1))
        (iter (- trials-left 1) passes))))
  (iter trials 0))

(define (random-in-range low high)
  (let
      ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (monte-carlo trials (is-in-shape? P x1 x2 y1 y2)))


(define (is-in-shape? P x1 x2 y1 y2)
  (lambda () (P (random-in-range x1 x2) (random-in-range y1 y2))))

(define (make-acumulator value)
  (lambda (amount) (begin (set! value (+ value amount)) value)))

(define (make-monitored f)
  (let
      ((count 0))
    (lambda (op)
      (if (eq? op 'how-many-calls?)
          count
          (begin (set! count (+ 1 count)) (f op))))))
(define (rand wannado)
  (cond
    ((eq? wannado 'generate)
      (let
          ((x random-init))
        (lambda () (set! x (rand-update x)) x)))
    ((eq? wannado 'reset) (lambda (new-value) (set! x (rand-update x)) x))))
      
  

#lang sicp
(define (attach-tag type contents)
  (if (number? contents)
      contents
  (cons type contents)))
(define (type-tag datum)
  (if (pair? datum)
  (car datum)
  (if (number? datum)
      'scheme-number
      (error "cunt"))))
(define (contents datum)
  (if (pair? datum)
  (cdr datum)
  datum))

(define (apply-generic op . args)
  (let
      ((types (map type-tag args)))
    (let
        ((proc (get op types)))
      (if proc
          (apply proc (map contents args))
        (let
            ((a (give-a-val (cdr args)))
             (b (give-a-val (cadr args))))
          (cond
            ((> a b) 
(define (give-a-val object)
  (let
      ((type (type-tag object)))
    (cond
      ((= type 'integer) 0)
      ((= type 'rational) 1)
      ((= type 'real) 2)
      ((= type 'complex 3)))))
(define (coerce-to type args)
  (map (lambda (datum) ((get-coercion (type-tag datum) type) datum))))
(define (is-any-empty? elements)
  (cond
    ((null? elements) #f)
    ((not (car elements)) #t)
    (else (is-any-empty? (cdr elements)))))
(define (find-right-type args changer)
  (if (null? changer)
      #f
  (let
      ((coerced (coerce-to (type-tag (car changer)) args)))
    (if (not (is-any-empty? coerced))
        coerced
        (find-right-type args (cdr changer))))))

                
                
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? a) (apply-generic '=zero? a))
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number) (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number) (lambda (x) (= 0 x)))
  'done)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  (define (number x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let
        ((gc (gcd n d)))
      (cons (/ n gc) (/ d gc))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y)) (* (denom x) (numer y))))
  (define (tag x)
    (attach-tag 'rational x))
  (define (project rational) (/ (numer rational) (denom rational)))
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational (lambda n d) (tag (make-rat n d)))
  (put 'equ? '(rational rational) (lambda (x y) (and (= (numer x) (numer y)) (= (denom x) (denom y)))))
  (put '=zero? 'rational (lambda (x) (and (= (numer x) 0) (= (denom x) 0))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;;we first install the selectors that identify whether they are rectangular or polar
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;;interface to rest of the system through the database
  (define (tag x) (attach-tag 'complex x))
  (define (project complex) (make-rat
  (put 'add '(complex complex) (lambda (x y) (tag (make-from-real-imag (+ (real-part x) (real-part y)) (+ (imag-part x) (imag-part y))))))
  (put 'sub '(complex complex) (lambda (x y) (tag (make-from-real-imag (- (real-part x) (real-part y)) (- (imag-part x) (imag-part y))))))
  (put 'mul '(complex complex) (lambda (x y) (tag (make-from-mag-ang (* (magnitude x) (magnitude y)) (+ (angle x) (angle y))))))
  (put 'div '(complex complex) (lambda (x y) (tag (make-from-mag-ang (/ (magnitude x) (magnitude y)) (- (angle x) (angle y))))))
  (put 'make-from-real-imag '(complex complex) (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang '(complex complex) (lambda (x y) (tag (make-from-mag-ang x y))))
  (put 'equ? '(complex complex) (lambda (x y) (and (= (real x) (real y)) (= (imag x) (imag y)))))
  (put '=zero? 'complex (lambda (x) (and (= (real x) 0) (= (imag x) 0))))
  'done)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))


(define (scheme-number->complex n)
  (apply-generic 'make-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)
(define (make-complex real-part imag-part)
  (cons real-part imag-part))
(define (real-part comp)
  (car comp))
(define (imag-part comp)
  (cdr comp))
(define (int-to-rat integer)
  (make-rational integer 1))
(define (rat-to-real rat)
  (/ (numer rat) (denom rat)))
(define (real-to-imag real)
  (make-complex real 0))

(define (raise object)
  (cond
    ((integer? object) (int-to-rat object))
    ((rational? object) (rat-to-real object))
    ((real? object) (real-to-imag object))
    (else object)))
  
(define (add-terms L1 L2)
  (cond
    ((empty-termlist? L1) L2)
    ((empty-termlist? L2) L1)
    (else
     (let
         ((t1 (first-term L1)) (t2 (first-term L2)))
       (cond
         ((

(define (install-polynomial-packange)
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable polynomial) (car polynomial))
  (define (term-list polynomial) (cdr polynomial))
  (define (same-variable? a b) (eq? a b))
  (define (add-poly poly1 poly2)
    (if (same-variable? (variable poly1) (variable poly2))
        (make-poly (variable poly1) (add-terms (term-list poly1) (term-list poly2)))
        (error "not same variable")))
  (define (mul-poly poly1 poly2)
    (if (same-variable? (variable poly1) (variable poly2))
        (make-poly (variable poly1) (mul-terms (term-list poly1) (term-list poly2)))
        (error "not the same variable")))
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial (lambda (var terms) (make-poly var terms)))
  'done)
    

  
      
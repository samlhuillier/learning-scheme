#lang scheme
(define x 5)

(define (memq symbol list)
  (cond
    ((null? list) #f)
    ((eq? (car list) symbol) #t)
    (else (memq symbol (cdr list)))))

(define (equal? string1 string2)
  (cond
    ((and (null? string1) (null? string2)) #t)
    ((or (null? string1) (null? string2)) #f)
    ((and (pair? string1) (pair? string2)) (if (not (eq? (car string1) (car string2))) #f (equal? (cdr string1) (cdr string2))))
    ((and (not (pair? string1)) (not (pair? string2))) (if (eq? string1 string2) #t #f))
    ((or (pair? string1) (pair? string2)) #f)
    (else (equal? (cdr string1) (cdr string2)))))

(define (deriv exp var)
  (cond
    ((or (number? exp)) 0)
    ((variable? exp) (if (same-variable? exp var) 1 0))
    ((exponentiation? exp) (make-product (exponent exp) (make-product (deriv (base exp) var) (make-exponentiation (base exp) (- (exponent exp) 1)))))
    ((sum? exp) (make-sum (deriv (addend exp) var) (deriv (augend exp) var)))
    ((product? exp) (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) var)) (make-product (multiplicand exp) (deriv (multiplier exp) var))))
    (else (error "not valid"))))

(define (variable? x) (symbol? x))
(define (same-variable? a b)
  (and (variable? a) (variable? b) (eq? a b)))
(define (make-sum a b)
  (cond
    ((=number? a 0) b)
    ((=number? b 0) a)
    ((and (number? a) (number? b)) (+ a b))
    ((and (pair? (car a)) (pair? (car b))) (list '+ (car a) (car b)))
 
    (else (list '+ a b))))
             
  
(define (make-product a b)
  (cond
    ((or (=number? a 0) (=number? b 0)) 0)
    ((=number? a 1) b)
    ((=number? b 1) a)
    ((and (number? a) (number? b)) (* a b))
    (else
  (list '* a b))))
(define (sum? exp)
  (and (pair? exp) (eq? (car exp) '+)))
(define (product? exp)
  (and (pair? exp) (eq? (car exp) '*)))
(define (addend exp) (cadr exp))
(define (augend exp)
  (if (pair? (cdr (cdr exp)))
      (cons (car exp) (cdr (cdr exp)))
      (cadr (cdr exp))))
(define (multiplier exp) (cadr exp))

(define (multiplicand exp)
  (if (pair? (cdr (cdr exp)))
      (cons (car exp) (cdr (cdr exp)))
      (cadr (cdr exp))))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))
(define (base exp)
  (cadr exp))
(define (exponent exp)
  (cadr (cdr exp)))
(define (make-exponentiation base power)
  (cond
    ( (=number? power 1) base)
    ((=number? power 0) 1)
   (else
  (list '** base power))))
     



(define (element-of-set? x set)
  (cond
    ((null? set) #f)
    ((equal? (car set) x) #t)
    ((< x (car set)) #f)
    (else (element-of-set? x (cdr set)))))
(define (remove-element x set)
  (cond
    ((null? set) #f)
    ((equal? x (car set)) (cdr set))
    (else (cons (car set) (remove-element x (cdr set))))))
      
(define (adjoin-set set object)
  
      (cons x set))

(define (intersection-set a b)
  (cond
    ((or (null? a) (null? b)) '())
    ((element-of-set? (car a) b) (cons (car a) (intersection-set (cdr a) (remove-element (car a) b))))
    (else (intersection-set (cdr a) b))))


(define (union-set a b)
  (cond
    ((null? a) b)
    ((not (element-of-set? (car a) b)) (cons (car a) (union-set (cdr a) b)))
    (else (union-set (cdr a) b))))


(define (union-set-overlaps a b)
  (if (null? a)
      b
      (cons (car a) (union-set-overlaps (cdr a) b))))
(define (ordered-intersection set1 set2)
  (cond
    ((or (null? set1) (null? set2)) '())
    ((= (car set1) (car set2)) (cons (car set1) (ordered-intersection (cdr set1) (cdr set2))))
    ((> (car set1) (car set2)) (ordered-intersection set1 (cdr set2)))
    (else (ordered-intersection (cdr set1) set2))))
(define (adjoin-set-ordered x set)
  (cond
    ((null? set) '())
    ((= x (car set)) set)
    ((< x (car set)) (cons x set))
    (else (cons (car set) (adjoin-set-ordered x (cdr set))))))

(define (union-set-ordered set1 set2)
  (cond
    ((null? set1) set2)
    ((null? set2) set1)
    ((= (car set1) (car set2)) (cons (car set1) (union-set-ordered (cdr set1) (cdr set2))))
    ((> (car set1) (car set2)) (cons (car set2) (union-set-ordered set1 (cdr set2))))
    (else (cons (car set1) (union-set-ordered (cdr set1) set2)))))

(define (entry tree) (car tree))
(define (left-node tree) (car (cdr tree)))
(define (right-node tree) (caddr tree))
(define (make-tree-node entry left-node right-node)
  (list entry left-node right-node))
(define (element-of-tree? x tree)
  (cond
    ((null? tree) #f)
    ((= x (entry tree)) #t)
    ((< x (entry tree)) (element-of-tree? x (left-node tree)))
    (else (element-of-tree? x (right-node tree)))))

(define (adjoin-to-tree x tree)
  (cond
    ((null? tree) (make-tree-node x null null))
    ((= x (entry tree)) tree)
    ((< x (entry tree)) (make-tree-node (entry tree) (adjoin-to-tree x (left-node tree)) (right-node tree)))
    (else (make-tree-node (entry tree) (left-node tree) (adjoin-to-tree x (right-node tree))))))
(define (append list1 list2)
  (cond
    ((null? list1) (if (pair? list2) list2 (list list2)))
    ((not (pair? list1)) (if (pair? list2) (cons list1 list2) (cons list1 (list list2))))
      (else (cons (car list1) (append (cdr list1) list2)))))



     
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (equal? (car object) 'leaf))
(define (symbol-leaf object)
  (cadr object))
(define (weight-leaf object)
  (caddr object))

(define (make-leaf-set pairs)
  (cond
    ((null? pairs) null)
    ((and (pair? pairs) (pair? (car pairs))) (adjoin-set-huff (make-leaf (car (car pairs)) (cdr (car pairs))) (make-leaf-set (cdr pairs))))
    (else (error "cunt"))))



(define (weight tree-node)
  (if (leaf? tree-node)
      (weight-leaf tree-node)
      (car (cdr (cdr (cdr tree-node))))))
(define (adjoin-set-huff x set)
  (cond
    ((null? set) (list x))
    ((< (weight x) (weight (car set))) (cons x set))
    (else (cons (car set) (adjoin-set-huff x (cdr set))))))

(define (symbols tree-node)
  (if (leaf? tree-node)
  (symbol-leaf tree-node)
  (car (cdr (cdr tree-node)))))

(define (left-branch tree-node) (car tree-node))
(define (right-branch tree-node) (car (cdr tree-node)))




(define (decode bits tree)
  (define (iter new-bits current-branch)
  (cond
    ((leaf? current-branch) (cons (symbol-leaf current-branch) (iter new-bits tree)))
    ((or (null? current-branch) (null? new-bits)) null)
    
    ((= 0 (car new-bits)) (iter (cdr new-bits) (left-branch current-branch)))
    ((= 1 (car new-bits)) (iter (cdr new-bits) (right-branch current-branch)))))
  (iter bits tree))

(define (make-code-tree left right)
  (list left right (append (symbols left) (symbols right)) (+ (weight left) (weight right))))
(define ll (make-leaf 'a 5))
(define rr (make-leaf 'b 6))
(define root (make-code-tree ll rr))

(define sample-tree (make-code-tree (make-leaf 'A 4) (make-code-tree (make-leaf 'B 2) (make-code-tree (make-leaf 'D 1) (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (encode message huff-tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) huff-tree) (encode (cdr message) huff-tree))))
(define (contains? items val)
  (cond
    ((null? items) #f)
    ((not (pair? items)) (equal? val items))
    ((equal? val (car items)) #t)
      (else (contains? (cdr items) val))))
(define (encode-symbol symbol tree)
  (cond
    ((equal? (symbols tree) symbol) null)
    ((contains? (symbols (left-branch tree)) symbol) (cons '0 (encode-symbol symbol (left-branch tree))))
    ((contains? (symbols (right-branch tree)) symbol) (cons '1 (encode-symbol symbol (right-branch tree))))
    (else (error "cunt"))))

(define (generate-huffman-tree pairs)
 (successive-merge (make-leaf-set pairs)))
(define (successive-merge ordered-list)
  (if (null? (cdr ordered-list))
      (car ordered-list)
      (successive-merge (adjoin-set-huff (make-code-tree (car ordered-list) (cadr ordered-list)) (cdr (cdr ordered-list))))))
    
(define (generate-huffman-tree-me pairs)
 (make-leaf-set pairs))

(define rock-pairs (list (cons 'a 2) (cons 'boom 1) (cons 'Get 2) (cons 'job 2) (cons 'na 16) (cons 'Sha 3) (cons 'yip 9) (cons 'Wah 1)))
(define rock-huff-tree (generate-huffman-tree rock-pairs))

(define lyrics '(Get a job
Sha na na na na na na na na
Get a job
Sha na na na na na na na na
Wah yip yip yip yip yip yip yip yip yip
Sha boom))



(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2)) (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2)) (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2)) (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2)) (- (angle z1) (angle z2))))





(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (square x) (* x x))
(define (type-tag datum)
  (if (pair? datum)
  (car datum)
  (error "wrong data")))
(define (contents datum)
  (if (pair? datum)
  (cdr datum)
  (error "wrong data")))

(define (rectangular? z)
  (equal? (type-tag z) 'rectangular))
(define (polar? z)
  (equal? (type-tag z) 'polar))

(define (real-part-rectangular z)
  (car z))
(define (imag-part-rectangular z)
  (cdr z))
(define (mag-from-rectangular z)
  (sqrt (+ (square (real-part-rectangular z)) (square (imag-part-rectangular z)))))
(define (ang-from-rectangular z)
  (atan (imag-part-rectangular z) (real-part-rectangular z)))
(define (make-from-real-im-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-angle-rectangular mag angle)
  (attach-tag 'rectangular (cons (* mag (cos angle)) (* mag (sin angle)))))

(define (mag-part-polar z) (car z))
(define (angle-part-polar z) (cdr z))
(define (real-part-polar z)
  (* (mag-part-polar z) (cos (angle-part-polar z))))
(define (imag-part-polar z)
  (* (mag-part-polar z) (sin (angle-part-polar z))))
(define (make-from-mag-angle-polar mag angle)
  (attach-tag 'polar (cons mag angle)))
(define (make-from-im-real-polar x y)
  (attach-tag 'polar (cons (sqrt (+ (square x) (square y))) (atan x y))))

(define (real-part z)
  (cond
    ((rectangular? z) (real-part-rectangular z))
    ((polar? z) (real-part-polar z))
    (else (error "neither"))))
(define (imag-part z)
  (cond
    ((rectangular? z) (imag-part-rectangular z))
    ((polar? z) (imag-part-polar z))
    (else (error "cunt"))))
(define (magnitude z)
  (cond
    ((rectangular? z) (mag-from-rectangular z))
    ((polar? z) (mag-part-polar z))
    (else (error "cunttt"))))
(define (angle z)
  (cond
   ((rectangular? z) (ang-from-rectangular z))
   ((polar? z) (angle-part-polar z))
   (else (error "wrong"))))
(define (make-from-real-imag x y)
  (make-from-real-im-rectangular x y))
(define (make-from-mag-ang r a)
  (make-from-mag-angle-polar r a))


  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var) (deriv (augend exp) var)))
  (define (deriv-product exp var)
    (make-sum (make-product (deriv (multiplicand exp) var)) (make-product (deriv (multiplier exp) var) (multiplicand exp))))
(define (deriv-exp exp var)
  (make-product (augend exp) (power (addend exp) (- (augend exp) 1))))

(define (instal-deriv)
(put 'deriv '+ deriv-sum)
(put 'deriv '* deriv-product)
  'done)

(define (make-hq-file division-name file)
  (cons division-name file))
(define (get-division-name hq-file)
  (car hq-file))
(define (get-division-file hq-file)
  (cdr hq-file))



(define (make-from-real-imag-message x y)
  (define (dispatch op)
    (cond
      ((eq? op 'real-part) x)
      ((eq? op 'imag-part) y)
      ((eq? op 'magnitude) (sqrt (square x) (square y)))
      ((eq? op 'angle) (atan y x))
      (else (error "error"))))
  dispatch)

(define (make-from-mag-ang-message mag ang)
  (define (dispatch op)
    (cond
      ((eq? op 'magnitude) mag)
      ((eq? op 'angle) ang)
      ((eq? op 'real-part) (* mag (cos ang)))
      ((eq? op 'imag-part) (* mag (sin ang)))
      (else (error "wrong"))))
  dispatch)




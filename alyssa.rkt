#lang scheme
(define (mycons a b)
  (lambda (m) (m a b)))
(define (mycar pair)
  (pair (lambda (a b) a)))
(define (mycdr pair)
  (pair (lambda (a b) b)))
(define (make-interval a b) (mycons a b))
(define (lower-bound interval) (min (mycar interval) (mycdr interval)))
(define (upper-bound interval) (max (mycar interval) (mycdr interval)))
(define (add-interval inter1 inter2)
  (make-interval (+ (upper-bound inter1) (upper-bound inter2)) (+ (lower-bound inter1) (lower-bound inter2))))
(define (subtract inte1 inte2)
  (make-interval (- (upper-bound inte1) (lower-bound inte2)) (- (upper-bound inte2) (lower-bound inte1))))
(define (mul-interval inte1 inte2)
  (let
      ((a (* (lower-bound inte1) (lower-bound inte2)))
       (b (* (lower-bound inte1) (upper-bound inte2)))
       (c (* (upper-bound inte1) (upper-bound inte2)))
       (d (* (upper-bound inte1) (lower-bound inte2))))
    (make-interval (max a b c d) (min a b c d))))

(define (div-interval inte1 inte2)
  (if (<= (* (lower-bound inte1) (upper-bound inte2)) 0)
           (display "cant")
  (mul-interval inte2 (make-interval (/ 1.0 (lower-bound inte1)) (/ 1.0 (upper-bound inte1))))))


(define (make-center-percent center percent)
  (let
      ((width (* center (/ percent 100))))
    (make-interval (+ center width) (- center width))))
(define (average x y)
  (/ (+ x y) 2))
(define (select-center interval)
  (average (lower-bound interval) (upper-bound interval)))
(define (select-center-percent interval)
  (* 100 (/ (/ (- (upper-bound interval) (lower-bound interval)) 2) (select-center interval))))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2) (add-interval r1 r2)))
(define (par2 r1 r2)
  (let
      ((one (make-interval 1 1)))
    (div-interval one (add-interval (div-interval one r1) (div-interval one r2)))))

(define (list-ref list n)
  (if (= 0 n)
      (car list)
      (list-ref (cdr list) (- n 1))))

(define (length list)
  (if (null? list)
      0
      (+ 1 (length (cdr list)))))
(define (length-iter list)
  (define (iter count list)
    (if (null? list)
        count
        (iter (+ 1 count) (cdr list))))
  (iter 0 list))


(define (last-pair list)
  (if (null? (cdr list))
      list
      (last-pair (cdr list))))
(define (reverse-order lirst)
  (define (get-to-last ll)
    (if (null? (cdr ll))
        (car ll)
        (get-to-last (cdr ll))))
  (get-to-last lirst))



(define (cc amount listofcoins)
  (cond
    ((= amount 0) 1)
    ((or (< amount 0) (no-more? listofcoins)) 0)
    (else (+ (cc (- amount (first-denomination listofcoins)) listofcoins) (cc amount (except-first-denomination listofcoins))))))

(define (first-denomination items)
  (car items))
(define (except-first-denomination items)
  (cdr items))
(define (no-more? items)
  (null? items))



               
(define (same-parity a . bb)
  (define (iter b)
  (let
      ((rem (remainder a 2)))
    (if (null? b) b
    (if (= rem (remainder (car b) 2))
      (cons (car b) (iter (cdr b)))
      (iter (cdr b))))))
  (iter bb))



(define (colonel a . b)
  (remainder (car (cdr (cdr b))) 2))

(define (scale items factor)
  (if (null? items)
  items
  (cons (* factor (car items)) (scale (cdr items) factor))))


(define (scaale factor items)
  (map (lambda (x) (* x factor)) items))

(define (square x) (* x x))


(define (square-myself list)
  (if (null? list)
      null
      (cons (square (car list)) (square-myself (cdr list)))))
(define (hesquares list)
  (map square list))


(define (for-each proc list) 
   (cond 
    ((null? list) #t) 
    (else (proc (car list)) 
          (for-each proc (cdr list)))))

(define (total-leaves node)
  (cond
    ((null? node) 0)
    ((not (pair? node)) 1)
      (+ (total-leaves (car node)) (total-leaves (cdr node)))))


(define (reverse items)
  (if (null? items)
      items
      (cons (reverse (cdr items)) (cons (car items) null))))
(define (deep-reverse items)
  (if (or (null? items) (not (pair? items)))
      items
      
      (append (deep-reverse (cdr items))  (cons (deep-reverse (car items)) null))))

(define (fringe items)
  (if (null? items)
      null
 (if (or (not (pair? items)) )
      (cons items null)
      (append (fringe (car items)) (fringe (cdr items))))))




(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))


(define (total-weight mobile)
  (let
      ((left (left-branch mobile))
       (right (right-branch mobile)))
    (cond
      ((and (not (pair? left)) (not (pair? right))) (+ (branch-structure left) (branch-structure right)))
      ((and (pair? left) (not (pair? right))) (+ (total-weight (branch-structure left)) (branch-structure right)))
      ((and (not (pair? left)) (pair? right)) (+ (branch-structure left) (total-weight (branch-structure right))))
      (else (+ (total-weight (branch-structure left)) (total-weight (branch-structure right)))))))

(define (addorcont branch-structure)
  (if (pair? branch-structure)
      (better-totweight branch-structure)
      branch-structure))
      
      
(define (better-totweight mobile)
  (let
      ((left (left-branch mobile))
       (right (right-branch mobile)))
    (+ (addorcont (branch-structure left)) (addorcont (branch-structure right)))))

(define (balanced? mobile)
  (let
      ((left (left-branch mobile))
       (right (right-branch mobile)))
     
     (= (* (branch-length left) (better-totweight (branch-structure left))) (* (branch-length right) (better-totweight (branch-structure right))))))

(define (scale-tree node factor)
  (cond
    ((null? node) null)
    ((not (pair? node)) (* factor node))
    (else (list (scale-tree (car node) factor) (scale-tree (cdr node) factor)))))



(define (square-map items)
  (map (lambda (in) (if (not (pair? in)) (square in) (square-map in))) items))

           
(define (scale-wid-map node factor)
  (map (lambda (in) (if (pair? in) (scale-wid-map in factor) (* in factor))) node))

(define (tree-map proc tree)
  (cond
    ((null? tree) null)
    ((not (pair? tree)) (proc tree))
    (else
     (cons (tree-map proc (car tree)) (tree-map proc (cdr tree))))))
(define (abst-square-tree tree) (tree-map square tree))

(define (subsets s)
  (if (null? s)
      (list null)
      (let
          ((rest (subsets (cdr s))))
        (append rest (map (subsets rest))))))





(define (enumerate-int-in-range low high)
  (if (> low high)
      null
      (cons low (enumerate-int-in-range (+ 1 low) high))))
(define (append list1 list2)
  (cond
    ((null? list1) list2)
    ((not (pair? list1)) (list list1 list2))
    (else
      
      (cons (car list1) (append (cdr list1) list2)))))

(define (enumerate-tree-leaves tree)
  (cond
    ((null? tree) null)
    ((not (pair? tree)) tree)
    (else
      (append (enumerate-tree-leaves (car tree)) (enumerate-tree-leaves (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulation + 0 (map square (filter odd? (enumerate-tree-leaves tree)))))

(define tester (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (fib n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    ((= n 2) 1)
    (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (even-fib n)
  (accumulation cons (list null) (filter even? (map fib (enumerate-int-in-range 0 n)))))

(define (list-fib-squares n)
  (accumulation cons null (map square (map fib (enumerate-int-in-range 0 (+ 1 n))))))


(define (appender seq1 seq2)
  (accumulation cons seq2 seq1))
(define (lengthh sequence)
  (accumulation (lambda (x y) (+ 1 y)) 0 sequence))



(define (horner-eval x coefficient-sequence)
  (accumulation (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms))) 0 coefficient-sequence))



(define (count-leaves-accu t)
  (accumulation + 0 (map (lambda (x) 1) t)))



      

(define ll (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

(define (get-first list)
  (accumulation (lambda (x y) (cons (car x) y)) null list))
      
(define (remove-first list)
  (accumulation (lambda (x y) (cons (cdr x) y)) null list))

(define (my-map proc items)
  (if (null? items)
      null
      (cons (proc (car items)) (map proc (cdr items)))))
(define (prime? n)
  (define (iter test)
    (cond
      ((= (remainder n test) 0) #f)
      ((> (square test) n) #t)
      (else (iter (+ 1 test)))))
  (iter 2))

(define (dot-product v w)
  (accumulation + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (m-row) (dot-product m-row v)) m))





(define (reverse-right sequence)
  (accumulation (lambda (x y) (append y x)) null sequence))


(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


(define (reverse-left sequence)
  (fold-left (lambda (x y) (cons y x)) null sequence))

     




(define a (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define b (list (list 4 6 2) (list 1 3 9) (list 7 8 5)))


(define (find-pairs n)
  (let
      ((i (enumerate-int-in-range 1 n)))
    (accumulation append null (map (lambda (x) (map (lambda (y) (list x y)) (enumerate-int-in-range 1 (- x 1)))) i))))




        
        
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-list-from-pair pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (filter predicate list)
  (cond
    ((null? list) null)
    ((predicate (car list)) (cons (car list) (filter predicate (cdr list))))
    (else
     (filter predicate (cdr list)))))
(define (prime-pairs n)
  (map make-list-from-pair (filter prime-sum? (find-pairs n))))

(define (flatmap proc seq)
  (accumulation append null (map proc seq)))
(define (permutations s)
  (if (null? s)
      (list null)
      (flatmap (lambda (x) (map (lambda (p) (cons x p)) (permutations (removee x s)))) s)))

(define (removee x s)
  (filter (lambda (y) (not (= x y))) s))
      

(define (all-triples n)
  (map
   (lambda (x)
     (map
      (lambda (y)
        (map
         (lambda (z) (list x y z)) (enumerate-int-in-range 1 (- y 1))))
      (enumerate-int-in-range 1 (- x 1))))
     (enumerate-int-in-range 1 n)))

(define (all-triples-flat n)
  (flatmap
   (lambda (x)
     (flatmap
      (lambda (y)
        (map
         (lambda (z) (list x y z)) (enumerate-int-in-range 1 (- y 1))))
      (enumerate-int-in-range 1 (- x 1))))
   (enumerate-int-in-range 1 n)))


(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (empty-board board-size)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-int-in-range 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))
(define (accumulation acproc initial list)
  (if (null? list)
      initial
      (acproc (car list) (accumulation acproc initial (cdr list)))))

(define (replace-by-index index val seq)
  (cond
    ((null? seq) null)

   ((= index 0) (cons val (replace-by-index (- index 1) val (cdr seq))))
  (else (cons (car seq) (replace-by-index (- index 1) val (cdr seq))))))

(define (obtain-by-index index seq)
  (if (= 0 index)
      (car seq)
      (obtain-by-index (- index 1) (cdr seq))))
(define (adjoin-position new-row k rest-of-queens)
  (replace-by-index
   new-row
   (replace-by-index k 1 (obtain-by-index new-row rest-of-queens))
   rest-of-queens))


(define (create-list size val)
  (if (= 0 size)
      null
      (cons val (create-list (- size 1) val))))



(define (empty-board board-size)
  (create-list board-size (create-list board-size 0)))

(define (safe? k positions)
  (

        
           

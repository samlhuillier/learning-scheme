#lang sicp
(#%require sicp-pict)



(define wave2 (beside mark-of-zorro (flip-vert mark-of-zorro)))
(define wave4 (below wave2 (flip-horiz wave2)))

(define (duplicate-flip image)
  (let
      ((step1 (beside image (flip-vert image))))
    (paint (below step1 (flip-horiz step1)))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let
          ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (top-split painter n)
  (if (= n 0)
      painter
      (let
          ((smaller (top-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let
          ((right-spli (right-split painter (- n 1)))
           (up-spli (top-split painter (- n 1))))
        (beside (below painter (beside up-spli up-spli)) (below (below right-spli right-spli) (corner-split painter (- n 1)))))))

(define (square-limit painter n)
  (let
      ((corn (corner-split painter n)))
    (below (beside (flip-horiz (flip-vert corn)) (flip-vert corn)) (beside (flip-horiz corn) corn))))

(define (square-of-four tl tr bl br)
  (lambda (painter) (below (beside (bl painter) (br painter)) (beside (tl painter) (tr painter)))))

(define (abs-square-limit painter n)
  (let
      ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define (split op1 op2)
  (lambda (painter n)
    (if (= n 0)
        painter
  (let
      ((smaller ((split op1 op2) painter (- n 1))))
    (op1 painter (op2 smaller smaller))))))

(define abs-right-split (split beside below))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

(define (make-vect x y)
  (cons x y))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))
(define (add-vect a b)
  (make-vect
   (+ (xcor-vect a) (xcor-vect b))
   (+ (ycor-vect a) (ycor-vect b))))
(define (sub-vect a b)
  (make-vect
   (- (xcor-vect a) (xcor-vect b))
   (- (ycor-vect a) (ycor-vect b))))

(define (scale-vect v s)
  (make-vect
   (* (xcor-vect v) s)
   (* (ycor-vect v) s)))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (make-frame-wierd origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cdr (car frame)))
(define (edge2-frame frame)
  (cdr (cdr (car frame))))



                

(define (make-segment start end)
  (cons start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))




(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let
        ((m (frame-coord-map frame)))
      (let
          ((new-origin (m origin)))
        (painter
         (make-frame
          new-origin
          (sub-vect (m corner1) new-origin)
          (sub-vect (m corner2) new-origin)))))))




(define (flip-vertt painter)
  (transform-painter painter (make-vect 0.0 1.0) (make-vect 1.0 1.0) (make-vect 0.0 0.0)))
                     
  
        
;;; previous chapter

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (average x y) (/ (+ x y) 2))

;;; 2.1

;;; Rational Numbers

(define (negative-rat? n d)
  (cond ((or (and (< n 0) (< d 0)) 
             (and (> n 0) (< d 0))) (- n))
        (else n)))

(define (make-rat n d) 
  (let ((g (gcd n d))
        (a (negative-rat? n d))
        (b (abs d))) 
  (cons (/ a g) (/ b g))))

(define (numer x) (car x)) 
(define (denom x) (cdr x))

(define (add-rat x y) 
  (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y)))) 

(define (sub-rat x y) 
  (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y)))) 

(define (mul-rat x y) 
  (make-rat (* (numer x) (numer y)) (* (denom x) (denom y)))) 

(define (div-rat x y) 
  (make-rat (* (numer x) (denom y)) (* (denom x) (numer y))))

(define (equal-rat? x y) 
  (= (* (numer x) (denom y)) (* (numer y) (denom x))))

(define (print-rat x) 
  (newline) 
  (display (numer x)) 
  (display "/") 
  (display (denom x)))

;;; Points, Line Segments, & Rectangles

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment p q) (cons p q))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
(define (length-segment s)
  (let ((p (start-segment s))
        (q (end-segment s)))
  (sqrt (+ (square (- (x-point q) (x-point p)))
           (square (- (y-point q) (y-point p)))))))

(define (midpoint-segment s)
  (let ((p (start-segment s))
        (q (end-segment s)))
  (make-point (average (x-point p) (x-point q))
              (average (y-point p) (y-point q)))))

(define (print-point p)
  (newline)
  (display "(")
  (display
  (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-rect a b c d) (cons a (cons b (cons c d))))
(define (rect-ul r) (car r))
(define (rect-ur r) (car (cdr r)))
(define (rect-ll r) (car (cdr (cdr r))))
(define (rect-lr r) (cdr (cdr (cdr r))))
(define (perimeter-rect r)
  (+ (* 2 (length-segment (make-segment (rect-ul r) (rect-ur r))))
     (* 2 (length-segment (make-segment (rect-ul r) (rect-ll r))))))
(define (area-rect r)
  (* (length-segment (make-segment (rect-ul r) (rect-ur r)))
     (length-segment (make-segment (rect-ul r) (rect-ll r)))))


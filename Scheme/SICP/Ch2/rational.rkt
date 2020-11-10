;;; previous chapter

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

;;; 2.1

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



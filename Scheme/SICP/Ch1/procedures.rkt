(define (square x) (* x x))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (average x y) (/ (+ x y) 2))

(define (improve guess x) (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (improve guess x) guess)) (/ guess 10000)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (cube-improve guess x) 
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cube-good-enough? guess x)
  (< (abs (- (cube-improve guess x) guess)) (/ guess 10000)))

(define (cube-iter guess x)
  (if (cube-good-enough? guess x)
      guess
      (cube-iter (cube-improve guess x) x)))

(define (cubert x)
  (cube-iter 1.0 x))

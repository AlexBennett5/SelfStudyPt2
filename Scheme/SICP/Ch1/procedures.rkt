;;; 1.1

(define (square x) (* x x))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (average x y) (/ (+ x y) 2))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (improve guess) guess)) (/ guess 10000)))
  (define (improve guess) 
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(define (cubert x)
  (define (good-enough? guess)
    (< (abs (- (improve guess) guess)) (/ guess 10000)))
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (define (cubert-iter guess)
    (if (good-enough? guess)
        guess
        (cubert-iter (improve guess))))
  (cubert-iter 1.0))

;;; 1.2


(define (func-ch1 n)
  (define (func-iter c1 c2 c3 count n)
    (cond ((< n 3) n)
          ((= count n) c1)
          (else (func-iter (+ c1 (+ (* 2 c2) (* 3 c3))) c1 c2 (inc count) n))))
  (func-iter 2 1 0 2 n))


(define (pascal row col)
  (cond ((= row 1) 1)
        ((or (= col 1) (= col row)) 1)
        (else (+ (pascal (dec row) (dec col)) (pascal (dec row) col)))))


(define (even? n)
  (= (remainder n 2) 0))

(define (expt b n)
  (define (expt-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (expt-iter (square b) (/ n 2) a))
          (else (expt-iter b (- n 1) (* a b)))))
  (expt-iter b n 1))


(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (mult a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (double (mult a (halve b))))
        (else (+ a (mult a (- b 1))))))

(define (next n) (if (= n 2) 3 (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b) (= (remainder b a) 0))
(define (smallest-divisor n) (find-divisor n 2))
(define (prime? n) (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime n (- (runtime) start-time))
      #f))
(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline)
  #t)

(define (even? n) (= (remainder n 2) 0))

(define (search-for-primes init count)
  (define (search n count)
    (cond ((= count 0) nil)
          ((timed-prime-test n) (cons n (search (+ n 2) (dec count))))
          (else (search (+ n 2) count))))
  (search (if (even? init) (inc init) init) count))


(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a) (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;;; 1.3

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (filter-acc combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (if (filter a) (combiner result (term a)) result))))
  (iter a null-value))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
      dx))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (yk k) (f (+ a (* k h))))
  (define (term k)
    (* (cond ((or (= 0 k) (= n k)) 1)
             ((even? k) 2)
             (else 4))
       (yk k)))
(* (/ h 3) (sum term 0 inc n)))

(define (close-enough? x y) (< (abs (- x y)) 0.001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
  (if (close-enough? neg-point pos-point)
      midpoint
      (let ((test-value (f midpoint)))
        (cond ((> test-value 0)
              (search f neg-point midpoint))
              ((< test-value 0)
              (search f midpoint pos-point))
              (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (< a-value 0) (> b-value 0))
           (search f a b))
          ((and (< b-value 0) (> a-value 0))
           (search f b a))
          (else (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
    (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define (golden-ratio x)
    (fixed-point-print (lambda (x) (+ 1 (/ 1 x))) 1.0))

(define (log-solution x)
    (fixed-point-print (lambda (x) (/ (log 1000) (log x))) 1.5))

(define (fixed-point-print f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
    (display guess)
    (display " -> ")
    (display next)
    (newline)
    (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define (cont-frac n d k)
  (define (cont-aug q)
    (cond ((= q k) (/ (n q) (d q)))
          (else (/ (n q) (+ (d q) (cont-aug (inc q)))))))
  (cont-aug 1))


(define (cont-frac-iter n d k)
  (define (cont-aug q result)
    (if (= q 0) result (cont-aug (dec q) (/ (n q) (+ (d q) result)))))
  (cont-aug k 0))

(define (e-minus-two)
  (cont-frac-iter (lambda (i) 1.0)
                  (lambda (i) (if (= (remainder i 3) 2)
                                  (* 2 (quotient (+ i 1) 3))
                                  1))
                  100))

(define (tan-cf x k)
  (cont-frac-iter (lambda (i) (if (= i 1) x (- (square x))))
                  (lambda (i) (+ 1 (* 2 (- i 1))))
                  k))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cube x) (* x x x))
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(define (double f)
  (lambda (x) (f (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1) f (compose f (repeated f (dec n)))))

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (repeated-smooth f n)
  ((repeated smooth n) f))

(define (iterative-improve check improve)
  (lambda (x) (if (check x) x ((iterative-improve check improve) (improve x)))))

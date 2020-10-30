
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

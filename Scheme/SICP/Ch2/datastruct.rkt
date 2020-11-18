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

;;; Interval Arithmetic

(define (make-interval a b) (cons a b))
(define (lower-bound inv) (car inv))
(define (upper-bound inv) (cdr inv))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
  (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (spans-zero? inv)
  (and (>= (upper-bound inv) 0)
       (<= (lower-bound inv) 0)))

(define (div-interval x y)
  (if (spans-zero? y)
      (error "div-interval cannot divide a interval than spans 0")
      (mul-interval x
        (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y))))))

;;; Misc

(define (cons-expt a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (log-reduce n base)
  (cond ((not (= (remainder n base) 0)) 0)
        (else (+ (log-reduce (/ n base) base) 1))))

(define (car-expt num)
  (log-reduce num 2))
(define (cdr-expt num)
  (log-reduce num 3))

;;; 2.2

(define (list-ref items n) 
  (if (= n 0) 
      (car items) 
      (list-ref (cdr items) (- n 1))))

(define (length items) 
  (define (length-iter a count) 
    (if (null? a) 
        count 
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (append list1 list2) 
  (if (null? list1) 
      list2 
      (cons (car list1) (append (cdr list1) list2))))

(define (last-pair ls)
  (cond ((null? ls) (error "Can't submit empty list"))
        ((null? (cdr ls)) ls)
        (else (last-pair (cdr ls)))))

(define (reverse ls)
  (if (null? ls)
      nil
      (append (reverse (cdr ls))
              (list (car ls)))))

(define (no-more? coin-values) (null? coin-values))
(define (except-first-denomination ls) (cdr ls))
(define (first-denomination ls) (car ls))

(define (cc amount coin-values) 
  (cond ((= amount 0) 1) 
        ((or (< amount 0) (no-more? coin-values)) 0) 
        (else (+ (cc amount 
                   (except-first-denomination coin-values)) 
                 (cc (- amount 
                   (first-denomination coin-values)) coin-values)))))

(define (just-even ls)
  (cond ((null? ls) nil)
        ((even? (car ls)) (cons (car ls) (just-even (cdr ls))))
        (else (just-even (cdr ls)))))

(define (just-odd ls)
  (cond ((null? ls) nil)
        ((odd? (car ls)) (cons (car ls) (just-odd (cdr ls))))
        (else (just-odd (cdr ls)))))

(define (same-parity x . y)
  (if (even? x)
      (cons x (just-even y))
      (cons x (just-odd y))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (for-each proc ls)
  (cond ((null? ls) #f)
        ((null? (cdr ls)) (proc (car ls)))
        (else (proc (car ls))(for-each proc (cdr ls)))))

(define (count-leaves x) 
  (cond ((null? x) 0) 
        ((not (pair? x)) 1) 
        (else (+ (count-leaves (car x)) 
                 (count-leaves (cdr x))))))

(define (deep-reverse tree)
  (if (pair? tree)
      (reverse (map deep-reverse tree))
      tree))

(define (fringe tree)
  (cond ((null? tree) nil)
        ((pair? tree) (append (fringe (car tree))
                              (fringe (cdr tree))))
        (else (list tree))))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

(define (leaf? x) (not (pair? x)))

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (leaf? sub-tree)
             (proc sub-tree)
             (tree-map proc sub-tree)))
       tree))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (subset) (cons (car s) subset)) rest)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
          (cons (car sequence)
                (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree) 
  (cond ((null? tree) nil) 
        ((not (pair? tree)) (list tree)) 
        (else (append (enumerate-tree (car tree)) 
                      (enumerate-tree (cdr tree))))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) 
                      (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))


(define (count-leaves tree)
  (accumulate + 0 (map (lambda (root)
                         (if (leaf? root)
                             1
                             (count-leaves root)))
                       tree)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op init seq) (accumulate op init seq))

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))


;;;;;;;;;;;;;;;;;;;;;;;;
;; exercises 1.3
(define (max-of-two x y)
  (if (> x y)
      x
      y))
(define (larger x y z)
  (max-of-two (max-of-two x y) z))

;;;;;;;;;;;;;;;;;;;;;;;;
;; exercises 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -)  ;; an operator will be reutrned as a procedure
   a
   b))

;;;;;;;;;;;;;;;;;;;;;;;;
;; exercises 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
(test 0 (p))  ;; ChezScheme has not output
;; because calling (p) would fall in infinite loop

;;;;;;;;;;;;;;;;;;;;;;;;
;; exercises 1.6
;; (define (square x)
;;   (* x x))
;; (define (sqrt-inter guess x)
;;   (if (good-enough? guess x)
;;       guess
;;       (sqrt-inter (improve guess x)
;; 		  x)))
;; (define (improve guess x)
;;   (average guess (/ x guess)))
;; (define (average x y)
;;   (/ (+ x y) 2))
;; (define (good-enough? guess x)
;;   (< (abs (- (square guess) x)) 0.001))
;; (define (sqrt x)
;;   (sqrt-inter 1.0 x))
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))
(define (square x)
  (* x x))
(define (sqrt-inter guess x)
  (new-if (good-enough? guess x)
      guess
      (sqrt-inter (improve guess x)
		  x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-inter 1.0 x))

;; the new-if function just can wrok with recursion
;; because ChezScheme evaluates the expresion in applicative order(depth first) rather than
;; normal order(breadth-first)
;; so recursion of sqrt-inter can not stop
;; so then new-if can not work


;;;;;;;;;;;;;;;;;;;;;;;;
;; exercises 1.8
(define (cbrt-iter guess x)
  (if (cube-good-enough? guess x)
      guess
      (cbrt-iter (improve-cubic guess x) x)))
(define (cube-good-enough? guess x)
  (< (abs (- (* (* guess guess) guess) x)) 0.001))
(define (improve-cubic guess x)
  (/ (+ (/ x (* guess guess)) (* 2.0 guess)) 3.0))
(define (cbrt x)
  (cbrt-iter 1.0 x))
;; if use int insteat of float, cbrt may not correctly
;; or rewrite as below
(define (encapsuled-cbrt x)
  (define (cbrt-iter guess x)
    (if (cube-good-enough? guess x)
	guess
	(cbrt-iter (improve-cubic guess x) x)))
  (define (cube-good-enough? guess x)
    (< (abs (- (* (* guess guess) guess) x)) 0.001))
  (define (improve-cubic guess x)
    (/ (+ (/ x (* guess guess)) (* 2.0 guess)) 3.0))
  (cbrt-iter 1.0 x))

;;;;;;;;;;;;;;;;;;;;;;;;
;; exercises 1.10
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))
;;          | 0, y = 0
;;          | 2y, x = 0
;;A(x, y) = | 
;;          | 2, y = 1
;;          | A(x-1, A(x, y-1))
(define (f n) (A 0 n)) ;; => 2n
(define (g n) (A 1 n)) ;; => 2^n
(define (h n) (A 2 n)) ;; => 2^(2^(2^(2^(...))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.11
;; assume that n is non-negative
(define (lalala n)
  (cond ((< n 3) n)
	(else (+ (lalala (- n 1))
		 (* 2 (lalala (- n 2)))
		 (* 3 (lalala (- n 3)))))))
(define (lalala-iteration n)
  (define (lalala-iter a b c count)
    (cond ((< count 3) count)
	  ((= count 3) (+ a (* 2 b) (* 3 c)))
	  (else (lalala-iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
  (lalala-iter 2 1 0 n))

;;;;;;;;;;;;;;;;;;;;;;;;
;; exercises 1.12
;; x is then xth line and y is the nth element in xth line
(define (pascal x y)
  (cond ((or (= x 2) (= x 1)) 1)
	((= y 1) 1)
	((= x y) 1)   ;; may not be safe
	(else (+ (pascal (- x 1) (- y 1)) (pascal (- x 1) y)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.16
(define (fast-exp b n)
  (define (fast-exp-h a b n)
    (cond ((= n 1) a)  ;; notice it is (= n 1) not (= n 0)
	  ((odd? n) (fast-exp-h (* a b) b (- n 1)))
	  (else (fast-exp-h (* a (* b b)) b (/ n 2)))))
  (cond ((= n 1) b)
	(else (fast-exp-h 1 b n))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.17
(define (fast-mult x y)
  (define (double x)
    (* 2 x))
  (define (halve x)
    (/ x 2))
  (cond ((= y 1) x)
	((even? y) (double (fast-mult x (halve y))))
	(else (+ x (double (fast-mult x (halve (- y 1))))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.19
;; let's compute p' and q'
;; first time: a <- bq + aq + ap = a'
;;             b <- bp + aq = b'
;; second time: a' = b'q + a'q + a'p
;;                 = (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
;;                 = bpq + aq^2 + bq^2 + aq^2 + apq + bqp + apq + ap^2
;;                 = (2pq + q^2)b + (2pq + 2q^2 + p^2)a
;;                 = (2pq + q^2)b + (2pq + q^2)a + (p^2 + q^2)a
;;              b' = b'p + a'q
;;                 = (bp + aq)p + (bq + aq + ap)q
;;                 = bp^2 + apq + bq^2 + aq^2 + apq
;;                 = (p^2 + q^2)b + a(2pq + q^2)
;; change it to the form of transformation
;; so q' = (2pq + q^2)
;;    p' = (p^2 + q^2)
;; so we have code below
(define (fib n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
	  ((even? count)
	   (fib-iter a
		     b
		     (+ (* p p) (* q q))
		     (+ (* 2 p q) (* q q))
		     (/ count 2)))
	  (else (fib-iter (+ (* b q) (* a q) (* a p))
			   (+ (* b p) (* a q))
			   p
			   q
			   (- count 1)))))
  (fib-iter 1 0 0 1 n))

;; now let's skip section2 to section3
;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.30
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) b))))
(define (iter-sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result a))))
  (iter a 0))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.31
(define (product term a next b)
  (if (> a b)
      0
      (* (term a) (product term (next a) b))))
(define (iter-product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* (term a) result))))
  (iter a 1))

(define (fact-next a) (+ a 1))
(define (fact-term a) a)
(define (fact-using-prod b)
  (iter-product fact-term 1 fact-next b))

(define (pi-next a) (+ a 1))
(define (pi-term a)
  (cond ((even? a) (/ (+ a 2) (+ a 1)))
	(else (/ (+ a 1) (+ a 2)))))
(define (quatre-pi b)
  (iter-product pi-term 2 pi-next b))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner
			    null-value
			    term
			    (next a)
			    next
			    b))))
      
(define (iter-accumulate combiner null-value term a next b)
  (define (iter combiner a result)
    (if (> a b)
	result
	(iter combiner (next a) (combiner a result))))
  (iter combiner a null-value))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.33
(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term (filter a))
		(filtered-accumulate filter
				     combiner
				     null-value
				     term
				     (next a)
				     next
				     b))))
      
(define (iter-filtered-accumulate filter combiner null-value term a next b)
  (define (iter combiner a result)
    (if (> a b)
	result
	(iter combiner (next a) (combiner (filter a) result))))
  (iter combiner a null-value))


;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.34
;; exception: attempt to apply non-procedure 2

;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.37
(define (cont-frac n d k)
  (if (<= k 1)
      (d k)
      (/ (n k) (+ (d k) (cont-frac n d (- k 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.38
(define (e-cont-frac n d k)
  (cont-frac (lambda (x) 1.0)
	     (lambda (x)
	       (let ((remain (remainder (- x 1) 3)))
		 (cond ((= remain 0) 1.0)
		       ((= remain 2) 1.0)
		       (else (* 2 (+ 1 (/ (- x 1) 3)))))))
	     k))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.39
;; unkknow

;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.40
(define (cube x)
  (* (* x x) x))
(define (square x)
  (* x x))
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.41
(define (double p)
  (lambda (x) (p (p x))))
(define (inc x) (+ x 1))
((double inc) 1)
(((double (double double)) inc) 5) ;; => +16

;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.42
(define (compose f g)
  (lambda (x) (f (g x))))
((compose square inc) 6)

;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.43
(define (repeat p k)
  (cond ((<= k 0) (lambda (x) x))
	((= k 1) (lambda (x) (p x)))
	(else (compose p (repeat p (- k 1))))))
((repeat square 3) 5)

;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.44
(define (smooth f)
  (lambda (x dx)
    (/ (+ (f (- x dx))
	  (f x)
	  (f (+ x dx)))
       3)))
(define (n-fold-smooth f k)
  (repeat (smooth f) k))

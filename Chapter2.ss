(define (square a)
  (* a a))

(define (gcd x y)
  (if (= y 0)
      x
      (gcd y (remainder x y))))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom y)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom y)))
	    (* denom x) (denom y)))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom y) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom y) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.1
(define (make-rat a b)
  (let ((abs-a (abs a))
	(abs-b (abs b)))
    (let ((g (gcd abs-a abs-b)))
      (if (> 0 (* a b))
	  (cons (- (/ abs-a g)) (/ abs-b g))
	  (cons (/ abs-a g) (/ abs-b g))))))


;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.2
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (midpoint p1 p2)
  (define (halve a)
    (/ a 2))
  (make-point (halve (+ (x-point p1) (x-point p2)))
	      (halve (+ (y-point p1) (y-point p2)))))

(define (distance-point p1 p2)
  (let ((x (- (x-point p1) (x-point p2)))
	(y (- (y-point p1) (y-point p2))))
    (sqrt (+ (square x) (square y)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start end)
  (cons start end))

(define (start-segment line)
  (car line))

(define (end-segment line)
  (cdr line))

(define (midpoint-segment seg)
  (midpoint (start-segment seg) (end-segment seg)))

(define (leng-segment seg)
  (distance-point (start-segment seg) (end-segment seg)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.3
(define line-a (make-segment (make-point 1 3) (make-point 4 3)))
(define line-b (make-segment (make-point 4 5) (make-point 4 9)))
(define line-c (make-segment (make-point 3 9) (make-point 3 4)))

(define (make-triangle a b c)
  ;; a b c are all of segment type
  (cons (cons a b) c))
(define tri-a (make-triangle line-a line-b line-c))
;; tri-a => ((((1 . 3) 4 . 3) (4 . 5) 4 . 9) (3 . 9) 3 . 4)
(define (perimeter-tri tri)
  (let ((a (car (car tri)))
	(b (cdr (car tri)))
	(c (cdr tri)))
    (+ (leng-segment a)
       (leng-segment b)
       (leng-segment c))))

(define (area-tri tri)
  (let ((a (leng-segment (car (car tri))))
	(b (leng-segment (cdr (car tri))))
	(c (leng-segment (cdr tri))))
    (let ((p (/ (+ a b c) 2)))
      (sqrt (* p
	       (- p a)
	       (- p b)
	       (- p c))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.4
;; marvelous work
;; (define (cons x y)
;;   (lambda (m) (m x y)))
;; (define (car z)
;;   (z (lambda (p q) p)))
;; (car (cons 1 2))
;; => (car (lambda (m) (m 1 2)))
;; => (lambda (lambda (p q) p) (lambda (p q) p ) 1 2)
;; => 1

;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.5
(define (mcons x y)
  (define (xxp base ex)
    (if (= ex 0)
	1
	(* base (xxp base (- ex 1)))))
  (* (xxp 2 x) (xxp 3 y)))

(define (take z n)
  (if (= (remainder z n) 0)
      (+ 1 (mcar (/ z n)))
      0))
(define (mcar z)
  (take z 2))
(define (mcdr z)
  (take z 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exercises of interval arithmatics are skipped
(car (cons 1 2))
;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.17
(define (length a)
  (if (null? a)
      0
      (+ 1 (length (cdr a)))))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (last-pair a)
  (if (null? a)
      '()
      (list-ref a (- (length a) 1))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.18
;; you can use cons for it generates a larger pair
;; use append instead
;; notice the (list (car a))
(define (reverse a)
  (if (null? a)
      a
      (append (reverse (cdr a))  (list (car a)))))


;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.19
(define (cc amount coin-values)
  (define no-more? null?)
  (define except-first-denomination cdr)
  (define first-denomination car)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else
	 (+ (cc amount
		(except-first-denomination coin-values))
	    (cc (- amount
		   (first-denomination coin-values))
		coin-values)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.20
(define (same-parity . rest)
  (define (filter a)
    (if (even? a)
	even?
	odd?))
  (define (build fil l)
    (if (null? l)
	l
	(if (fil (car l))
	    (cons (car l)
		  (build fil (cdr l)))
	    (build fil (cdr l)))))
  (build (filter (car rest)) rest))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.21
(define (square-list l)
  (if (null? l)
      l
      (cons (* (car l) 2)
	    (square-list (cdr l)))))
(define (square-list l)
  (map square l))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.23
(define (mfor-each p l)
  (if (null? l)
      l
      (p (car l)))
  (if (null? l)
      l
      (mfor-each p (cdr l))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.25
(define (find-seven l)
  (cond ((null? l) l)
	((pair? l)
	 (find-seven (car l))
	 (find-seven (cdr l)))
	(else (= l 7) #t)))
(find-seven '(1 3 (5 7) 9))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.27
;; notice the two list procedure
;; car will unnest a nested list
;; so you have to use list to nest the list again 
(define (deep-reverse l)
  (cond ((null? l) l)
	((not (pair? l)) l)
	(else (append (deep-reverse (cdr l))
		      (if (list? (car l))
			  (list (deep-reverse (car l)))
			  (list (car l)))))))
;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.28
;; pay attention to the (list tj)
(define (fringe t)
  (cond ((null? t) t)
	((not (list? t)) (list t))
	(else (append (fringe (car t)) (fringe (cdr t))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define left-branch car)

(define right-branch
  (lambda (x)
    (car (cdr x))))

(define branch-length car)
(define branch-structure
  (lambda (x)
    (car (cdr x))))
(define a-mobile (make-mobile (make-branch 12 34)
			      (make-branch 13 (make-mobile (make-branch 14 15)
							   (make-branch 16 17)))))
a-mobile ;; => ((12 34) (13 ((14 15) (16 17))))
(left-branch a-mobile)
(right-branch a-mobile)
(branch-length (left-branch a-mobile))
(branch-structure (left-branch a-mobile))
(branch-length (right-branch a-mobile))
(branch-structure (right-branch a-mobile))

(define (total-weight l)
  (cond ((not (list? l)) 0) ;; a branch must be a list. A sigle number must be length
	((not (list? (branch-structure l)))
	 (branch-structure l))
	(else (+ (total-weight (left-branch l))
		 (total-weight (right-branch l))))))
(total-weight a-mobile) ;; => 66

(define (balance? m)
  (define (branch-weight b)
    (cond ((not (list? (branch-structure b)))
	   (* (branch-length b)
	      (branch-structure b)))
	  (* (branch-length b)
	     (branch-weight (branch-structure b)))))
  (= (branch-weight (left-branch m))
     (branch-weight (right-branch m))))
(define balanced-mobile (make-mobile (make-branch 13 14)
	     (make-branch 14 13)))
;; use cons intead of list just leads to change in branch-structure and right-branch

;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.30
(define (square-tree t)
  (map (lambda (sub-tree)
	 (cond ((not (pair? sub-tree))
		(square sub-tree))
	       (else (square-tree sub-tree))))
       t))
;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.31
(define (tree-map p t)
  (map (lambda (sub-tree)
	 (cond ((not (pair? sub-tree))
		(p sub-tree))
	       (else (tree-map p sub-tree))))
       t))
(define (sq-tr t) (tree-map square t))

;;;;;;;;;;;;;;;;;;;;;;;;
;kjijhjhvgfescyntchyxxxxx         ;; 2.32
;; the rest is all the subsets that exclude the first element
;; so we need all the subsets that include the first element
(define (subsets s)
  (cond ((null? s) (list '()))
	(else (let ((rest (subsets (cdr s))))
		(append rest (map (lambda (x)
				    (cons (car s) x))
				  rest))))))
;; Now I understand why Haskell uses guard.
;; functional programming is totally different from imperative programming

;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))
  
(define (mmap p sequence)
  (accumulate (lambda (x y)
		(cons (p x) y))
	      '()
	      sequence))
(define (append seq1 seq2)
  (accumulate cons
	      seq2
	      seq1))

(define (length sequence)
  (accumulate (lambda (x y)
		(+ 1 y))
	      0
	      sequence))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ (* x higher-terms) this-coeff))
	      0
	      coefficient-sequence))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.35
(define (flatten t)
  (cond ((null? t) '())
	((not (pair? t)) (list t))
	(else (append (flatten (car t))
		      (flatten (cdr t))))))
(define (count-leaves tree)
  (accumulate (lambda (x y)
		(+ 1 y))
	      0
	      (flatten tree)))
;; actually I just compute the length of a flattened nest listed

;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.36
;; the (map car seqs) and (map cdr seqs) take
;; out corresponding elements out of the sequences
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      init
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))


;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.37
(define test-matrix '((1 2 3)
		      (4 5 6)
		      (7 8 9)))
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x)
	 (dot-product x v))
       m)) ;; => notice that map would return a vector

(define (transpose mat)
  (accumulate-n cons
		'()
		mat))

;; use vectors directly
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x)
	   (matrix-*-vector cols x)) m)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.38
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (fold-right op initial (cdr sequence)))))
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))
(fold-right / 1 '(1 2 3)) ;; => 3/2
(fold-left / 1 '(1 2 3))  ;; => 1/6
(fold-right list '() '(1 2 3)) ;; => (1 (2 (3 ())))
(fold-left list '() '(1 2 3))  ;; => (((() 1) 2) 3)
;; but if the operator is itself commutative
;; not important, then result of fold-right
;; and fold-left would be the same

;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.39
;; notice the difference between append and cons
(define (right-reverse l)
  (fold-right (lambda (x y)
		(append y (list x))) '() l))
(define (left-reverse l)
  (fold-left (lambda (x y)
	       (cons y x)) '() l))
(right-reverse '(1 2 3 4 5))
(left-reverse '(1 2 3 4 5))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.40
;; to be continued

;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.41
;; to be continued

;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.42
;; to be continued

;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.43
;; to be continued

(define (interval a b)
  (cond ((= a b) (list a))
	(else (append (list a) (inter (+ 1 a) b)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; I skip exercises in 2.2.4 Example: A Picture Language

;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.54
(define (memq item x)
  (cond ((null? x) #f)
	((eq? item (car x)) x)
	(else (memq item (cdr x)))))

(define (equal? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
	((or (null? l1) (null? l2)) #f)
	(else (and (eq? (car l1) (car l2))
		   (equal? (cdr l1) (cdr l2))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.55
(car ''abracadabra)
(define a ''abracadabra)
a ;; => 'abracadabra

;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.3.2 Symbolic Differentiation
(define (=number? exp num)
  (and (number? exp)
       (= exp num)))
(define (variable? e)
  (symbol? e))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))
(define (sum? e)
  (and (pair? e)
       (eq? (car e) '+)))
(define (addend e) (cadr e))
(define (augend e) (caddr e))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))
(define (product? e)
  (and (pair? e)
       (eq? (car e) '*)))
(define (multiplier e) (cadr e))
(define (multiplicand e) (caddr e))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	((exponentiation? exp)
	 (make-product (make-product (exponent exp)
				     (deriv (base exp) var))
		       (make-exponentiation (base exp)
					    (- (exponent exp) 1))))
	(else
	 (error "unknown expression type -- DERIV" exp))))
;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.56
(define (exponentiation? e)
  (and (pair? e)
       (eq? (car e) '**)))
(define (base e)
  (cadr e))
(define (exponent e)
  (caddr e))
(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
	((=number? e 1) b)
	((=number? b 1) 1)
	(else (list '** b e))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Maybe I will go back to this chapter next semester.

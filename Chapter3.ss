;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.1
(define (make-accumulator base)
  (lambda (inc)
    (begin
      (set! base (+ inc base))
      base)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.2
(define (make-monitored p)
  (let ((counter 0))
    (lambda (option)
      (begin
	(cond ((eq? option 'how-many-calls?)
	       (begin
		 (set! counter (+ counter 1))
		 counter))
	      (else
	       (p option)))))))
  

;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.8
(define f
  (let ((called #f))
    (lambda (num)
      (if called
	  0
	  (begin
	    (set! called #t)
	    num)))))
(define (mystery x)
  (define (loop x y)
    (if (null? x)
	y
	(let (( temp (cdr x)))
	  (set-cdr! x y)
	  (loop temp x))))
  (loop x '()))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.17
;; memq checks the existence of an element in a list
(define (count-pair x)
  (let ((encountered '()))
    (if (or (not (pair? x))
	    (memq x encountered))
	0
	(begin
	  (set! encountered (cons x encountered))
	  (+ (count-pair (car x))
	     (count-pair (cdr x))
	     1)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.18
(define (cycle? x)
  (let ((record '()))
    (cond ((not (pair? x)) #f)
	  ((memq x record) #t)
	  (else
	   (begin
	     (set! record (cons x record))
	     (or (cycle? (car x))
		 (cycle? (cdr x))))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.19
;; copied from community.schemewiki.org/?sicp-ex-3.19
(define (contaons-cycle? lst)
  (define (safe-cdr l)
    (if (pair? l)
	(cdr l)
	'()))
  (define (iter a b)
    (cond ((not (pair? a)) #f)
	  ((not (pair? b)) #f)
	  ((eq? a b) #t)
	  ((eq? a (safe-cdr b)) #t)
	  (else (iter (safe-cdr a)
		      (safe-cdr (safe-cdr b))))))
  (iter (safe-cdr lst)
	(safe-cdr (safe-cdr lst))))



(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (steram-map proc s)
  (if (steram-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
		   (steram-map proc (stream-cdr s)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
	     (stream-for-each proc (stream-cdr s)))))
(define (cons-stream a b)
  (cons a (delay b)))
(define (stream-car stream)
  (car stream))
(define (stream-cdr stream)
  (force (cdr stream)))
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))


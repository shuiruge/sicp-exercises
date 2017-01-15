; 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
(test 0 (p))
;; returning an endless

;; Explain:
;; Because of applicative order, that is "the innermost runs the first", when evaluate `(test 0 (p))`, the operand `(p)` will be evaluated before evaluating `test`. However, `p` is defined as a endless loop.
;; Contrarily, for a normal order interpreter, judegement is taken first. Thus no opportunity would `p` be run.
;; Really???

;; Another way of doing so is:
(define (new-p)
  (define (env) 73))
(define (new-test x y)
  (if (= x 0)
      0
      y))
(new-test 0 (new-p))
(env)


; 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))
(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
	  guess
	  (new-sqrt-iter (improve guess x)
		     x)))
;; wherein:
(define (good-enough? guess x)
  (< (distance (* guess guess)
	       x)
     0.001))
(define (distance x y)
  (abs (- x y)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
;; as comparision
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

;; run in bigloo
(new-sqrt-iter 1 2)
;; returning ERROR
;;     *** ERROR:bigloo:
;;     `segmentation violation' exception -- raised
;;      1. improve, stdin@607
;;      2. new-sqrt-iter, stdin@235
;;      ......
;;      10. new-sqrt-iter, stdin@235

;; Explain:
;; When calling `new-if` in the first level of recursion of `new-sqrt-iter`, before evaluating it, its operand shall be evaluated first (That is, "the innermost runs the first". C.f. section 1.1.5), **even before evaluating `cond`!** The same will happen in the second level of recursion, wherein the `cond` will not be evaluated too. And so on, and so on. Without making a judgement, never will this recursion stop. This maybe how things go wrong.
;; Contrarily, `sqrt-iter` runs well, since the judgement is taken **before** calling the recursion.

;; Conclusion:
;; Bug is hard to be avoided!


; 1.7
(define (new-good-enough? guess x)
  (< (relative-distance guess
			(improve guess x))
     0.001))
(define (relative-distance x y)
  (/ (distance x y)
     (abs x)))
(define (nnew-sqrt-iter guess x)
  (if (new-good-enough? guess x)
      guess
      (nnew-sqrt-iter (improve guess x)
		      x)))
(nnew-sqrt-iter 1 2)
(sqrt-iter 1 2)
;; no improvement?


; 1.8
(define (improve-cubic guess x)
  (/ (+ (/ x (* guess guess))
	(* 2 guess))
     3))
(define (good-enough?-cubic guess x)
  (< (distance (* guess guess guess)
	       x)
     0.001))
(define (sqrt-iter-cubic guess x)
  (if (good-enough?-cubic guess x)
      guess
      (sqrt-iter-cubic (improve-cubic guess x)
		       x)))
(sqrt-iter-cubic 1 8)


; 1.9
;; For the first:
;; 
;; (+ 4 5)
;; (inc (+ (dec 4) 5))
;; (inc (+ 3 5))
;; (inc (inc (+ (dec 3) 5)))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (inc (+ (dec 2) 5)))))
;; (inc (inc (inc (inc (+ 1 5)))))
;; (inc (inc (inc (inc (inc (+ (dec 1) 5))))))
;; (inc (inc (inc (inc (inc (+ 0 5))))))
;; (inc (inc (inc (inc (inc 5)))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9
;; 
;; which is manifestly recursive.
;; And for the second:
;; 
;; (+ 4 5)
;; (+ (dec 4) (inc 5))
;; (+ 3 6)
;; (+ (dec 3) (inc 6))
;; (+ 2 7)
;; (+ (dec 2) (inc 7))
;; (+ 1 8)
;; (+ (dec 1) (inc 8))
;; (+ 0 9)
;; 9
;; 
;; which is manifestly iterative.

; 1.10
; Q1-1:
;; (A 1 2)
;; (A 0
;;   (A 1 1))
;; (A 0 2)
;; 4 = 2^2
;; ......
;; (A 1 10) returns 2^10
;; 
;; Q1-2:
;; (A 2 4)
;; (A 1
;;   (A 2 3))
;; (A 1
;;   (A 1 
;;     (A 2 2)))
;; (A 1
;;   (A 1 
;;     (A 1 
;;       (A 2 1))))
;; (A 1
;;   (A 1 
;;     (A 1 2)))
;; (A 1
;;   (A 1 2^2))
;; (A 1 2^2^2)
;; 2^2^2^2
;;
;; Q1-3:
;; (A 3 3)
;; (A 2
;;   (A 3 2))
;; (A 2
;;   (A 2
;;     (A 3 1)))
;; (A 2
;;   (A 2 2))
;; (A 2 2^2)
;; (A 2 4)
;; 2^2^2^2
;;
;; Q2:
;; f(n) = 2 * n
;; g(n) = 2 ^ n
;; h(n) = 2 ^ ... ^ 2, with n 2s.

; 1.11
;; tree-recursion
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2
            (f (- n 2)))
         (* 3
            (f (- n 3))))))

;; iteration
(define (f n)
  (if (< n 3)
      n
      (f-iter n 3 2 1 0)))
(define (f-iter n count f-count-1 f-count-2 f-count-3)
  (if (= (- count 1) n)
        f-count-1
        (f-iter n
                (+ count 1)
                (+ f-count-1
                   (* 2 f-count-2)
                   (* 3 f-count-3))
                f-count-1
                f-count-2)))

; 1.12
(define (pascal-triangle row column)
  (cond ((> column row) 'empty)
        ((or (= column 1)
             (= column row))
          1)
        (else (if (= row 1)
                  1
                  (+ (pascal-triangle (- row 1)
                                      (- column 1))
                     (pascal-triangle (- row 1)
                                      column))))))

; 1.13
;; A math problem???

; 1.14
;; What is a cent???

; 1.15
;; Q-a:
;; log_3(12.5 / 0.1)
;; Q-b:
;; Theta of a and Theta of log(a).

; 1.16
define (fast-expt b n)
  (fast-expt-iter b n 1))
(define (fast-expt-iter b n a)
  (if (= n 0)
      a
      (if (even? n)
          (fast-expt-iter (* b b)
                          (/ n 2)
                          a)
          (fast-expt-iter b
                          (- n 1)
                          (* a b)))))

; 1.17
(define (fast-multiply n b)
  (fast-multiply-iter n b 0))
(define (fast-multiply-iter n b product)
  (if (= n 0)
      product
      (if (even? n)
          (fast-multiply-iter (halve n) (double b) product)
          (fast-multiply-iter (- n 1) b (+ product b)))))
;; and for self-consistency:
(define (halve a)
  (/ a 2))
(define (double a)
  (* a 2))

; 1.18
;; Isn't this 1.17 again???

; 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* q q) (* p p))	      ; compute p'
                   (+ (* 2 p q) (* q q))      ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
;; by comparison with the most original version of `fib`, this does be amazingly fast.

; 1.20
;; A possible answer: There's no end since, if "fully expand and then compute" (normal-order), then no part of the argument of `gcd` will be `0` so as to trigger the terminator. So, there will be infinitely many `reminder` for normal-order interpreter.
;; But this is wrong! The reason is that, when `if` is called, the condition must be exacuted, even in normal-order interpreter! So, there does be an end.
;; So, the correct answer is, instead, 
(define (exacuted-reminders-in-normal-order total-steps reminders-in-a reminders-in-b)
  (define (exacuted-by-if)
    reminders-in-b)
  (if (= steps 1)
      (+ (exacuted-by-if) reminders-in-a)
      (+ (exacuted-reminders-in-normal-order (- total-steps 1)
                             reminders-in-b
                             (+ reminders-in-a reminders-in-b 1))
          (exacuted-by-if))))
(exacuted-reminders-in-normal-order 5 0 0)
;; returns 18.
;; And applicative-order exacutes
(define (exacuted-reminders-in-applicative-order total-steps)
  (define (exacuted-reminders-in-applicative-order-iter total-steps counter) ;no reminders in a and b in applicative-order
    (if (= total-steps 1)
        counter
        (exacuted-reminders-in-applicative-order-iter (- total-steps 1)
                                                      (+ counter 1))))
  (exacuted-reminders-in-applicative-order-iter total-steps 0))
(exacuted-reminders-in-applicative-order 5)
;; returns 4.

; 1.21
;; just run it.

; 1.22
; 1.23
; 1.24
;; No `runtime` in Scheme!!!

; 1.25
;; I cannot understand the `expmod` function defined in the texture!

; 1.26
;; If you use `(square expr)`, the `expr` will be computed once. But, it were twice if you use `(* expr expr)` instead. This makes a tree recursion, which is Theta of exponential. By exp(ln n) = n, we find it is Theta of n.

; 1.27
;; No `random` in Scheme.

; 1.28
(define (Miller-Rabin-test n)
  (define (try-it a)
    (if (non-trivial-square-root-of-1-modulo-n? a n)
        false
        (= (expmod a (- n 1) n) a)))
  (try-it (+ 1 (random (- n 1)))))
(define (non-trivial-square-root-of-1-modulo-n? a n)
  (cond (or (= a 1)
            (= a (- n 1)))
        false
        (= 1
           (reminder a n))))
;; However, we cannot run it to test this code, since no `runtime` in Scheme.

; 1.29
;; Formula of Simpson rule follows [WikiPedia](https://en.wikipedia.org/wiki/Simpson%27s_rule#Composite_Simpson.27s_rule).
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (simpson-rule-integral integrand a b n)
  (define (h)
    (/ (- b a) n))
  (define (simpson-rule-integrand x)
    (+ (integrand x)
       (* 4 (integrand (+ x (h))))
       (integrand (+ x (* 2 (h))))))
  (define (simpson-rule-next x)
    (+ x (* 2 (h))))
  (* (/ (h) 3)
     (sum simpson-rule-integrand a simpson-rule-next b)))
(define (cube x) (* x x x))
(simpson-rule-integral cube 0.0 1.0 100)
;; => 0.25000000000000044

; 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
	      (+ result
		 (term a)))))
  (iter a 0))
;; test:
(time (simpson-rule-integral cube 0.0 1.0 100))
;; => 0.25000000000000033
;; Compared with:
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(time (simpson-rule-integral cube 0.0 1.0 100))
;; it's a little faster.

; 1.31-a
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))
(define (inc x)
  (+ x 1))
(define (identity x)
  x)
(define (factorial-product n)
  (product identity 1 inc n))
(factorial-product 4)
;; => 24
(define (term-for-pi n)
  (* (/ (- n 1.0)
	n)
     (/ (+ n 1.0)
	n)))
(define (inc-2 x)
  (+ x 2))
(define (product-for-pi n)
  (product term-for-pi 3 inc-2 n))
(* 4
   (product-for-pi 1000))
;; => 3.143163842419195

; 1.31-b
(define (product-iter term a next b result)
  (if (> a b)
      result
      (product-iter term (next a) next b
		    (* result (term a)))))
(define (product term a next b)
  (product-iter term a next b 1))
;; test:
(factorial-product 4)
(* 4
   (product-for-pi 1000))

; 1.32-a
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))
;; test:
(define (product term a next b)
  (accumulate * 1 term a next b))
(factorial-product 4)

; 1.32-b
(define (accumulate-iter combiner term a next b result)
  (if (> a b)
      result
      (accumulate-iter combiner term (next a) next b
		       (combiner result
				 (term a)))))
(define (accumulate combiner null-value term a next b)
  (accumulate-iter combiner term a next b null-value))
;; test:
(factorial-product 4)

; 1.33
(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (filter (term a))
	  (combiner (term a)
		    (accumulate combiner null-value term (next a) next b))
	  null-value)))

; 1.33-a
(define (sum-prime a b)
  (filtered-accumulate prime? + 0 identity a inc b))

; 1.33-b
(define (product-relatively-prime-to-n n)
  (define (relative-prime-to-n? x)
    (= (gcd x n)
       1))
  (filtered-accumulate relative-prime-to-n? * 1 identity 1 inc n))

; 1.34
;; By substitutive evaluation:
;; (f f)
;; (f 2)
;; (2 2)
;; error: 2 is not a procedure.

; 1.35
;; recursive version:
;; an argument max-steps is added for safty
(define (close-enough? a b)
  (< (abs (- a b))
     0.001))
(define (fixed-point f guess)
  (define (safety-fixed-point guess max-steps)
    (if (< max-steps 1)
	"Bad guess!"
	(let ((next-guess (f guess)))
	  (if (close-enough? next-guess guess)
	      guess
	      (safety-fixed-point next-guess
				  (- max-steps 1))))))
  (safety-fixed-point guess 1000))
;; test:
(fixed-point cos 1.0)
;; => 0.7395672022122561
;; thus the exercise:
(fixed-point (lambda (x) (+ 1
			    (/ 1 x)))
	     1.0)

; 1.36
(define (close-enough? a b)
  (< (abs (- a b))
     0.001))
(define (fixed-point-and-show-seq f guess)
  (define (safety-fixed-point-and-show-seq guess max-steps)
    (display guess)
    (newline)
    (if (< max-steps 1)
	"Bad guess!"
	(let ((next-guess (f guess)))
	  (if (close-enough? next-guess guess)
	      guess
	      (safety-fixed-point-and-show-seq next-guess
					       (- max-steps 1))))))
  (safety-fixed-point-and-show-seq guess 1000))
;; without average damping:
(fixed-point-and-show-seq (lambda (x) (/ (log 1000.0) (log x)))
			  2)
;; with average damping:
(define (average x y)
  (/ (+ x y)
     2))
(fixed-point-and-show-seq (lambda (x) (average (/ (log 1000.0) (log x))
					       x))
			  2)
;; with much less steps.

; 1.37-a
;; recursive version:
;; the trick is converting the terrible notation e.g. n_i to n_(k+1-i).
(define (cont-frac n d k)
  (define n-inverse (n (- (+ k 1)
			  i)))
  (define d-inverse (d (- (+ k 1)
			   i)))
  (if (= k 1)
      (/ (n-inverse 1)
	 (d-inverse 1))
      (/ (n-inverse k)
	 (+ (d-inverse k)
	    (cont-frac n-inverse
		       d-inverse
		       (- k 1))))))
(cont-frac (lambda (i) 1.0)
	   (lambda (i) 1.0)
	   1000)
; 1.37-b
;; iterative version:
(define (cont-frac n d k)
  (define (cont-frac-iter k result)
    (if (= k 1)
	(/ (n 1)
	   (+ (d 1)
	      result))
	(cont-frac-iter (- k 1)
			(/ (n k)
			   (+ (d k)
			      result)))))
  (cont-frac-iter k 0))
(cont-frac (lambda (i) 1.0)
	   (lambda (i) 1.0)
	   1000)

; 1.38
(define (euler-n i) 1.0)
(define (euler-d i)
  (cond ((= i 2)
	 2.0)
	((= (remainder (- i 2) 3)
	    0)
	 (+ 2.0 (euler-d (- i 3))))
	(else 1.0)))
(cont-frac euler-n euler-d 1000)

; 1.39
(define (tan-ck x k)
  (define (tan-ck-sub k)
    (let ((n (lambda (i) (- (square x))))
	  (d (lambda (i) (+ (* 2 i)
			    1))))
      (cont-frac n d k)))
  (if (= k 1)
      x
      (/ x
	 (+ 1 (tan-ck-sub (- k 1))))))
(define (square x) (* x x))
(tan-ck 1.0 100)
;; as comparison:
(tan 1.0)

; 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))
;; e.g. a = 1, b = 2, c = 3
(newtons-method (cubic 1 2 3) 1)

; 1.41
(define (double f)
  (lambda (x)
    (f (f x))))
(define (inc x) (+ x 1))
(((double (double
	   double))
  inc)
 5)
;; => 21

; 1.42
(define (compose g f)
  (lambda (x)
    (g (f x))))
(define (square x)
  (* x x))
((compose square inc) 6)
;; => 49

; 1.43
(define (repeated f times)
  (if (= 1 times)
      f
      (compose f (repeated f (- times 1)))))
((repeated square 2) 5)
;; => 625

; 1.44
(define (smooth f)
  (define dx 0.001)
  (define (average a b c)
    (/ (+ a b c)
       3))
  (lambda (x)
    (average (f (- x dx))
	     (f x)
	     (f (+ x dx)))))
(define (n-fold-smooth f n)
  ((repeated smooth n) f))

; 1.45
(define (close-enough? a b)
  (< (abs (- a b))
     0.001))
(define (fixed-point f guess)
  (define (safety-fixed-point guess max-steps)
    (if (< max-steps 1)
	"Bad guess!"
	(let ((next-guess (f guess)))
	  (if (close-enough? next-guess guess)
	      guess
	      (safety-fixed-point next-guess
				  (- max-steps 1))))))
  (safety-fixed-point guess 10000))
(define (compose g f)
  (lambda (x)
    (g (f x))))
(define (repeated f times)
  (cond ((= 0 times)
	 (lambda (x) x))
	((= 1 times)
	 f)
	(else
	 (compose f (repeated f (- times 1))))))
(define (average-damp f)
  (lambda (x) (average x (f x))))
(define (average x y)
  (/ (+ x y)
     2))


(define (power x n)
  (define (times-x y)
    (* x y))
  ((repeated times-x (- n 1)) x))
;;test:
(power 2 3)
(power 2 1)

(define (transform x n)
  (lambda (y)
    (/ x (power y n))))

(define (root-of-power n)
  (lambda (x)
    (fixed-point ((repeated average-damp
			    (+ 2
			       (round (/ (sqrt n)
				      2))))
		  (transform x (- n 1)))
		 1.0)))
((root-of-power 20) 4.2)
;; it seems that times of average-damp = (+ 2 (round (/ (sqrt n) 2))) is enough.

; 1.46
(define (iterative-improve good-enough? improve)
  (define (f x guess)
    (if (good-enough? x guess)
	guess
	(f x (improve x guess))))
  f)

; 1.46-a
(define (sqrt x guess)
  (define (good-enough? x guess)
    (define (square x)
      (* x x))
    (< (abs (- (square guess)
	       x))
       0.001))
  (define (improve x guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) x guess))
(sqrt 2 1.0)
;; => 1.4142156862745097

; 1.46-b  
(define (fixed-point f guess)
  (define (close-enough? f guess)
    (< (abs (- (f guess)
	       guess))
       0.001))
    (define (improve f guess)
      (/ (+ (f guess)
	    guess)
	 2))
    ((iterative-improve close-enough? improve) f guess))
(fixed-point cos 1.0)
;; => 0.7392146118880453

; EOF

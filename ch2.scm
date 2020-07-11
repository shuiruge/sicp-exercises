; 2.1
(define (make-rat n d)
  (let ((sign (* n d))
	(n-abs (abs n))
	(d-abs (abs d)))
    (let ((g (gcd n-abs d-abs)))
      (cond ((> sign 0)
	     (cons (/ n-abs g) (/ d-abs g)))
	    ((< sign 0)
	     (cons (* (- 1)
		      (/ n-abs g))
		   (/ d-abs g)))
	    ((= 0 sign)
	     (if (= n 0)
		 0
		 (display "nonsense")))))))

;; recall
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
;; test
(make-rat 2 (- 4))
;; => (-1 . 2)

; 2.2
(define (midpoint-segment segment)
  (define (average x y)
    (/ (+ x y) 2))
  (let ((start-p (start-point segment))
        (end-p (end-point segment)))
      (make-point (average (x-point start-p)
                           (x-point end-p))
                  (average (y-point start-p)
                           (y-point end-p)))))
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-segment start-point end-point)
  (cons start-point end-point))
(define (start-point segment)
  (car segment))
(define (end-point segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))
;; test
(define segmt
  (make-segment (make-point 0 0)
                (make-point 1 2)))
(print-point (midpoint-segment segmt))

; 2.3
(define (perimeter rectangle)
  (* 2 (+ (width rectangle) (height rectangle))))
(define (area rectangle)
  (* (width rectangle) (height rectangle)))

(define (make-rectangle width height)
  (cons width height))
(define (width rectangle)
  (car rectangle))
(define (height rectangle)
  (cdr rectangle))

; 2.4

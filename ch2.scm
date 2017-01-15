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


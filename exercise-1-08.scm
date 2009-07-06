(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (cbrt x)
  (define (cbrt-iter guess)
    (if (good-enough? guess)
        guess
        (cbrt-iter (improve guess x))))
  (define (good-enough? guess)
    (< (abs (- (cube guess) x)) 0.001))
  (cbrt-iter 1.0))

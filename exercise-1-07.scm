(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
    (average guess (/ x guess)))

(define (sqrt-new x)
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess x))))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (sqrt-iter 1.0))

; sqrt-new fails for small numbers because precision higher than 0.001 is needed
; > (square (sqrt-new 0.000001))
; 0.0009772285838805523

; sqrt-new fails for large numbers because of limited precision. Essentially
; the epsilon between the nearest two numbers is larger than 0.001, so
; good-enough? is never true. How sad.
; > (sqrt-new 5000000000000000)
; ...hangs...

(define (sqrt-new2 x)
  (define (sqrt-iter old-guess guess)
    (if (good-enough? old-guess guess)
        guess
        (sqrt-iter guess (improve guess x))))
  (define (good-enough? old-guess guess)
    (< (abs (/ (- old-guess guess)
               guess))
       0.001))
  (sqrt-iter 1.1 1.0))

; > (square (sqrt-new2 0.000001))
; 1.0000003066033492e-06
; > (sqrt-new2 5000000000000000)
; 70710678.11866389

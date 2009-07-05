(define (square x)
  (* x x))

(define (sum-of-squares a b)
  (+ (square a) (square b)))

(define (min2 a b)
  (if (< a b) a b))

(define (min3 a b c)
  (if (< a b)
      (min2 a c)
      (min2 b c)))

(define (sum-square-largest a b c)
  (cond ((= a (min3 a b c)) (sum-of-squares b c))
        ((= b (min3 a b c)) (sum-of-squares a c))
        ((= c (min3 a b c)) (sum-of-squares a b))))

(sum-square-largest 1 2 3)
; 13

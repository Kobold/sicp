(define (pascal m n)
  (if (or (= 0 n) (= m n))
      1
      (+ (pascal (- m 1) (- n 1))
         (pascal (- m 1) n))))

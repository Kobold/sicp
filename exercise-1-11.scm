(define (f n)
  (define (fp x)
    (* x (f (- n x))))
  (if (< n 3)
      n
      (+ (fp 1) (fp 2) (fp 3))))

(define (f-iter n)
  (define (f-iterp count fn-1 fn-2 fn-3)
    (if (< n count)
        fn-1
        (f-iterp (+ count 1)
                 (+ fn-1 (* 2 fn-2) (* 3 fn-3))
                 fn-1
                 fn-2)))
  (if (< n 3)
      n
      (f-iterp 3 2 1 0)))

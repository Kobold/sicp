; Show that the golden ratio is a fixed point of the transformation x -> 1 + 1/x
;
; ϕ = (1 + sqrt(5)) / 2
;
; x -> 1 + 1/x
;      1 + 1/ϕ
;      1 + 1/((1 + sqrt(5)) / 2)
;      1 + 2/(1 + sqrt(5))
;      (3 + sqrt(5))/(1 + sqrt(5)) =? (1 + sqrt(5)) / 2
;      6 + 2 sqrt(5) = 1 + sqrt(5) + sqrt(5) + 5 by cross multiplication
;                    = 6 + 2 sqrt(5).


; ...use this fact to compute ϕ by means of the fixed-point procedure.
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(display (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
; 1.6180327868852458

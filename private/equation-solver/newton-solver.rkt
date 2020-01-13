#lang racket/base


(require rackunit
         racket/math)

(provide newton-solver)

;Newtons method for solving the equation f(x) = 0
;given initial guess x0, the next iterative guess is
;x1 = x0 - f(x0)/f'(x0)
;where f' denotes the derivative with respect to x.

;; Function Number? Number? Number? -> Number
;; return number x such that function(x) = 0
;; Function is an analytic function, ie it must be possible to calculate the derivate in point x
;; Number1 is an intial guess x0; if not provided, the solver starts at x0=1
;; Number2 is the acceptable epsilon, ie if |f(x0)| < eps we are done; defaults to 1.e-6
;; Number3 is the dx used for calculating the derivative df/dx; defaults to 1.e-6
(define (newton-solver f [x0 1] [eps 1.e-6] [dx 1.e-6])
  "solve for x: f(x)=0 ie. |f(x)| < eps"
  (cond ((< (abs (f x0)) eps) x0) ; we are done; return x0
        (#t                      ; make a new guess x1
         (let* ((dy (- (f (+ x0 dx)) (f (- x0 dx)))) 
                (dy/dx (/ dy dx 2))) ; estimate dy/dx  = (f(x+dx)-f(x-dx)) / (2*dx)
           (cond ((zero? dy/dx) ; we have reached a local minimum (usually be initial starting condition x0)
                  ; add a dx and try again
                  (newton-solver f (+ x0 dx) eps dx))
                 (#t (newton-solver f (- x0 (/ (f x0) dy/dx)) eps dx)))))))

(module+ test
 (check-= (newton-solver (lambda (x) (+ 2 x))) -2. 1e-6 "failed linear function f(x)=2+x")
 (check-= (newton-solver (lambda (x) (- (sin x)(cos x)))) (/ pi 4) 1e-6 "failed trig. function f(x)=sin(x)+cos(x)")
 (check-= (newton-solver (lambda (x) (- (* x x) 1))  0  )  1  1e-6 "failed to pass local minimum")
 (check-= (newton-solver (lambda (x) (- (* x x) 1)) -0.1) -1  1e-6 "failed to find negative root"))







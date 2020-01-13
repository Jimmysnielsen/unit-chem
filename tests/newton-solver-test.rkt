#lang racket/base

(require "../private/equation-solver/newton-solver.rkt")

(newton-solver (lambda (x) (+ 2 x)))


(newton-solver (lambda (x) (log (+ x 2))) 1)

;; 
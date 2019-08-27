#lang racket/gui

(require "rune.rkt")

(define (persian a n)
  (let ([row (besiden n a)]
        [column (stackn (- n 2) a)]
        [center (make-cross a)])
    (stack-frac (/ (- n 1) n)
                (stack-frac (/ 1 (- n 1)) row
                            (beside-frac (/ (- n 1) n)
                                         (beside-frac (/ 1 (- n 1)) column center)
                                         column)) row)))

(show (persian (make-cross rcross) 5))
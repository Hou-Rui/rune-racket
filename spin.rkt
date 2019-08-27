#lang racket/gui

(require "rune.rkt")

(define (spin p a x n)
  (let ([na (+ a (* x (/ pi n)))])
    (if (= x n) (random-color p)
        (above (random-color p) (spin (rotate na p) na (+ x 1) n)))))

(show (spin ribbon 0 0 50))
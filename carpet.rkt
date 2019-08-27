#lang racket/gui

(require "rune.rkt")

(define (row m a)
  (if (= m 1) (random-color a)
      (beside-frac (/ 1 m) (random-color a) (row (- m 1) a))))

(define (random-colored-carpet n m a)
  (if (= n 1) (row m a)
      (stack-frac (/ 1 n) (row m a) (random-colored-carpet (- n 1) m a))))

(show (random-colored-carpet 10 10 heart))
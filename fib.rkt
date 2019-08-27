#lang racket

(define (fib n)
  (letrec ([iter (lambda (x m r1 r2)
                   (if (= x m) (+ r1 r2)
                       (iter (+ x 1) m r2 (+ r1 r2))))])
    (if (member n '(1 2)) 1 (iter 3 n 1 1))))

;;; Test
(display (fib 200))
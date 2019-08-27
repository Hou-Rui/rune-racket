#lang racket

(define (quick-power b e)
  (letrec ([iter (λ (b e res)
                   (cond [(= e 0) res]
                         [(= (modulo e 2) 1)
                          (iter (* b b) (/ (- e 1) 2) (* res b))]
                         [else (iter (* b b) (/ e 2) res)]))])
    (iter b e 1)))

;;; Test
(display (quick-power 3 100))
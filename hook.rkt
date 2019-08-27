#lang racket/gui

(require "rune.rkt")

(define (hook frac)
  (stack square (beside-frac (- 1 frac) blank square)))

(define (spiral thickness depth)
  (letrec ([iter (λ (t d m)
                   (let* ([mod (modulo d 4)]
                          [frac (/ t 2)]
                          [rune (hook frac)])
                     (cond
                       [(= m d) blank]
                       [(= mod 0) (stack-frac t rune (iter t (+ d 1) m))]
                       [(= mod 1) (beside-frac (- 1 t) (iter t (+ d 1) m)
                                               (quarter-turn-right rune))]
                       [(= mod 2) (stack-frac (- 1 t) (iter t (+ d 1) m)
                                              (turn-upside-down rune))]
                       [(= mod 3) (beside-frac t (quarter-turn-left rune)
                                               (iter t (+ d 1) m))])))])
    (iter thickness 0 depth)))

(show (spiral 1/5 20))
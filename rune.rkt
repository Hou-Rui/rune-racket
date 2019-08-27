#lang racket/gui

(require (prefix-in pt: pict))
(require (prefix-in ptc: pict/color))
(require racket/draw)
(require racket/random)

(provide rune)
(struct rune (pict colored?))
(define (pict->rune p) (rune p #f))

(provide repeat-pattern)
(define (repeat-pattern n fn x)
  (if (<= n 0) x (repeat-pattern (- n 1) fn (fn x))))

(provide beside-frac)
(define (beside-frac frac a b)
  (let ([scaled-a (pt:scale (rune-pict a) frac 1)]
        [scaled-b (pt:scale (rune-pict b) (- 1 frac) 1)])
    (rune (pt:ht-append scaled-a scaled-b)
          (and (rune-colored? a) (rune-colored? b)))))

(provide beside)
(define (beside a b)
  (beside-frac 0.5 a b))

(provide besiden)
(define (besiden n a)
  (if (= n 1) a (beside-frac (/ 1 n) a (besiden (- n 1) a))))

(provide stack-frac)
(define (stack-frac frac a b)
  (let ([scaled-a (pt:scale (rune-pict a) 1 frac)]
        [scaled-b (pt:scale (rune-pict b) 1 (- 1 frac))])
    (rune (pt:vc-append scaled-a scaled-b) (rune-colored? a))))

(provide stack)
(define (stack a b)
  (stack-frac 0.5 a b))

(provide stackn)
(define (stackn n a)
  (if (= n 1) a (stack-frac (/ 1 n) a (stackn (- n 1) a))))

(provide rotate)
(define (rotate x a)
  (let* ([pic (pt:rotate (rune-pict a) x)]
         [dx (/ (- 512 (pt:pict-width pic)) 2)]
         [dy (/ (- 512 (pt:pict-height pic)) 2)])
    (rune (pt:inset/clip pic dx dy) (rune-colored? a))))

(provide quarter-turn-left)
(define (quarter-turn-left a)
  (rune (pt:rotate (rune-pict a) (/ pi 2)) (rune-colored? a)))

(provide quarter-turn-right)
(define (quarter-turn-right a)
  (rune (pt:rotate (rune-pict a) (/ pi -2)) (rune-colored? a)))

(provide turn-upside-down)
(define (turn-upside-down a)
  (rune (pt:rotate (rune-pict a) pi) (rune-colored? a)))

(provide scale)
(define (scale ratio a)
  (rune (pt:scale (rune-pict a) ratio ratio) (rune-colored? a)))

(provide scale-independent)
(define (scale-independent x-ratio y-ratio a)
  (rune (pt:scale (rune-pict a) x-ratio y-ratio) (rune-colored? a)))

(provide flip-vert)
(define (flip-vert a)
  (scale-independent 1 -1 a))

(provide flip-horiz)
(define (flip-horiz a)
  (scale-independent -1 1 a))

(provide make-cross)
(define (make-cross a)
  (stack (beside (quarter-turn-right a) (turn-upside-down a))
         (beside a (quarter-turn-left a))))

(provide above)
(define (above a b)
  (rune (pt:cc-superimpose (rune-pict b) (rune-pict a))
        (and (rune-colored? a) (rune-colored? b))))

(provide circle square blank corner nova sail rcross pentagram heart ribbon)

(define circle (pict->rune (pt:disk 512)))
(define square (pict->rune (pt:filled-rectangle 512 512)))
(define blank (pict->rune (pt:blank 512)))

(define corner
  (pict->rune (pt:dc (λ (dc dx dy)
                       (send dc draw-polygon
                             (list (cons 256 0)
                                   (cons 512 0)
                                   (cons 512 256))
                             dx dy))
                     512 512)))

(define nova
  (pict->rune (pt:dc (λ (dc dx dy)
                       (send dc draw-polygon
                             (list (cons 256 0)
                                   (cons 256 128)
                                   (cons 512 256)
                                   (cons 128 256))
                             dx dy))
                     512 512)))

(define sail
  (pict->rune (pt:dc (λ (dc dx dy)
                       (send dc draw-polygon
                             (list (cons 256 0)
                                   (cons 256 512)
                                   (cons 512 512))
                             dx dy))
                     512 512)))

(define rcross
  (pict->rune (pt:dc (λ (dc dx dy)
                       (send dc draw-polygon
                             (list (cons 0 0)
                                   (cons 512 0)
                                   (cons 512 512)
                                   (cons 384 384)
                                   (cons 384 128)
                                   (cons 128 128))
                             dx dy)
                       (send dc draw-polygon
                             (list (cons 128 128)
                                   (cons 128 384)
                                   (cons 384 384))
                             dx dy))
                     512 512)))

(define pentagram
  (pict->rune (pt:dc (λ (dc dx dy)
                       (letrec ([path (new dc-path%)]
                                [len (/ 256 (+ 1 (sin (/ pi 10))))]
                                [theta (* 0.4 pi)]
                                [nx (λ (x a) (+ x (* len (cos a))))]
                                [ny (λ (y a) (+ y (* len (sin a))))]
                                [draw (λ (x y a n)
                                        (cond [(= n 10) (send path close)]
                                              [(= (modulo n 2) 1)
                                               (send path line-to (nx x a) (ny y a))
                                               (draw (nx x a) (ny y a)
                                                     (- a theta) (+ 1 n))]
                                              [else
                                               (send path line-to (nx x a) (ny y a))
                                               (draw (nx x a) (ny y a)
                                                     (+ a (* 2 theta)) (+ 1 n))]))])
                         (send path move-to 256 0)
                         (draw 256 0 theta 1)
                         (send dc draw-path path dx dy)))
                     512 512)))

(define heart
  (pict->rune (pt:dc (λ (dc dx dy)
                       (letrec ([path (new dc-path%)]
                                [x (λ (t) (+ 256 (* 256 (expt (sin t) 3))))]
                                [y (λ (t) (+ 224 (* -16 (- (- (- (* 13 (cos t))
                                                                 (* 5 (cos (* 2 t))))
                                                              (* 2 (cos (* 3 t))))
                                                           (cos (* 4 t))))))]
                                [iter (λ (t)
                                        (if (>= t (* pi 2)) (send path close)
                                            (begin (send path line-to (x t) (y t))
                                                   (iter (+ t (/ pi 50))))))])
                         (send path move-to (x 0) (y 0))
                         (iter 0)
                         (send dc draw-path path dx dy)))
                     512 512)))

(define ribbon
  (pict->rune (pt:dc (λ (dc dx dy)
                       (letrec ([path (new dc-path%)]
                                [x (λ (t) (+ 256 (* (* 8 t) (cos t))))]
                                [y (λ (t) (- 256 (* (* 8 t) (sin t))))]
                                [nx (λ (t) (+ (x t) 5))]
                                [ny (λ (t) (- (y t) 5))]
                                [iter1 (λ (t)
                                         (cond [(< t 30)
                                                (send path line-to (x t) (y t))
                                                (iter1 (+ t 0.1))]))]
                                [iter2 (λ (t)
                                         (cond [(> t 0)
                                                (send path line-to (nx t) (ny t))
                                                (iter2 (- t 0.1))]))])
                         (send path move-to (x 0) (y 0))
                         (iter1 0)
                         (iter2 30)
                         (send dc draw-path path dx dy)))
                     512 512)))

(define (color-fn color)
  (λ (a) (rune (pt:colorize (rune-pict a) color) #t)))

(provide red green blue yellow purple indigo pink orange brown black white)
(define red (color-fn "red"))
(define green (color-fn "limegreen"))
(define blue (color-fn "dodgerblue"))
(define yellow (color-fn "gold"))
(define purple (color-fn "purple"))
(define indigo (color-fn "navy"))
(define pink (color-fn "salmon"))
(define orange (color-fn "darkorange"))
(define brown (color-fn "brown"))
(define black (color-fn "black"))
(define white (color-fn "white"))

(define colors (list red green blue yellow purple indigo pink orange brown))

(provide random-color)
(define (random-color a)
  ((random-ref colors) a))

(provide show)
(define (show a)
  (pt:show-pict (if (rune-colored? a) (rune-pict a)
                    (ptc:black (rune-pict a)))))

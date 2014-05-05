#lang racket/base

(require "deepmirror.rkt")

(define-shape (first)
  ([(scale 1.5)
    (scope
     (brick))
    (scope
     (rotate 72) (hue= 72)
     (brick))
    (scope
     (rotate 144) (hue= 144)
     (brick))
    (scope
     (rotate 216) (hue= 216)
     (brick))
    (scope
     (rotate 288) (hue= 288)
     (brick))
    (scope
     (saturation 0)
     (brightness 0.78)
     (circle 25))]))

(define-shape (brick)
  ([(rotate -31)
    (x 20)
    (scale 0.9)
    (brightness 0.95)
    (circle 15)
    (brick)]
   [(rotate -29)
    (x 20)
    (scale 0.9)
    (brightness 0.925)
    (square 15)
    (brick)]))

(render first #:max-recursion 30)
#lang racket/base

(require "deepmirror.rkt")

(define-shape (q theta)
  ([ (square)
     (x 25)
     (rotate theta)
     (scale 0.95)
     (alpha 0.9)
     (q theta)
     ]
   [ (rectangle)
     (x 25)
     (rotate theta)
     (scale 0.95)
     (alpha 0.9)
     (q theta)
     ]
   ))

(define-shape (start)
  ([ (scale 2)
     
     (q 45)
     (hue 90)
     (q -45)

     (flipx)
     (hue 90)
     (q -45)
     (hue 90)
     (q 45)
     
     ]))

(render start
        #:max-recursion 50
        #:random-seed 42)


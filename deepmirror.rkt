(module cfdg racket/base
  
  (require racket/draw
           math/matrix
           syntax/parse/define
           racket/flonum
           racket/class
           racket/math)
  
  (struct render-state
    [call-depth
     transformation-matrix
     hue
     saturation
     brightness
     alpha])
  
  (define (hsb->rgb hue saturation brightness)
    (let* ([c (fl* brightness saturation)]
           [huep (fl/ hue 60.0)]
           [sp (flfloor huep)]
           [f (fl- huep sp)]
           [s (inexact->exact sp)]
           [p (fl* brightness (fl- 1.0 saturation))]
           [q (fl* brightness (fl- 1.0 (fl* saturation f)))]
           [t (fl* brightness (fl- 1.0 (fl* saturation (fl- 1.0 f))))])
      (apply values
             (map (lambda (x) (inexact->exact (flround (fl* 255.0 x))))
                  (cond
                    [(= s 0) (list brightness t p)]
                    [(= s 1) (list q brightness p)]
                    [(= s 2) (list p brightness t)]
                    [(= s 3) (list p q brightness)]
                    [(= s 4) (list t p brightness)]
                    [(= s 5) (list brightness p q)]
                    [(= s 6) (list brightness t p)])))))
  
  (define max-depth (make-parameter #f))
  (define state (make-parameter #f))
  (define dc (make-parameter #f))
  
  (define (invoke args bodies)
    (let ([body (list-ref bodies (random (length bodies)))])
      (let* ([cur-state (state)]
             [d (render-state-call-depth cur-state)])
        (when (< d (max-depth))
          (parameterize ([state
                          (struct-copy render-state cur-state [call-depth (add1 d)])])
            (apply body args))))))
  
  (define (update-dc-ctm)
    (let ([cur-state (state)]
          [dc (dc)])
      (let ([ctm (render-state-transformation-matrix cur-state)])
        (send dc set-initial-matrix
              (vector (matrix-ref ctm 0 0)
                      (matrix-ref ctm 0 1)
                      (matrix-ref ctm 1 0)
                      (matrix-ref ctm 1 1)
                      (matrix-ref ctm 0 2)
                      (matrix-ref ctm 1 2))))))

  (define (update-dc-pen-brush pen brush)
    (let ([cur-state (state)]
          [dc (dc)])
      (let*-values ([(h) (render-state-hue cur-state)]
                    [(s) (render-state-saturation cur-state)]
                    [(b) (render-state-brightness cur-state)]
                    [(r g b) (hsb->rgb h s b)]
                    [(alpha) (render-state-alpha cur-state)]
                    [(color) (make-object color% r g b alpha)])
        (when pen
          (send dc set-pen color 1.0 'solid))
        (when brush
          (send dc set-brush color 'solid))
        )))

  (define no-color (make-object color% 0 0 0 0.0))
    
  (define (square . x)
    (invoke x
            [list (lambda x
                    (update-dc-ctm)
                    (update-dc-pen-brush #f #t)
                    (send (dc) set-pen no-color 0 'transparent)
                    (let ([size (if (> (length x) 0)
                                    (exact->inexact (car x))
                                    10.0)])
                      (send (dc) draw-rectangle
                            (fl- 0.0 (fl/ size 2.0))
                            (fl- 0.0 (fl/ size 2.0))
                            size size)))]))
  
  (define (rectangle . x)
    (invoke x
            [list (lambda x
                    (update-dc-ctm)
                    (update-dc-pen-brush #t #f)
                    (send (dc) set-brush no-color 'transparent)
                    (let ([size (if (> (length x) 0)
                                    (exact->inexact (car x))
                                    10.0)])
                      (send (dc) draw-rectangle
                            (fl- 0.0 (fl/ size 2.0))
                            (fl- 0.0 (fl/ size 2.0))
                            size size)))]))
  
  (define (circle . x)
    (invoke x
            [list (lambda x
                    (update-dc-ctm)
                    (update-dc-pen-brush #f #t)
                    (send (dc) set-pen "black" 0 'transparent)
                    (let ([size (if (> (length x) 0)
                                    (exact->inexact (car x))
                                    10.0)])
                      (send (dc) draw-ellipse
                            (fl- 0.0 (fl/ size 2.0))
                            (fl- 0.0 (fl/ size 2.0))
                            size size)))]))
  
  
  
  (define-simple-macro
    (scope body ...)
    (parameterize
        ([state (struct-copy render-state (state))])
      body ...)
    )
  
  (define-simple-macro
    (define-shape (name arg ...)
      [(body ...) ...])
    (define (name . x)
      (invoke x
              [list (lambda (arg ...)
                      body ...) ...])))
  
  (define (clamp v lo hi)
    (cond
      [(fl< v lo) lo]
      [(fl< hi v) hi]
      [else v]))
  
  (define (hue x)
    (let* ([cur-state (state)]
           [hue2 (fl+ (render-state-hue cur-state) (exact->inexact x))]
           [new-hue
            (cond
              [(fl< hue2 0.0)   (fl+ hue2 360.0)]
              [(fl< 360.0 hue2) (fl- hue2 360.0)]
              [else hue2])])
      (state (struct-copy render-state cur-state
                          [hue new-hue]
                           ))))
  (define (hue= hue2)
    (let ([hue2 (->fl hue2)])
      (state (struct-copy render-state (state)
                          [hue (cond
                                 [(fl< hue2 0.0)   (fl+ hue2 360.0)]
                                 [(fl< 360.0 hue2) (fl- hue2 360.0)]
                                 [else hue2])]))))
  
  (define (saturation x)
    (let ([cur-state (state)])
      (state (struct-copy render-state cur-state
                          [saturation (clamp (fl* (render-state-saturation cur-state) (exact->inexact x)) 0.0 1.0)]
                          ))))
  (define (saturation= x)
    (state (struct-copy render-state (state) [saturation (clamp (exact->inexact x) 0.0 1.0)])))
  
  
  (define (alpha x)
    (let ([cur-state (state)])
      (state (struct-copy render-state cur-state
                          [alpha (clamp (fl* (render-state-alpha cur-state) (exact->inexact x)) 0.0 1.0)]
                          ))))
  (define (alpha= x)
    (state (struct-copy render-state (state) [alpha (clamp (->fl x) 0.0 1.0)])))
  
  (define (brightness x)
    (let ([cur-state (state)])
      (state (struct-copy render-state cur-state
                          [brightness (clamp (fl* (render-state-brightness cur-state) (exact->inexact x)) 0.0 1.0)]
                          ))))
  (define (brightness= x)
    (state (struct-copy render-state (state) [brightness (clamp (exact->inexact x) 0.0 1.0)])))
  
  (define (x v)
    (let ([cur-state (state)])
      (state (struct-copy render-state cur-state [transformation-matrix
                                                  (matrix* (render-state-transformation-matrix cur-state)
                                                           (matrix ([1 0 v] [0 1 0] [0 0 1])))]))))
  (define (y v)
    (let ([cur-state (state)])
      (state (struct-copy render-state cur-state [transformation-matrix
                                                  (matrix* (render-state-transformation-matrix cur-state)
                                                           (matrix ([1 0 0] [0 1 (- v)] [0 0 1])))]))))
  (define (xy x y)
    (let ([cur-state (state)])
      (state (struct-copy render-state cur-state [transformation-matrix
                                                  (matrix* (render-state-transformation-matrix cur-state)
                                                           (matrix ([1 0 x] [0 1 (- y)] [0 0 1])))]))))
  
  (define (sheary v)
    (let ([cur-state (state)])
      (state (struct-copy render-state cur-state
                          [transformation-matrix
                           (matrix*
                            (render-state-transformation-matrix cur-state)
                            (matrix ([1.0 (- v) 0.0] [0.0 1.0 0.0] [0.0 0.0 1.0])))]))))
  (define (shearx v)
    (let ([cur-state (state)])
      (state (struct-copy render-state cur-state
                          [transformation-matrix
                           (matrix*
                            (render-state-transformation-matrix cur-state)
                            (matrix ([1.0 0.0 0.0] [(- v) 1.0 0.0] [0.0 0.0 1.0])))]))))
  
  (define scale
    (case-lambda
      [(xy)
       (let ([cur-state (state)])
         (state (struct-copy render-state cur-state [transformation-matrix
                                                     (matrix* (render-state-transformation-matrix cur-state)
                                                              (matrix ([xy 0 0] [0 xy 0] [0 0 1])))])))]
      [(x y)
       (let ([cur-state (state)])
         (state (struct-copy render-state cur-state [transformation-matrix
                                                     (matrix* (render-state-transformation-matrix cur-state)
                                                              (matrix ([x 0 0] [0 y 0] [0 0 1])))])))]
      ))
  
  (define (flipx)
    (let ([cur-state (state)])
      (state (struct-copy render-state cur-state
                          [transformation-matrix
                           (matrix*
                            (render-state-transformation-matrix cur-state)
                            (matrix ([-1.0 0.0 0.0] [0.0 1.0 0.0] [0.0 0.0 1.0])))]))))

  (define (flipy)
    (let ([cur-state (state)])
      (state (struct-copy render-state cur-state
                          [transformation-matrix
                           (matrix*
                            (render-state-transformation-matrix cur-state)
                            (matrix ([1.0 0.0 0.0] [0.0 -1.0 0.0] [0.0 0.0 1.0])))]))))

  (define (rotate d)
    (let ([r (fl/ (fl* pi (exact->inexact d)) 180.0)]
          [cur-state (state)])
      (state (struct-copy render-state cur-state
                          [transformation-matrix
                           (matrix*
                            (render-state-transformation-matrix cur-state)
                            (matrix ([(flcos r) (flsin r) 0.0]
                                     [(fl- 0.0 (flsin r)) (flcos r) 0.0]
                                     [0.0 0.0 1.0])))]))))
  

  
  (define (render rule
                  #:args [rule-args '()]
                  #:width [width 256]
                  #:height [height 256]
                  #:max-recursion [max-recursion 20]
                  #:random-seed [seed #f])
    (when (not (eq? seed #f))
      (random-seed seed))
    (define target (make-object bitmap% width height #f #t))
    (parameterize ([dc (send target make-dc)]
                   [max-depth max-recursion]
                   [state (render-state 0
                                        (identity-matrix 3)
                                        0.0 1.0 1.0
                                        1.0)])
      (send (dc) set-smoothing 'smoothed)
      (xy (/ width 2) (- (/ height 2)))
      (apply rule rule-args)
      target))
  
  (provide scope define-shape)
  (provide square circle rectangle)
  
  (provide hue hue= saturation saturation= brightness brightness= alpha alpha=)
  (provide x y xy scale rotate shearx sheary flipx flipy)
  (provide render)
 )
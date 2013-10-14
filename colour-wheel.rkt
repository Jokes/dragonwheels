#lang racket/gui

(require racket/block)
(require "dragon-list.rkt" "dragon-funcs.rkt" "dragon-colours.rkt")

(define Nobody (Dragon "(nobody)" '(1 1 1)))
(define pairing-male Nobody)
(define pairing-female Nobody)
(define (set-male d)
  (set! pairing-male d)
  (for-each (λ (n) (send (list-ref male-drops n) set-selection 
                         (sub1 (list-ref (Dragon-numbers d) n))))
            '(0 1 2))
  (refresh-pairing))
(define (set-female d)
  (set! pairing-female d)
  (for-each (λ (n) (send (list-ref female-drops n) set-selection 
                         (sub1 (list-ref (Dragon-numbers d) n))))
            '(0 1 2))
  (refresh-pairing))
(define (get-male) pairing-male)
(define (get-female) pairing-female)

(define vis '(#t #t #t))
(define acc '(#t #t #f))
(define accnum '(0 2 2))

(define hex-lookup
  (let ([hv (list (make-hash hex-values) (make-hash tum-values) (make-hash horn-values)
                  (make-hash shade-values) (make-hash high-values))])
    (λ (k [i 0]) (first (hash-ref (list-ref hv i) k)))))

(define (num->col n [i 0]) (apply make-color (hex-lookup (colours n) i)))

(define τ (* 2 3.14159265358979))
(define rot (* τ 1/4))

(define (xy->angle x y)
  (let* ([half (if (zero? x) (/ τ 4) (atan (/ y x)))]
         [prel (+ rot (if (> x 0) (if (> y 0) half (+ τ half)) (+ (/ τ 2) half)))])
    (if (> prel τ) (- prel τ) prel)))
(define (xy->pst x y width height)
  (let* ([dist (sqrt (+ (* x x) (* y y)))]
         [smaller-bound (if (< width height) width height)] [inrad-base (/ smaller-bound 4)]
         [ring-breadth (/ inrad-base 3)])
    (cond [(or (< dist inrad-base) (> dist (+ inrad-base (* ring-breadth 3)))) ""]
          [(< dist (+ inrad-base ring-breadth)) "Tertiary: "]
          [(< dist (+ inrad-base (* ring-breadth 2))) "Secondary: "]
          [else "Primary: "])))
(define (angle->colour a)
  (string-titlecase (symbol->string (colours (inexact->exact (ceiling (* 67 (/ a τ))))))))

(define (in-range? pst clr)
  (let* ([nth (match pst [1 first] [2 second] [3 third])]
         [m-bound (nth (Dragon-numbers pairing-male))]
         [f-bound (nth (Dragon-numbers pairing-female))])
    (and (nth vis)
         (if (and (equal? pairing-male Nobody) (equal? pairing-female Nobody))
             #t
             (cond [(equal? pairing-male Nobody) (equal? clr f-bound)]
                   [(equal? pairing-female Nobody) (equal? clr m-bound)]
                   [else
                    (let* ([lowbound (if (< m-bound f-bound) m-bound f-bound)]
                           [highbound (if (< m-bound f-bound) f-bound m-bound)]
                           [split (>= (- highbound lowbound) 34)])
                      
                      (if split
                          (or (<= clr lowbound) (>= clr highbound))
                          (and (>= clr lowbound) (<= clr highbound))))])))))

(define (draw-circle dc x y r)
  (send dc draw-ellipse (- x r) (- y r) (* r 2) (* r 2)))
(define (draw-segment dc x y r n [nib 1])
  (let ([nibble (* (/ τ 67) (* nib 0.05))])
    (send dc draw-arc (- x r) (- y r) (* r 2) (* r 2)
          (+ rot (* (sub1 n) (/ τ 67)) nibble) (+ rot (- (* n (/ τ 67)) nibble)))))

(define (draw-rings dc [solid #f])
  (let*-values 
      ([(width height) (send dc get-size)] 
       [(midwidth) (/ width 2)] [(midheight) (/ height 2)]
       [(smaller-bound) (if (< width height) width height)] [(inrad-base) (/ smaller-bound 4)]
       [(ring-breadth) (/ inrad-base 3)] [(ring-inner) (/ ring-breadth 3)]
       [(ring-boundary) (/ ring-inner 8)])
    (send dc set-background (make-color 240 240 240))
    (send dc clear)
    (send dc set-smoothing 'smoothed)
    (send dc set-pen (send dc get-background) ring-boundary (if solid 'transparent 'solid))
    (for ([j (in-range 0 3)])
      (let ([inrad (+ (- inrad-base ring-boundary) (* ring-breadth (- 2 j)))])
        (for ([i (in-range 1 68)])
          (let ([in-colour (in-range? (add1 j) i)] [iflip (- 68 i)])
            (send dc set-brush 
                  (if in-colour (num->col i) (make-color 220 220 220)) 'solid)
            (draw-segment dc midwidth midheight (+ inrad ring-breadth) iflip (if solid 0 1))
            (when (and in-colour (list-ref acc j))
              (send dc set-pen "black" 0 'transparent)
              (for ([a (in-range 1 3)])
                (send dc set-brush (num->col i (+ (list-ref accnum j) a)) 'solid)
                (draw-segment dc midwidth midheight 
                              (+ inrad ring-inner (/ ring-breadth (* 3 a))) iflip 4))
              (send dc set-brush (num->col i) 'solid)
              (draw-segment dc midwidth midheight (+ inrad ring-inner) iflip 2)
              (send dc set-pen (send dc get-background) ring-boundary 
                    (if solid 'transparent 'solid)))))
        (send dc set-brush (send dc get-background) 'solid)
        (draw-circle dc midwidth midheight inrad)))))

(define (draw-ring-mask dc)
  (let*-values
      ([(width height) (send dc get-size)] 
       [(midwidth) (/ width 2)] [(midheight) (/ height 2)]
       [(smaller-bound) (if (< width height) width height)] [(inrad-base) (/ smaller-bound 4)]
       [(ring-breadth) (/ inrad-base 3)] [(ring-inner) (/ ring-breadth 3)]
       [(ring-boundary) (/ ring-inner 8)])
    (send dc set-background "white")
    (send dc clear)
    (send dc set-smoothing 'smoothed)
    (send dc set-pen (send dc get-background) ring-boundary 'solid)
    (for ([j (in-range 0 3)])
      (let ([inrad (+ (- inrad-base ring-boundary) (* ring-breadth (- 2 j)))])
        (for ([i (in-range 1 68)])
          (send dc set-brush "black" 'solid)
          (draw-segment dc midwidth midheight (+ inrad ring-breadth) (- 68 i)))
        (send dc set-brush "white" 'solid)
        (draw-circle dc midwidth midheight inrad)))))

(define (export-rings)
  (let*-values ([(width height) (send (send canvas get-dc) get-size)]
                [(smaller-bound) (if (< width height) width height)]
                [(rng) (make-bitmap smaller-bound smaller-bound)]
                [(rng-dc) (new bitmap-dc% [bitmap rng])]
                [(msk) (make-bitmap smaller-bound smaller-bound #f)]
                [(msk-dc) (new bitmap-dc% [bitmap msk])]
                [(fin) (make-bitmap smaller-bound smaller-bound)]
                [(fin-dc) (new bitmap-dc% [bitmap fin])])
    (draw-rings rng-dc #t)
    (draw-ring-mask msk-dc)
    (send fin-dc erase)
    (send fin-dc draw-bitmap rng 0 0 'solid (make-color 0 0 0) msk)
    (send fin save-file
          (string-append (name-dragon pairing-male) "-"
                         (name-dragon pairing-female) "-"
                         (number->string smaller-bound) ".png") 'png)))

(define frame (new frame% [label "Dragonwheels v1.0"] ; VERSION NUMBER HERE
                   [width 700] [height 500])) 
(define horizon (new horizontal-panel% [parent frame] [alignment '(center center)] 
                     [min-height 100] [min-width 200] [style '(auto-vscroll auto-hscroll)]))
(define mid-p (new vertical-panel% [parent horizon]))
(define top-p (new horizontal-panel% [parent mid-p] [alignment '(center center)]
                   [stretchable-height #f]))

(define my-canvas%
  (class canvas%
    (define/override (on-event event)
      (let*-values ([(width height) (send (send this get-dc) get-size)]
                    [(x) (- (send event get-x) (/ width 2))] 
                    [(y) (- (send event get-y) (/ height 2))])
        (send colour-label set-label 
              (let*-values ([(width height) (send (send this get-dc) get-size)]
                            [(pst) (xy->pst x y width height)])
                (if (equal? pst "") pst
                    (string-append pst (angle->colour (xy->angle x y))))))))
    (super-new)))

(define (reset-all)
  (set-male Nobody) (set-female Nobody)
  (set! rot (* τ 1/4))
  (set! timon #f)
  (send pause-spin set-label "Spin")
  (send tim stop)
  (set! acc '(#t #t #f))
  (set! accnum '(0 2 2))
  (for-each (λ (p n)
              (let ([b (first (send p get-children))] [b2 (second (send p get-children))])
                (unless (equal? (substring (send b get-label) 0 4) "Hide")
                  (send b command b))
                (send b2 set-label (if (list-ref acc n) "-Accent" "+Accent"))))
            (send toggle-p get-children) '(0 1 2)))

(define (colour-drops tall-p getpr setpr)
  (map 
   (λ (n s) 
     (new choice% [label s] [parent tall-p]
          [callback (λ (c e) 
                      (let ([drns (Dragon-numbers (getpr))])
                        (setpr (Dragon "(Unknown)"
                                       (append (take drns n)
                                               (cons (add1 (send c get-selection))
                                                     (drop drns (add1 n))))))))]
          [choices (build-list 67 (λ (n) (symbol->string (colours (add1 n)))))]))
   '(0 1 2) '("Primary: " "Secondary: " "Tertiary: ")))
(define male-p (new vertical-panel% [parent (new horizontal-panel% [parent top-p] 
                                                 [alignment '(left center)])] 
                    [stretchable-width #f] [alignment '(right center)]))
(define male-dragdrop 
  (let ([m-dropdrags (cons Nobody male-dragons)])
    (new choice% [label ""]
         [parent (new horizontal-panel% [parent male-p] 
                      [stretchable-height #f] [alignment '(center center)])]
         [callback (λ (c e) (set-male (list-ref m-dropdrags (send c get-selection))))]
         [choices (map name-dragon m-dropdrags)])))
(define male-drops (colour-drops male-p get-male set-male))

(define reset-b (new button% [parent top-p] [label "Reset"] [stretchable-height #t]
                     [callback (λ (b e) (reset-all) (refresh-pairing))]))

(define toggle-p (new vertical-panel% [parent top-p] [stretchable-width #f] 
                      [alignment '(center center)]))
(for-each 
 (λ (n s) 
   (define i-p (new horizontal-panel% [parent toggle-p] [stretchable-height #f]
                    [alignment '(right center)]))
   (new message% [parent i-p] [label s])
   (new button% [parent i-p] [label "Hide"]
        [callback (λ (b e) (set! vis (append (take vis n) (cons (not (list-ref vis n)) 
                                                                (drop vis (add1 n)))))
                    (if (list-ref vis n)
                        (send b set-label "Hide")
                        (send b set-label "Show"))
                    (refresh-pairing))])
   (new button% [parent i-p] [stretchable-width #f]
        [label (string-append (if (list-ref acc n) "-" "+") "Accent")]
        [callback (λ (b e) (set! acc (append (take acc n) (cons (not (list-ref acc n))
                                                                (drop acc (add1 n)))))
                    (if (list-ref acc n) 
                        (send b set-label "-Accent") 
                        (send b set-label "+Accent"))
                    (refresh-pairing))]))
 '(0 1 2) '("Primary:" "Secondary:" "Tertiary:"))

(define pause-spin (new button% [parent top-p] [label "Spin"] [stretchable-height #t]
                        [callback (λ (b e) (if timon 
                                               (block (send tim stop) 
                                                      (send b set-label "Spin")) 
                                               (block (send tim start 500) 
                                                      (send b set-label "Pause")))
                                    (set! timon (not timon)))]))
(define female-p (new vertical-panel% [parent (new horizontal-panel% [parent top-p] 
                                                   [alignment '(right center)])] 
                      [stretchable-width #f] [alignment '(right center)]))
(define female-dragdrop 
  (let ([f-dropdrags (cons Nobody female-dragons)])
    (new choice% [label ""]
         [parent (new horizontal-panel% [parent female-p] 
                      [stretchable-height #f] [alignment '(center center)])]
         [callback (λ (c e) (set-female (list-ref f-dropdrags (send c get-selection))))]
         [choices (map name-dragon f-dropdrags)])))
(define female-drops
  (colour-drops female-p get-female set-female))

(define export-b (new button% [parent toggle-p] [label "Export"] [stretchable-width #t]
                      [callback (λ (b e) (export-rings))]))

(define pairing-label (new message% [parent mid-p] [label ""] [auto-resize #t]))
(define (refresh-pairing)
  (send pairing-label set-label (string-append (name-dragon pairing-male) "/" 
                                               (name-dragon pairing-female)))
  (send canvas refresh))
(define canvas (new my-canvas% [parent mid-p] [min-width 100] [min-height 100]
                    [paint-callback 
                     (λ (canvas dc)
                       (draw-rings dc))]))
(define colour-label (new message% [parent mid-p] [label ""] [auto-resize #t]))

#;(for-each (λ (d) (new button% [label (name-dragon d)] [parent left-p]
                        [callback (λ (b e) (set-male d))])) 
            (cons Nobody male-dragons))
#;(for-each (λ (d) (new button% [label (name-dragon d)] [parent right-p]
                        [callback (λ (b e) (set-female d))])) 
            (cons Nobody female-dragons))

(send frame show #t)
(refresh-pairing)

(define tim
  (new timer% 
       [notify-callback 
        (λ () (set! rot (let ([prel (- rot (* τ 1/67))]) (if (< prel 0) (+ prel τ) prel)))
          (refresh-pairing))] 
       [interval 500]))
(define timon #f)
(send tim stop)

#lang racket/gui

(require racket/block)
(require "dragon-list.rkt")

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

(define hex-values
  '((aqua (114 196 196)) 
    (avocado (86 124 52)) (azure (5 35 67)) (banana (253 255 114)) (beige (202 187 162)) 
    (black (51 51 51)) (blood (69 15 15)) (blue (50 75 169)) (brown (117 81 54))
    (caribbean (0 134 206)) (carmine (177 58 58)) (charcoal (84 84 84)) (chocolate (72 38 14))
    (coal (75 73 70)) (coral (204 111 111)) (crimson (133 0 18)) (denim (47 69 87))
    (emerald (32 96 63)) (fire (239 92 35)) (forest (66 80 53)) (gold (232 175 73))
    (goldenrod (148 134 71)) (green (98 156 63)) (grey (126 126 126)) (ice (218 224 243))
    (ivory (255 210 151)) (jade (97 171 137)) (jungle (30 54 26)) (lavender (204 164 224))
    (leaf (165 227 45)) (lemon (255 230 59)) (magenta (233 52 170)) (maize (255 253 234))
    (maroon (101 33 39)) (midnight (41 43 56)) (mulberry (110 35 93)) (navy (33 43 95))
    (obsidian (0 0 0)) (orange (213 96 43)) (pink (231 127 191)) (platinum (200 190 206))
    (purple (162 97 207)) (red (193 39 45)) (rose (255 214 246)) (royal (77 44 137))
    (rust (139 50 32)) (sand (178 119 73)) (seafoam (170 241 177)) (shadow (58 46 68))
    (silver (187 186 191)) (sky (174 200 255)) (slate (86 77 72)) (soil (90 69 52))
    (splash (99 148 221)) (spring (169 160 50)) (steel (85 105 121)) (stone (150 145 130))
    (stonewash (121 150 194)) (storm (117 122 219)) (sunshine (250 145 43)) (swamp (104 127 103))
    (tangerine (255 115 96)) (teal (43 118 143)) (thistle (143 124 139)) (tomato (186 49 28))
    (violet (100 63 156)) (white (255 255 255))))

(define hex-lookup
  (let ([hv (make-hash hex-values)])
    (λ (k) (first (hash-ref hv k)))))

(define (num->col n) (apply make-color (hex-lookup (colours n))))

(define τ (* 2 3.14159265358979))
(define rot (* τ 1/4))

(define (xy->angle x y)
  (let* ([half (if (zero? x) (/ τ 4) (atan (/ y x)))]
         [prel (+ rot (if (> x 0) (if (> y 0) half (+ τ half)) (+ (/ τ 2) half)))])
    (if (> prel τ) (- prel τ) prel)))
(define (xy->pst x y)
  (let ([dist (sqrt (+ (* x x) (* y y)))])
    (cond [(or (< dist 200) (> dist 350)) ""]
          [(< dist 250) "Primary: "]
          [(< dist 300) "Secondary: "]
          [else "Tertiary: "])))
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
(define (draw-segment dc x y r n)
  (let ([nibble (* (/ τ 67) 0.05)])
    (send dc draw-arc (- x r) (- y r) (* r 2) (* r 2)
          (+ rot (* (sub1 n) (/ τ 67)) nibble) (+ rot (- (* n (/ τ 67)) nibble)))))

(define (draw-rings dc)
  (let-values ([(width height) (send dc get-size)])
    (send dc set-background (make-color 240 240 240))
    (send dc clear)
    (send dc set-smoothing 'smoothed)
    (send dc set-pen (send dc get-background) 2 'solid)
    (for ([j (in-range 0 3)])
      (for ([i (in-range 1 68)])
        (send dc set-brush 
              (if (in-range? (- 3 j) i) (num->col i) (make-color 220 220 220)) 'solid)
        (draw-segment dc (/ width 2) (/ height 2) (+ 200 (* 50 (- 3 j))) (- 68 i)))
      (send dc set-brush (send dc get-background) 'solid)
      (draw-circle dc (/ width 2) (/ height 2) (+ 200 (* 50 (- 2 j)))))))

(define frame (new frame% [label "Dragonwheels v0.1"])) ; VERSION NUMBER HERE
(define horizon (new horizontal-panel% [parent frame] [alignment '(center center)]))
(define left-p (new vertical-panel% [parent horizon]))
(define mid-p (new vertical-panel% [parent horizon]))
(define top-p (new horizontal-panel% [parent mid-p] [alignment '(center center)]))
(define right-p (new vertical-panel% [parent horizon]))

(define my-canvas%
  (class canvas%
    (define/override (on-event event)
      (let*-values ([(width height) (send (send this get-dc) get-size)]
                    [(x) (- (send event get-x) (/ width 2))] 
                    [(y) (- (send event get-y) (/ height 2))])
        (send colour-label set-label 
              (let ([pst (xy->pst x y)])
                (if (equal? pst "") pst
                    (string-append pst (angle->colour (xy->angle x y))))))))
    (super-new)))

(define (reset-all)
  (set-male Nobody) (set-female Nobody)
  (set! rot (* τ 1/4))
  (set! timon #f)
  (send pause-spin set-label "Spin")
  (send tim stop)
  (for-each (λ (b) (unless (equal? (substring (send b get-label) 0 4) "Hide")
                     (send b command b))) (send toggle-p get-children)))

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
(define male-label (new message% [label ""] [auto-resize #t]
                        [parent (new horizontal-panel% [parent male-p] [stretchable-height #f]
                                     [alignment '(center center)])]))
(define male-drops (colour-drops male-p get-male set-male))

(define reset-b (new button% [parent top-p] [label "Reset"] [stretchable-height #t]
                     [callback (λ (b e) (reset-all) (refresh-pairing))]))

(define toggle-p (new vertical-panel% [parent top-p] [stretchable-width #f] 
                      [alignment '(center center)]))
(for-each 
 (λ (n s) (new button% [parent toggle-p] [label (string-append "Hide " s)]
               [callback (λ (b e) (set! vis (append (take vis n) (cons (not (list-ref vis n)) 
                                                                       (drop vis (add1 n)))))
                           (if (list-ref vis n)
                               (send b set-label (string-append "Hide " s))
                               (send b set-label (string-append "Show " s)))
                           (refresh-pairing))]))
 '(0 1 2) '("Primary" "Secondary" "Tertiary"))

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
(define female-label (new message% [label ""] [auto-resize #t]
                          [parent (new horizontal-panel% [parent female-p] [stretchable-height #f]
                                       [alignment '(center center)])]))
(define female-drops
  (colour-drops female-p get-female set-female))

(define pairing-label (new message% [parent mid-p] [label ""] [auto-resize #t]))
(define (refresh-pairing)
  (send pairing-label set-label (string-append (name-dragon pairing-male) "/" 
                                               (name-dragon pairing-female)))
  (send male-label set-label (name-dragon pairing-male))
  (send female-label set-label (name-dragon pairing-female))
  (send canvas refresh))
(define canvas (new my-canvas% [parent mid-p] [min-width 700] [min-height 700]
                    [paint-callback 
                     (λ (canvas dc)
                       (draw-rings dc))]))
(define colour-label (new message% [parent mid-p] [label ""] [auto-resize #t]))

(for-each (λ (d) (new button% [label (name-dragon d)] [parent left-p]
                      [callback (λ (b e) (set-male d))])) 
          (cons Nobody male-dragons))
(for-each (λ (d) (new button% [label (name-dragon d)] [parent right-p]
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

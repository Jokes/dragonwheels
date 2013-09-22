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
(define acc '(#t #f #f))

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

(define tum-values
  '((chocolate (131 84 56)) 
    (maize (230 219 163)) (shadow (73 57 57)) (sky (255 245 243))
    (jungle (62 96 48)) (gold (244 188 127)) (rust (200 77 49)) (white (242 238 234))
    (mulberry (140 76 203)) (stonewash (72 94 145)) (forest (39 50 29)) (sunshine (164 88 37))
    (tomato (230 70 49)) (ice (192 195 224)) (thistle (97 75 88)) (steel (151 155 156))
    (swamp (162 165 116)) (orange (238 136 70)) (crimson (81 15 21)) (platinum (249 249 249))
    (lavender (136 96 181)) (denim (101 139 160)) (avocado (158 195 93)) (fire (165 64 53))
    (blood (41 15 15)) (silver (94 94 105)) (purple (172 132 252)) (azure (50 139 201))
    (green (103 180 124)) (tangerine (192 56 34)) (maroon (147 65 47)) (grey (167 167 167)) 
    (violet (190 138 119)) (caribbean (0 70 109)) (leaf (100 146 12)) (sand (217 170 118))
    (red (255 97 61)) (charcoal (143 143 143)) (royal (125 85 197)) (teal (69 199 198))
    (spring (238 164 70)) (beige (114 108 86)) (carmine (111 40 60)) (coal (31 30 28))
    (storm (66 68 99)) (aqua (132 231 193)) (goldenrod (186 141 38)) (stone (224 221 192))
    (coral (246 196 165)) (black (201 192 190)) (navy (71 71 116)) (seafoam (221 250 205))
    (lemon (159 112 57)) (slate (138 127 121)) (magenta (255 129 173)) (obsidian (47 54 56))
    (blue (70 112 238)) (jade (51 124 90)) (banana (255 245 189)) (soil (136 118 93))
    (pink (187 116 181)) (midnight (27 29 32)) (splash (41 59 127)) (emerald (111 123 63))
    (ivory (255 219 177)) (brown (84 51 42)) (rose (247 159 201))))

(define horn-values
  '((chocolate (53 24 3))
    (maize (177 173 60)) (shadow (67 52 54)) (sky (137 216 219)) (jungle (88 140 53))
    (gold (241 198 158)) (rust (219 130 88)) (white (236 222 210)) (mulberry (84 56 83))
    (stonewash (205 221 255)) (forest (135 155 117)) (sunshine (91 55 42)) (tomato (254 79 30))
    (ice (174 184 214)) (thistle (216 197 230)) (steel (192 203 217)) (swamp (151 136 101))
    (orange (218 171 127)) (crimson (36 15 16)) (platinum (67 67 67)) (lavender (99 68 119))
    (denim (56 52 48)) (avocado (176 255 37)) (fire (91 51 47)) (blood (22 18 16))
    (silver (233 233 233)) (purple (106 52 173)) (azure (48 154 240)) (green (71 68 93))
    (tangerine (186 70 55)) (maroon (62 40 43)) (grey (190 190 190)) (violet (217 169 126))
    (caribbean (0 40 63)) (leaf (32 82 44)) (sand (217 136 98)) (red (218 171 127))
    (charcoal (36 36 36)) (royal (179 137 255)) (teal (50 61 73)) (spring (217 169 126))
    (beige (66 56 18)) (carmine (44 27 32)) (coal (135 133 130)) (storm (49 43 68))
    (aqua (198 238 225)) (goldenrod (170 129 0)) (stone (205 197 172)) (coral (245 171 166))
    (black (193 193 193)) (navy (46 51 55)) (seafoam (103 193 141)) (lemon (83 68 0))
    (slate (195 187 178)) (magenta (90 2 13)) (obsidian (47 54 56)) (blue (126 130 217))
    (jade (67 67 67)) (banana (255 238 177)) (soil (48 26 8)) (pink (211 175 196)) 
    (midnight (69 70 78)) (splash (95 226 222)) (emerald (193 217 91)) (ivory (255 250 243))
    (brown (55 44 35)) (rose (255 255 255))))

(define hex-lookup
  (let ([hv (list (make-hash hex-values) (make-hash tum-values) (make-hash horn-values))])
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
                (send dc set-brush (num->col i a) 'solid)
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

(define frame (new frame% [label "Dragonwheels v0.5"] [width 820] [height 600])) ; VERSION # HERE
(define horizon (new horizontal-panel% [parent frame] [alignment '(center center)] 
                     [min-height 100] [min-width 200] [style '(auto-vscroll auto-hscroll)]))
(define left-p (new vertical-panel% [parent horizon] [stretchable-width #f]))
(define mid-p (new vertical-panel% [parent horizon]))
(define top-p (new horizontal-panel% [parent mid-p] [alignment '(center center)]
                   [stretchable-height #f]))
(define right-p (new vertical-panel% [parent horizon] [stretchable-width #f]))

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
  (set! acc '(#t #f #f))
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
(define male-label (new message% [label ""] [auto-resize #t]
                        [parent (new horizontal-panel% [parent male-p] [stretchable-height #f]
                                     [alignment '(center center)])]))
(define male-drops (colour-drops male-p get-male set-male))

(define reset-b (new button% [parent top-p] [label "Reset"] [stretchable-height #t]
                     [callback (λ (b e) (reset-all) (refresh-pairing))]))

(define toggle-p (new vertical-panel% [parent top-p] [stretchable-width #f] 
                      [alignment '(center center)]))
(for-each 
 (λ (n s) 
   (define i-p (new horizontal-panel% [parent toggle-p] [stretchable-height #f]
                    [alignment '(center center)]))
   (new button% [parent i-p] [label (string-append "Hide " s)]
        [callback (λ (b e) (set! vis (append (take vis n) (cons (not (list-ref vis n)) 
                                                                (drop vis (add1 n)))))
                    (if (list-ref vis n)
                        (send b set-label (string-append "Hide " s))
                        (send b set-label (string-append "Show " s)))
                    (refresh-pairing))])
   (new button% [parent i-p] [stretchable-width #f]
        [label (string-append (if (list-ref acc n) "-" "+") "Accent")]
        [callback (λ (b e) (set! acc (append (take acc n) (cons (not (list-ref acc n))
                                                                (drop acc (add1 n)))))
                    (if (list-ref acc n) 
                        (send b set-label "-Accent") 
                        (send b set-label "+Accent"))
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

(define export-b (new button% [parent mid-p] [label "Export"] [callback (λ (b e) (export-rings))]))

(define pairing-label (new message% [parent mid-p] [label ""] [auto-resize #t]))
(define (refresh-pairing)
  (send pairing-label set-label (string-append (name-dragon pairing-male) "/" 
                                               (name-dragon pairing-female)))
  (send male-label set-label (name-dragon pairing-male))
  (send female-label set-label (name-dragon pairing-female))
  (send canvas refresh))
(define canvas (new my-canvas% [parent mid-p] [min-width 100] [min-height 100]
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

#lang racket

(provide Dragon Dragon-numbers name-dragon
         male-dragons female-dragons
         colours)

(struct Dragon (name numbers) #:transparent)

(define (name-colours dragon) (map colours (Dragon-numbers dragon)))
(define (name-dragon dragon)  
  (if (symbol? (Dragon-name dragon))
      (symbol->string (Dragon-name dragon))
      (Dragon-name dragon)))

(define colours
  (let ([ch 
         (make-hash 
          '((1 maize) (maize 1) (2 white) (white 2) (3 ice) (ice 3) (4 platinum) (platinum 4)
                      (5 silver) (silver 5) (6 grey) (grey 6) (7 charcoal) (charcoal 7)
                      (8 coal) (coal 8) (9 black) (black 9) (10 obsidian) (obsidian 10)
                      (11 midnight) (midnight 11) (12 shadow) (shadow 12)
                      (13 mulberry) (mulberry 13) (14 thistle) (thistle 14)
                      (15 lavender) (lavender 15) (16 purple) (purple 16) (17 violet) (violet 17)
                      (18 royal) (royal 18) (19 storm) (storm 19) (20 navy) (navy 20)
                      (21 blue) (blue 21) (22 splash) (splash 22) (23 sky) (sky 23)
                      (24 stonewash) (stonewash 24) (25 steel) (steel 25) (26 denim) (denim 26)
                      (27 azure) (azure 27) (28 caribbean) (caribbean 28)
                      (29 teal) (teal 29) (30 aqua) (aqua 30) (31 seafoam) (seafoam 31)
                      (32 jade) (jade 32) (33 emerald) (emerald 33) (34 jungle) (jungle 34)
                      (35 forest) (forest 35) (36 swamp) (swamp 36) (37 avocado) (avocado 37)
                      (38 green) (green 38) (39 leaf) (leaf 39) (40 spring) (spring 40)
                      (41 goldenrod) (goldenrod 41) (42 lemon) (lemon 42) (43 banana) (banana 43)
                      (44 ivory) (ivory 44) (45 gold) (gold 45) (46 sunshine) (sunshine 46)
                      (47 orange) (orange 47) (48 fire) (fire 48) (49 tangerine) (tangerine 49)
                      (50 sand) (sand 50) (51 beige) (beige 51) (52 stone) (stone 52)
                      (53 slate) (slate 53) (54 soil) (soil 54) (55 brown) (brown 55)
                      (56 chocolate) (chocolate 56) (57 rust) (rust 57) (58 tomato) (tomato 58)
                      (59 crimson) (crimson 59) (60 blood) (blood 60) (61 maroon) (maroon 61)
                      (62 red) (red 62) (63 carmine) (carmine 63) (64 coral) (coral 64)
                      (65 magenta) (magenta 65) (66 pink) (pink 66) (67 rose) (rose 67)))])
    (λ (k) (first (hash-ref ch k)))))

(define-syntax define-dragon-list
  (syntax-rules ()
    [(define-dragon-list «dragon-list»
       («dragon» «colours») ...)
     (begin
       (define «dragon» (Dragon '«dragon» (map colours «colours»))) ...
       (define «dragon-list» (list «dragon» ...)))]))

(define-dragon-list fae-boys)

(define-dragon-list guardian-boys)

(define-dragon-list imperial-boys)

(define-dragon-list mirror-boys)

(define-dragon-list pearlcatcher-boys)

(define-dragon-list ridgeback-boys)

(define-dragon-list skydancer-boys)

(define-dragon-list snapper-boys)

(define-dragon-list spiral-boys)

(define-dragon-list tundra-boys)

(define-dragon-list wildclaw-boys)

(define-dragon-list fae-girls)

(define-dragon-list guardian-girls)

(define-dragon-list imperial-girls)

(define-dragon-list mirror-girls)

(define-dragon-list pearlcatcher-girls)

(define-dragon-list ridgeback-girls)

(define-dragon-list skydancer-girls)

(define-dragon-list snapper-girls)

(define-dragon-list spiral-girls)

(define-dragon-list tundra-girls)

(define-dragon-list wildclaw-girls)

(define-dragon-list extra-boys
  (Example '(maize black ice)))

(define-dragon-list extra-girls
  (Examplette '(stone azure fire)))

(define male-dragons
  (append fae-boys guardian-boys imperial-boys mirror-boys pearlcatcher-boys ridgeback-boys 
          skydancer-boys snapper-boys spiral-boys tundra-boys wildclaw-boys
          extra-boys))

(define female-dragons
  (append fae-girls guardian-girls imperial-girls mirror-girls pearlcatcher-girls ridgeback-girls 
          skydancer-girls snapper-girls spiral-girls tundra-girls wildclaw-girls
          extra-girls))

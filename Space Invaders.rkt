;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Space Invaders|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; ====================
;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define CENTER-X (/ WIDTH 2))
(define CENTER-Y (/ HEIGHT 2))

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define INVADE-RATE 20)

(define MAX-NUM-INVADERS 20)

(define TRANSPARENT-BACKGROUND (rectangle WIDTH HEIGHT "outline" "black"))
(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define MISSILE (ellipse 5 15 "solid" "red"))

(define INVADER-WIDTH (image-width INVADER))
(define INVADER-HEIGHT/2 (/ (image-height INVADER) 2))
(define INVADER-WIDTH/2 (/ (image-width INVADER) 2))

(define TANK-HEIGHT (image-height TANK))
(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-WIDTH/2 (/ (image-width TANK) 2))

(define MISSILE-HEIGHT/2 (/ (image-height MISSILE) 2))
(define MISSILE-WIDTH/2 (/ (image-width MISSILE) 2))

(define X-HIT-RANGE (+ INVADER-WIDTH/2 MISSILE-WIDTH/2))
(define Y-HIT-RANGE (+ INVADER-HEIGHT/2 MISSILE-HEIGHT/2))


;; ====================
;; Data Definitions:

(define-struct game (stats invaders missiles tank))
;; Game is (make-game Stats
;;                    ListOfInvaders
;;                    ListOfMissiles
;;                    Tank)
;; interp. the current state of a space invaders game
;;         with the current counters, invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game g)
  (... (fn-for-stats (game-stats g))
       (fn-for-loi (game-invaders g))
       (fn-for-lom (game-missiles g))
       (fn-for-tank (game-tank g))))


(define-struct stats (ticks invaders-total invaders-left game-over))
;; Stats is (make-stats Natural[0, INVADE-RATE - 1]
;;                      Natural[0, MAX-NUM-INVADERS]
;;                      Natural[0, MAX-NUM-INVADERS]
;;                      Boolean)
;; interp. the number of ticks
;;         the total number invaders
;;         the number of invaders left
;;         whether the game is over

(define S0 (make-stats 0 0 0 false))                                 ;game starts
(define S1 (make-stats 5 0 0 false))                                 ;early game
(define S2 (make-stats (- INVADE-RATE 1) 1 1 false))                 ;1 invader
(define S3 (make-stats (- INVADE-RATE 1) 2 2 false))                 ;2 invaders
(define S4 (make-stats (- INVADE-RATE 1) 2 2 false))                 ;2 invaders
(define S5 (make-stats (- INVADE-RATE 1) 2 2 true))                  ;invader pasts bottom of screen, game over
(define S6 (make-stats (- INVADE-RATE 1) MAX-NUM-INVADERS 1 false))  ;max number of invaders reached, 1 left
(define S7 (make-stats (- INVADE-RATE 1) MAX-NUM-INVADERS 0 true))   ;max number of invaders reached, 0 left, game over

#;
(define (fn-for-stat s)
  (... (stats-ticks)
       (stats-invaders-total s)
       (stats-invaders-left s)
       (stats-game-over s)))


(define-struct invader (x y dir))
;; Invader is (make-invader Number Number Integer[-1, 1])
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader moves INVADER-SPEED pixels per tick left if dir -1, right if dir 1

(define I1 (make-invader 150 200 1))                                    ;moving right
(define I2 (make-invader 200 300 -1))                                   ;moving left
(define I3 (make-invader 50 (- HEIGHT INVADER-HEIGHT/2) 1))             ;reaches bottom of screen, moving right
(define I4 (make-invader 100 (+ (- HEIGHT INVADER-HEIGHT/2) 1) -1))     ;pasts bottom of scren, moving left
(define I5 (make-invader (+ INVADER-WIDTH/2 1) 100 -1))                 ;moving left, about to reach left wall
(define I6 (make-invader INVADER-WIDTH/2 (+ 100 1) 1))                  ;at left wall, bouncing back
(define I7 (make-invader (- WIDTH (+ INVADER-WIDTH/2 1)) 250 1))        ;moving right, about to reach right wall
(define I8 (make-invader (- WIDTH INVADER-WIDTH/2) (+ 250 1) -1))       ;at right wall, bouncing back

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile (invader-x I1) 300))                                   ;not hit I1
(define M2 (make-missile (invader-x I2) (- (+ (invader-y I2) Y-HIT-RANGE) 1)))  ;hit I2
(define M3 (make-missile (invader-x I3) 100))                                   ;not hit I3
(define M4 (make-missile (invader-x I4) (- MISSILE-HEIGHT/2)))                  ;not hit I4, off the screen

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank CENTER-X 1))                       ;center moving right
(define T1 (make-tank 50 1))                             ;moving right
(define T2 (make-tank 50 -1))                            ;moving left
(define T3 (make-tank (+ TANK-WIDTH/2 1) -1))            ;moving left, about to reach left wall
(define T4 (make-tank TANK-WIDTH/2 1))                   ;at left wall, bouncing back
(define T5 (make-tank (- (- WIDTH TANK-WIDTH/2) 1) 1))   ;moving right, about to reach right wall
(define T6 (make-tank (- WIDTH TANK-WIDTH/2) -1))        ;at right wall, bouncing back

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dx t)))

  
(define G0 (make-game S0 empty empty T0))                ;game starts
(define G1 (make-game S1 empty empty T1))                ;early game
(define G2 (make-game S2 (list I1) (list M1) T1))        ;1 invader
(define G3 (make-game S3 (list I1 I2) (list M1 M2) T2))  ;2 invaders, 1 hit
(define G4 (make-game S4 (list I1 I3) (list M1 M3) T2))  ;invader reaches bottom of screen, game about to end
(define G5 (make-game S5 (list I1 I4) (list M1 M4) T2))  ;invader pasts bottom of screen, game over
(define G6 (make-game S6 (list I2) (list M2) T2))        ;max number of invaders reached, 1 left and hit, game about to end
(define G7 (make-game S7 empty empty T2))                ;max number of invaders reached, 0 left, game over


;; ====================
;; Functions:

;; Game -> Game
;; start the game with (main G0)
;; <no tests for main function>

(define (main g)
  (big-bang g
            (to-draw render-game)  ; Game -> Image
            (on-tick next-game)    ; Game -> Game
            (on-key handle-key)))  ; Game KeyEvent -> Game


;; --------------------
;; Game -> Image
;; render game with invaders and missles and tank
(check-expect (render-game G0) (place-images (list (render-invaders (game-invaders G0))
                                                   (render-missiles (game-missiles G0)))
                                             (list (make-posn CENTER-X CENTER-Y)
                                                   (make-posn CENTER-X CENTER-Y))
                                             (render-tank (game-tank G0))))
(check-expect (render-game G2) (place-images (list (render-invaders (game-invaders G2))
                                                   (render-missiles (game-missiles G2)))
                                             (list (make-posn CENTER-X CENTER-Y)
                                                   (make-posn CENTER-X CENTER-Y))
                                             (render-tank (game-tank G2))))
(check-expect (render-game G3) (place-images (list (render-invaders (game-invaders G3))
                                                   (render-missiles (game-missiles G3)))
                                             (list (make-posn CENTER-X CENTER-Y)
                                                   (make-posn CENTER-X CENTER-Y))
                                             (render-tank (game-tank G3))))

;(define (render-game g) BACKGROUND) ;stub

(define (render-game g)
  (place-images (list (render-invaders (game-invaders g))
                      (render-missiles (game-missiles g)))
                (list (make-posn CENTER-X CENTER-Y)
                      (make-posn CENTER-X CENTER-Y))
                (render-tank (game-tank g))))


;; ....................
;; ListOfInvaders -> Image
;; render invaders
(check-expect (render-invaders empty) TRANSPARENT-BACKGROUND)
(check-expect (render-invaders (list I1)) (place-images (list INVADER)
                                                        (list (make-posn (invader-x I1) (invader-y I1)))
                                                        TRANSPARENT-BACKGROUND))
(check-expect (render-invaders (list I1 I2)) (place-images (list INVADER INVADER)
                                                           (list (make-posn (invader-x I1) (invader-y I1))
                                                                 (make-posn (invader-x I2) (invader-y I2)))
                                                           TRANSPARENT-BACKGROUND))

;(define (render-invaders loi) BACKGROUND) ;stub

(define (render-invaders loi)
  (place-images (invader-images loi)
                (invader-positions loi)
                TRANSPARENT-BACKGROUND))


;; ListOfInvaders -> ListOfImages
;; produce list of immages for invaders
(check-expect (invader-images empty) empty)
(check-expect (invader-images (list I1)) (list INVADER))
(check-expect (invader-images (list I1 I2)) (list INVADER INVADER))

;(define (invader-images loi) empty) ;stub

(define (invader-images loi)
  (cond [(empty? loi) empty]
        [else
         (cons INVADER (invader-images (rest loi)))]))


;; ListOfInvader -> ListOfPositions
;; produce list of positions for invaders
(check-expect (invader-positions empty) empty)
(check-expect (invader-positions (list I1)) (list (make-posn (invader-x I1) (invader-y I1))))
(check-expect (invader-positions (list I1 I2)) (list (make-posn (invader-x I1) (invader-y I1))
                                                     (make-posn (invader-x I2) (invader-y I2))))

;(define (invader-positions loi) empty) ;stub

(define (invader-positions loi)
  (cond [(empty? loi) empty]
        [else
         (cons (make-posn (invader-x (first loi)) (invader-y (first loi)))
               (invader-positions (rest loi)))]))


;; ....................
;; ListOfMissles -> Image
;; render missles
(check-expect (render-missiles empty) TRANSPARENT-BACKGROUND)
(check-expect (render-missiles (list M1)) (place-images (list MISSILE)
                                                        (list (make-posn (missile-x M1) (missile-y M1)))
                                                        TRANSPARENT-BACKGROUND))
(check-expect (render-missiles (list M1 M2)) (place-images (list MISSILE MISSILE)
                                                           (list (make-posn (missile-x M1) (missile-y M1))
                                                                 (make-posn (missile-x M2) (missile-y M2)))
                                                           TRANSPARENT-BACKGROUND))

;(define (render-missiles lom) BACKGROUND) ;stub

(define (render-missiles lom)
  (place-images (missile-images lom)
                (missile-positions lom)
                TRANSPARENT-BACKGROUND))


;; ListOfMissiles -> ListOfImages
;; produce list of images for missiles
(check-expect (missile-images empty) empty)
(check-expect (missile-images (list M1)) (list MISSILE))
(check-expect (missile-images (list M1 M2)) (list MISSILE MISSILE))

;(define (missile-images lom) empty) ;stub

(define (missile-images lom)
  (cond [(empty? lom) empty]
        [else
         (cons MISSILE (missile-images (rest lom)))]))


;; ListOfMissiles -> ListOfPositions
;; produce list of positions for missiles
(check-expect (missile-positions empty) empty)
(check-expect (missile-positions (list M1)) (list (make-posn (missile-x M1) (missile-y M1))))
(check-expect (missile-positions (list M1 M2)) (list (make-posn (missile-x M1) (missile-y M1))
                                                     (make-posn (missile-x M2) (missile-y M2))))

;;(define (missile-positions lom) empty) ;stub

(define (missile-positions lom)
  (cond [(empty? lom) empty]
        [else
         (cons (make-posn (missile-x (first lom)) (missile-y (first lom)))
               (missile-positions (rest lom)))]))


;; ....................
;; Tank -> Image
;; render tank
(check-expect (render-tank T0) (place-image TANK (tank-x T0) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;(define (render-tank t) BACKGROUND) ;stub

(define (render-tank t)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))


;; --------------------
;; Game -> Game
;; produce next game, in which
;;   tank moves left (dir = -1) or right (dir = 1)
;;   tank bounces back if moving towards and at wall
;;   missile moves up MISSILE-SPEED if not hitting invader and removed if hitting
;;   invader appears at random coordinates and with random dir every INVADE-RATE ticks for MAX-NUM-INVADERS times
;;   invader moves diagonally down INVADER-SPEED towards left (dir = -1) or right (dir = 1) if not hit by missile and removed if hit
;;   invader bounces back if moving towards and at wall
;;   game freezes if invarder pasts bottom of screen
;;   game freezes if last invader is removed
(check-expect (next-game G0) (make-game (next-stats G0) (next-invaders G0) (next-missiles G0) (next-tank G0)))
;(check-expect (next-game G2) (make-game (next-stats G2) (next-invaders G2) (next-missiles G2) (next-tank G2)))
;(check-expect (next-game G3) (make-game (next-stats G3) (next-invaders G3) (next-missiles G3) (next-tank G3)))
;(check-expect (next-game G4) (make-game (next-stats G4) (next-invaders G4) (next-missiles G4) (next-tank G4)))
(check-expect (next-game G5) (make-game (next-stats G5) (next-invaders G5) (next-missiles G5) (next-tank G5)))
(check-expect (next-game G6) (make-game (next-stats G6) (next-invaders G6) (next-missiles G6) (next-tank G6)))
(check-expect (next-game G7) (make-game (next-stats G7) (next-invaders G7) (next-missiles G7) (next-tank G7)))

;(define (next-game g) g) ;stub

(define (next-game g)
  (make-game (next-stats g)
             (next-invaders g)
             (next-missiles g)
             (next-tank g)))


;; ....................
;; Game -> Stats
;; produce next stats, in which
;;   number of ticks (modulo INVADE-RATE) increase by 1
;;   total number of missiles increase by 1, up to MAX-NUM-INVADRES, if number of ticks (before increase) is (- INVADE-RATE 1)
;;   number of missiles left increase by 1, if total number of missiles increases by 1
;;   number of missiles left decrease by the number of invaders hit
;;   game-over is true if an invader reaches the bottom of screen, or
;;                        total number of invaders is MAX-NUM-INVADERS and last invader is hit
(check-expect (next-stats G0) (make-stats 1 0 0 false))
(check-expect (next-stats G2) (make-stats 0 2 2 false))
(check-expect (next-stats G3) (make-stats 0 3 2 false))
(check-expect (next-stats G4) (make-stats 0 3 3 true))
(check-expect (next-stats G5) (make-stats (- INVADE-RATE 1) 2 2 true))
(check-expect (next-stats G6) (make-stats 0 MAX-NUM-INVADERS 0 true))
(check-expect (next-stats G7) (make-stats (- INVADE-RATE 1) MAX-NUM-INVADERS 0 true))

;(define (next-stats g) (game-stats g)) ;stub

(define (next-stats g)
  (cond [(stats-game-over (game-stats g)) (game-stats g)]
        [else
         (make-stats (next-ticks (game-stats g))
                     (next-invaders-total (game-stats g))
                     (next-invaders-left g)
                     (next-game-over g))]))


;; Stats -> Natural[0, INVADE-RATE - 1]
;; produce next number of ticks (modulo INVADE-RATE)
;; ASSUME: game is not over
(check-expect (next-ticks S0) 1)
(check-expect (next-ticks S2) 0)

;(define (next-ticks s) 0) ;stub

(define (next-ticks s)
  (cond [(= (stats-ticks s) (- INVADE-RATE 1)) 0]
        [else
         (add1 (stats-ticks s))]))


;; Stats -> Natural[0, MAX-NUM-INVADERS]
;; produce next number of invaders, up to MAX-NUM-INVADRES, if number of ticks (before increase) is (- INVADE-RATE 1)
;; ASSUME: game is not over
(check-expect (next-invaders-total S0) 0)
(check-expect (next-invaders-total S2) 2)
(check-expect (next-invaders-total S6) MAX-NUM-INVADERS)

;(define (next-invaders-total s) 0) ;stub

(define (next-invaders-total s)
  (cond [(= (stats-ticks s) (- INVADE-RATE 1))
         (increase-invaders-total (stats-invaders-total s))]
        [else
         (stats-invaders-total s)]))


;; Natural[0, MAX-NUM-INVADERS] -> Natural[0, MAX-NUM-INVADERS]
;; produce next number of invaders, up to MAX-NUM-INVADRES
;; ASSUME: number of ticks (before increase) is (- INVADE-RATE 1)
;;         game is not over
(check-expect (increase-invaders-total 1) 2)
(check-expect (increase-invaders-total MAX-NUM-INVADERS) MAX-NUM-INVADERS)

;(define (increase-invaders-total noi) 0) ;stub

(define (increase-invaders-total noi)
  (cond [(= noi MAX-NUM-INVADERS)
         MAX-NUM-INVADERS]
        [else
         (add1 noi)]))


;; Game -> Natural[0, MAX-NUM-INVADER]
;; produce next number of invaders left, in which
;;   number of missiles left increase by 1, if total number of missiles increases by 1
;;   number of missiles left decrease by the number of invaders hit
;; ASSUME: game is not over
(check-expect (next-invaders-left G0) 0)
(check-expect (next-invaders-left G2) 2)
(check-expect (next-invaders-left G3) 2)
(check-expect (next-invaders-left G6) 0)

;(define (next-invaders-left g) 0) ;stub

(define (next-invaders-left g)
  (decrease-invaders-left g (increase-invaders-left (game-stats g))))


;; Stats -> Natural[0, MAX-NUM-INVADER]
;; produce next number of invaders left, in which
;;   number of missiles left increase by 1, if total number of missiles increases by 1
;; ASSUME: game is not over
(check-expect (increase-invaders-left S0) 0)
(check-expect (increase-invaders-left S2) 2)
(check-expect (increase-invaders-left S3) 3)
(check-expect (increase-invaders-left S6) 1)

;(define (increase-invaders-left s) 0) ;stub

(define (increase-invaders-left s)
  (cond [(and (= (stats-ticks s) (- INVADE-RATE 1))
              (< (stats-invaders-total s) MAX-NUM-INVADERS))
         (add1 (stats-invaders-left s))]
        [else
         (stats-invaders-left s)]))


;; Game Natural[0, MAX-NUM-INVADERS] -> Natural[0, MAX-NUM-INVADERS]
;; produce next number if invaders left, in which
;;   number of missiles left decrease by the number of invaders hit
;; ASSUME: game is not over
(check-expect (decrease-invaders-left G0 0) 0)
(check-expect (decrease-invaders-left G2 2) 2)
(check-expect (decrease-invaders-left G3 3) 2)
(check-expect (decrease-invaders-left G6 1) 0)

;(define (decrease-invaders-left g noil) 0) ;stub

(define (decrease-invaders-left g noil)
  (- noil (invaders-hit (game-invaders g) (game-missiles g))))


;; ListOfInvaders ListOfMissiles -> Natural[0, MAX-NUM-INVADER]
;; produce number of invaders hit
;; ASSUME: game is not over
(check-expect (invaders-hit empty empty) 0)
(check-expect (invaders-hit (list I1) (list M1)) 0)
(check-expect (invaders-hit (list I1 I2) (list M1 M2)) 1)

;(define (invaders-hit loi lom) 0) ;stub

(define (invaders-hit loi lom)
  (cond [(empty? loi) 0]
        [else
         (if (invader-hit? (first loi) lom)
             (add1 (invaders-hit (rest loi) lom))
             (invaders-hit (rest loi) lom))]))


;; Invader ListOfMissiles -> Boolean
;; produce true if invader is hit
;; ASSUME: game is not over
(check-expect (invader-hit? I1 (list M1 M2)) false)
(check-expect (invader-hit? I2 (list M1 M2)) true)

;(define (invader-hit? i lom) false) ;stub

(define (invader-hit? i lom)
  (cond [(empty? lom) false]
        [else
         (if (invader-hit-one-missile? i (first lom))
             true
             (invader-hit? i (rest lom)))]))


;; Invader Missile -> Boolean
;; produce true if invader is hit by missile
;; ASSUME: game is not over
(check-expect (invader-hit-one-missile? I2 M1) false)
(check-expect (invader-hit-one-missile? I2 M2) true)

;(define (invader-hit-one-missile? i m) false) ;stub

(define (invader-hit-one-missile? i m)
  (and (and (> (missile-x m) (- (invader-x i) X-HIT-RANGE))
            (< (missile-x m) (+ (invader-x i) X-HIT-RANGE)))
       (and (> (missile-y m) (- (invader-y i) Y-HIT-RANGE))
            (< (missile-y m) (+ (invader-y i) Y-HIT-RANGE)))))


;; Game -> Boolean
;; produce next game-over, in which
;;   true if an invader reaches the bottom of screen, or
;;           total number of invaders is MAX-NUM-INVADERS and last invader is hit
;; ASSUME: game is not over
(check-expect (next-game-over G0) false)
(check-expect (next-game-over G2) false)
(check-expect (next-game-over G4) true)
(check-expect (next-game-over G6) true)

;(define (next-game-over g) false) ;stub

(define (next-game-over g)
  (or (invader-reaches-bottom? (game-invaders g))
      (no-invaders-left? g)))


;; ListOfInvaders -> Boolean
;; produce true if an invader reaches bottom of screen
;; ASSUME: game is not over
(check-expect (invader-reaches-bottom? (game-invaders G2)) false)
(check-expect (invader-reaches-bottom? (game-invaders G4)) true)

;(define (invader-reaches-bottom? loi) false) ;stub

(define (invader-reaches-bottom? loi)
  (cond [(empty? loi) false]
        [else
         (if (invader-reaches-bottom-one-invader? (first loi))
             true
             (invader-reaches-bottom? (rest loi)))]))


;; Invader -> Boolean
;; produce true if invader reaches bottom of screen
;; ASSUME: game is not over

;(define (invader-reaches-bottom-one-invader? i) false) ;stub

(define (invader-reaches-bottom-one-invader? i)
  (>= (invader-y i) (- HEIGHT INVADER-HEIGHT/2)))


;; Game -> Boolean
;; produce true if total number of invaders is MAX-NUM-INVADERS and last invader is hit
;; ASSUME: game is not over
(check-expect (no-invaders-left? G2) false)
(check-expect (no-invaders-left? G6) true)

;(define (no-invaders-left? g) false) ;stub

(define (no-invaders-left? g)
  (and (and (= (stats-invaders-total (game-stats g)) MAX-NUM-INVADERS)
            (= (stats-invaders-left (game-stats g)) 1))
       (invader-hit? (first (game-invaders g)) (game-missiles g))))


;; ....................
;; Game -> ListOfInvaders
;; produce next list of invaders with
;;   added invader when counter is (- INVADER-RATE 1) and number of invaders is less than MAX-NUM-INVADERS
;;   invaders hit removed
;;   invaders going down by INVADER-X-SPEED and INVADER-Y-SPEED towards the left (dr = -1) or the right (dr = 1)
;;   invaders bounce back off wall
(check-expect (next-invaders G0) empty)
;(check-expect (next-invaders G2) (cons (add-invader 1) (advance-invaders G2)))
;(check-expect (next-invaders G3) (cons (add-invader 1) (advance-invaders G3)))
;(check-expect (next-invaders G4) (cons (add-invader 1) (advance-invaders G4)))
(check-expect (next-invaders G5) (game-invaders G5))
(check-expect (next-invaders G6) (advance-invaders G6))
(check-expect (next-invaders G7) (game-invaders G7))

;(define (next-invaders g) empty) ;stub

(define (next-invaders g)
  (cond [(stats-game-over (game-stats g))
         (game-invaders g)]
        [(and (= (stats-ticks (game-stats g)) (- INVADE-RATE 1))
              (< (stats-invaders-total (game-stats g)) MAX-NUM-INVADERS))
         (cons (add-invader 1) (advance-invaders g))]
        [else
         (advance-invaders g)]))


;; Natural[1] -> Invader
;; produce a random invader
;; ASSUME: game is not over
;; <tests not applicable>

;(define (add-invader n) (make-invader 100 100 1)) ;stub

(define (add-invader n)
  (make-invader (+ (random (add1 (- WIDTH INVADER-WIDTH))) INVADER-WIDTH/2)
                INVADER-HEIGHT/2
                (sub1 (* (random 2) 2))))


;; Game -> ListOfInvaders
;; advance invaders that
;;   invaders hit removed
;;   invaders going down by INVADER-X-SPEED and INVADER-Y-SPEED towards the left (dr = -1) or the right (dr = 1)
;;   invaders bounce back off wall
;; ASSUME: game is not over
(check-expect (advance-invaders G0) empty)
(check-expect (advance-invaders (make-game S2 (list I2) empty T1)) (list (make-invader (- 200 INVADER-X-SPEED)
                                                                                       (+ 300 INVADER-Y-SPEED)
                                                                                       -1)))
(check-expect (advance-invaders G3) (list (make-invader (+ 150 INVADER-X-SPEED)
                                                        (+ 200 INVADER-Y-SPEED)
                                                        1)))
(check-expect (advance-invaders (make-game S2 (list I5) empty T1)) (list I6))
(check-expect (advance-invaders (make-game S2 (list I7) empty T1)) (list I8))

;(define (advance-invaders g) loi) ;stub

(define (advance-invaders g)
  (cond [(empty? (game-invaders g)) empty]
        [(invader-hit? (first (game-invaders g)) (game-missiles g))
         (advance-invaders (make-game (game-stats g)
                                      (rest (game-invaders g))
                                      (game-missiles g)
                                      (game-tank g)))]
        [else
         (cons (advance-invader (first (game-invaders g)))
               (advance-invaders (make-game (game-stats g)
                                            (rest (game-invaders g))
                                            (game-missiles g)
                                            (game-tank g))))]))


;; Invader -> Invader
;; advance invader that
;;   invader going down by INVADER-X-SPEED and INVADER-Y-SPEED towards the left (dr = -1) or the right (dr = 1)
;;   invader bounces back off wall
;; ASSUME: game is not over
(check-expect (advance-invader I2) (make-invader (- 200 INVADER-X-SPEED)
                                                 (+ 300 INVADER-Y-SPEED)
                                                 -1))
(check-expect (advance-invader I1) (make-invader (+ 150 INVADER-X-SPEED)
                                                 (+ 200 INVADER-Y-SPEED)
                                                 1))
(check-expect (advance-invader I5) I6)
(check-expect (advance-invader I7) I8)

;(define (advance-invader i) i) ;stub

(define (advance-invader i)
  (cond [(= (invader-dir i) -1)
         (invader-moving-left i)]
        [else
         (invader-moving-right i)]))


;; Invader -> Invader
;; advance invader that
;;   invader going down by INVADER-X-SPEED and INVADER-Y-SPEED towards the left
;;   invader bounces back off wall
;; ASSUME: game is not over
(check-expect (invader-moving-left I2) (make-invader (- 200 INVADER-X-SPEED)
                                                     (+ 300 INVADER-Y-SPEED)
                                                     -1))
(check-expect (invader-moving-left I5) I6)

;(define (invader-moving-left i) i) ;stub

(define (invader-moving-left i)
  (cond [(<= (- (invader-x i) INVADER-X-SPEED) INVADER-WIDTH/2)
         (make-invader INVADER-WIDTH/2
                       (+ (invader-y i) (- (invader-x i) INVADER-WIDTH/2))
                       1)]
        [else
         (make-invader (- (invader-x i) INVADER-X-SPEED)
                       (+ (invader-y i) INVADER-Y-SPEED)
                       -1)]))


;; Invader -> Invader
;; advance invader that
;;   invader going down by INVADER-X-SPEED and INVADER-Y-SPEED towards the right
;;   invader bounces back off wall
;; ASSUME: game is not over
(check-expect (invader-moving-right I1) (make-invader (+ 150 INVADER-X-SPEED)
                                                      (+ 200 INVADER-Y-SPEED)
                                                      1))
(check-expect (invader-moving-right I7) I8)

;(define (invader-moving-right i) i) ;stub

(define (invader-moving-right i)
  (cond [(>= (+ (invader-x i) INVADER-X-SPEED) (- WIDTH INVADER-WIDTH/2))
         (make-invader (- WIDTH INVADER-WIDTH/2)
                       (+ (invader-y i) (- (- WIDTH INVADER-WIDTH/2) (invader-x i)))
                       -1)]
        [else
         (make-invader (+ (invader-x i) INVADER-X-SPEED)
                       (+ (invader-y i) INVADER-Y-SPEED)
                       1)]))


;; ....................
;; Game -> ListOfMissiles
;; produce next list of missiles with
;;   missiles hitting invaders removed
;;   missiles going up by MISSILE-SPEED
;;   missiles going beyond screen removed
(check-expect (next-missiles G0) empty)
(check-expect (next-missiles G2) (list (make-missile 150 (- 300 MISSILE-SPEED))))
(check-expect (next-missiles G3) (list (make-missile 150 (- 300 MISSILE-SPEED))))
(check-expect (next-missiles (make-game S2 (list I1) (list M1 M4) T1)) (list (make-missile 150 (- 300 MISSILE-SPEED))))
(check-expect (next-missiles G5) (game-missiles G5))
(check-expect (next-missiles G7) (game-missiles G7))

;(define (next-missiles g) empty) ;stub

(define (next-missiles g)
  (cond [(stats-game-over (game-stats g))
         (game-missiles g)]
        [else
         (advance-missiles g)]))


;; Game -> ListOfMissiles
;; advance missiles that
;;   missiles hitting invaders removed
;;   missiles going up by MISSILE-SPEED
;;   missiles going beyond screen removed
;; ASSUME: game is not over
(check-expect (advance-missiles G0) empty)
(check-expect (advance-missiles G2) (list (make-missile 150 (- 300 MISSILE-SPEED))))
(check-expect (advance-missiles G3) (list (make-missile 150 (- 300 MISSILE-SPEED))))
(check-expect (advance-missiles (make-game S2 (list I1) (list M1 M4) T1)) (list (make-missile 150 (- 300 MISSILE-SPEED))))

(define (advance-missiles g)
  (cond [(empty? (game-missiles g)) empty]
        [(or (hit-invader? (first (game-missiles g)) (game-invaders g))
             (beyond-screen? (first (game-missiles g))))
         (advance-missiles (make-game (game-stats g)
                                      (game-invaders g)
                                      (rest (game-missiles g))
                                      (game-tank g)))]
        [else
         (cons (advance-missile (first (game-missiles g)))
               (advance-missiles (make-game (game-stats g)
                                            (game-invaders g)
                                            (rest (game-missiles g))
                                            (game-tank g))))]))


;; Missile ListOfInvaders -> Boolean
;; produce true if missile hits an invader
;; ASSUME: game is not over
(check-expect (hit-invader? M1 (list I1 I2)) false)
(check-expect (hit-invader? M2 (list I1 I2)) true)

;(define (hit-invader? m loi) false) ;stub

(define (hit-invader? m loi)
  (cond [(empty? loi) false]
        [else
         (if (invader-hit-one-missile? (first loi) m)
             true
             (hit-invader? m (rest loi)))]))


;; Missile -> Boolean
;; produce true if missile is beyond sreen
;; ASSUME: game is not over
(check-expect (beyond-screen? M1) false)
(check-expect (beyond-screen? M4) true)

;(define (beyond-screen? m) false) ;stub

(define (beyond-screen? m)
  (<= (missile-y m) (- MISSILE-HEIGHT/2)))


;; Missile -> Missile
;; advance missile that
;;   missiles going up by MISSILE-SPEED
;; ASSUME: game is not over
(check-expect (advance-missile M1) (make-missile (missile-x M1)
                                                 (- (missile-y M1) MISSILE-SPEED)))

;(define (advance-missile m) m) ;stub

(define (advance-missile m)
  (make-missile (missile-x m)
                (- (missile-y m) MISSILE-SPEED)))


;; ....................
;; Game -> Tank
;; produce next tank in which
;;   tank moves TANK-SPEED towards the left (dr = -1) or the right (dr = 1)
;;   tank bounces back off wall
(check-expect (next-tank G1) (make-tank (+ 50 TANK-SPEED) 1))
(check-expect (next-tank G3) (make-tank (- 50 TANK-SPEED) -1))
(check-expect (next-tank (make-game S1 empty empty T3)) T4)
(check-expect (next-tank (make-game S1 empty empty T5)) T6)
(check-expect (next-tank G5) (game-tank G5))
(check-expect (next-tank G7) (game-tank G7))

;(define (next-tank g) (game-tank g)) ;stub

(define (next-tank g)
  (cond [(stats-game-over (game-stats g))
         (game-tank g)]
        [else
         (if (= (tank-dir (game-tank g)) -1)
             (tank-moving-left (game-tank g))
             (tank-moving-right (game-tank g)))]))


;; Tank -> Tank
;; move tank that
;;   tank moves TANK-SPEED towards the left
;;   tank bounces back off wall
;; ASSUME: game is not over
(check-expect (tank-moving-left T2) (make-tank (- 50 TANK-SPEED) -1))
(check-expect (tank-moving-left T3) T4)

;(define (tank-moving-left t) t) ;stub

(define (tank-moving-left t)
  (cond [(<= (- (tank-x t) TANK-SPEED) TANK-WIDTH/2)
         (make-tank TANK-WIDTH/2 1)]
        [else
         (make-tank (- (tank-x t) TANK-SPEED) -1)]))


;; Tank -> Tank
;; move tank that
;;   tank moves TANK-SPEED towards the right
;;   tank bounces back off wall
;; ASSUME: game is not over
(check-expect (tank-moving-right T1) (make-tank (+ 50 TANK-SPEED) 1))
(check-expect (tank-moving-right T5) T6)

;(define (tank-moving-right t) t) ;stub

(define (tank-moving-right t)
  (cond [(>= (+ (tank-x t) TANK-SPEED) (- WIDTH TANK-WIDTH/2))
         (make-tank (- WIDTH TANK-WIDTH/2) -1)]
        [else
         (make-tank (+ (tank-x t) TANK-SPEED) 1)]))


;; --------------------
;; Game KeyEvent -> Game
;; handle key in which
;;   missile added to list of missiles when space bar is pressed
;;   tank's direction changed to the left (if not already moving left) when left arrow key is pressed
;;   tank's direction changed to the right (if not already moving right) when right arrow key is pressed
(check-expect (handle-key G1 " ") (make-game (game-stats G1)
                                             (game-invaders G1)
                                             (cons (add-missile (game-tank G1)) (game-missiles G1))
                                             (game-tank G1)))
(check-expect (handle-key G1 "left") (make-game (game-stats G1)
                                                (game-invaders G1)
                                                (game-missiles G1)
                                                (make-tank 50 -1)))
(check-expect (handle-key G3 "right") (make-game (game-stats G3)
                                                 (game-invaders G3)
                                                 (game-missiles G3)
                                                 (make-tank 50 1)))
(check-expect (handle-key G1 "right") G1)
(check-expect (handle-key G3 "left") G3)
(check-expect (handle-key G1 "up") G1)
(check-expect (handle-key G5 " ") G5)
(check-expect (handle-key G5 "left") G5)
(check-expect (handle-key G5 "right") G5)
(check-expect (handle-key G5 "up") G5)
(check-expect (handle-key G7 " ") G7)
(check-expect (handle-key G7 "left") G7)
(check-expect (handle-key G7 "right") G7)
(check-expect (handle-key G7 "up") G7)

;(define (handle-key g ke) g) ;stub

(define (handle-key g ke)
  (cond [(stats-game-over (game-stats g))
         g]
        [(key=? ke " ")
         (make-game (game-stats g)
                    (game-invaders g)
                    (cons (add-missile (game-tank g)) (game-missiles g))
                    (game-tank g))]
        [(key=? ke "left")
         (make-game (game-stats g)
                    (game-invaders g)
                    (game-missiles g)
                    (make-tank (tank-x (game-tank g)) -1))]
        [(key=? ke "right")
         (make-game (game-stats g)
                    (game-invaders g)
                    (game-missiles g)
                    (make-tank (tank-x (game-tank g)) 1))]
        [else
         g]))


;; ....................
;; Tank -> Missile
(check-expect (add-missile T1) (make-missile 50 (- HEIGHT (+ TANK-HEIGHT MISSILE-HEIGHT/2))))
  
;(define (add-missile t) M1) ;stub

(define (add-missile t)
  (make-missile (tank-x t)
                (- HEIGHT (+ TANK-HEIGHT MISSILE-HEIGHT/2))))
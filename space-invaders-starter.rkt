;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; updated
;; Constants:

(define WIDTH  300)
(define HEIGHT 500)
(define MTS (empty-scene WIDTH HEIGHT))
  
(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

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

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-WIDTH/2 (/ (image-width TANK) 2))
(define MISSILE (ellipse 5 15 "solid" "red"))
(define TANK-HEIGHT (image-height TANK))
(define INVADER-WIDTH/2 (/ (image-width INVADER) 2))
(define INVADER-WIDTH (image-width INVADER))

;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; game -> game
;; start with (main G0)
(define (main g)
  (big-bang g
    (on-tick     advance-game)
    (to-draw     render-game)
    (on-key      handle-key)))

;; game -> game
;; advance the game pieces
;(define (advance-game g) g) ;stub
(define (advance-game g)
  (if (game-over? (game-invaders g)) g
  (make-game (new-invader (advance-invaders (game-invaders g) (game-missiles g)))
             (advance-missiles (game-missiles g))
             (advance-tank (game-tank g)))))

;; listofinvaders -> boolean
;; if invaders land then game stays stationary

(define (game-over? loi)
  (cond [(empty? loi) false]
        [else
         (if (landed? (first loi)) true
             (game-over? (rest loi)))]))

;; invader -> boolean
;; determines if invader landed i.e. game over
(define (landed? i)
   (> (invader-y i) HEIGHT))


;; listofinvaders listofmissiles -> listofinvaders
;; advance the invaders given missile data
;(define (advance-invaders loi lom) loi) ;stub

(define (advance-invaders loi lom)
  (cond [(empty? loi) empty]
        [else
         (if (is-hit? (first loi) lom)
             (advance-invaders (rest loi) lom)
             (cons (move-invader (first loi)) (advance-invaders (rest loi) lom)))]))

;; invader -> invader
;; move the invader
;(define (move-invader i) i) ;stub

(define (move-invader i)
  (cond [(< (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED)) INVADER-WIDTH/2)
            (make-invader INVADER-WIDTH/2 (+ (invader-y i) INVADER-Y-SPEED) 1)]
        [(> (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED)) (- WIDTH INVADER-WIDTH/2))
            (make-invader (- WIDTH INVADER-WIDTH/2) (+ (invader-y i) INVADER-Y-SPEED) -1)]
        [else
            (make-invader (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED))
                          (+ (invader-y i) INVADER-Y-SPEED) (invader-dx i))]))

;; invader listofmissiles -> boolean
;; returns true if invader is hit by missile
;(define (is-hit? i lom) false) ;stub
(check-expect (is-hit? I1 empty) false)
(check-expect (is-hit? I1 (cons M1 empty)) false)
(check-expect (is-hit? I1 (cons M2 empty)) true)
(check-expect (is-hit? I1 (cons M3 empty)) true)
               
(define (is-hit? i lom)
  (cond [(empty? lom) false]
        [else
         (if (and
              (>= (+ (missile-x (first lom)) HIT-RANGE) (invader-x i))
              (<= (- (missile-x (first lom)) HIT-RANGE) (invader-x i))
              (>= (+ (missile-y (first lom)) HIT-RANGE) (invader-y i))
              (<= (- (missile-y (first lom)) HIT-RANGE) (invader-y i)))
             true (is-hit? i (rest lom)))]))


;; listofinvaders -> listofinvaders
;; add new invader at random interval
;(define (new-invader loi) loi) ;stub
(define (new-invader loi)
  (if (< INVADE-RATE (random 105))
      (cons (make-invader (+ INVADER-WIDTH/2 (random (- WIDTH INVADER-WIDTH))) (/ (image-height INVADER) 2) 1) loi)
      loi))

;; tank -> tank
;; advance tank's position
(define (advance-tank t)
   (make-tank
    (cond [(< (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) TANK-WIDTH/2) TANK-WIDTH/2]
          [(> (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) (- WIDTH TANK-WIDTH/2)) (- WIDTH TANK-WIDTH/2)]
          [else (+ (tank-x t) (* (tank-dir t) TANK-SPEED))])
      (tank-dir t)))

;; listofmissiles -> listofmissiles
;; advances the missiles
;(define (advance-missiles lom) lom) ;stub

(define (advance-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (if (< (missile-y (first lom)) 0)
             (advance-missiles (rest lom))
             (cons (make-missile
                    (missile-x (first lom))
                    (- (missile-y (first lom)) MISSILE-SPEED))
                   (advance-missiles (rest lom))))]))

;; game -> image
;; render the game pieces
;; !!!
;(define (render-game g) MTS) ;stub
(define (render-game g) (place-image TANK (tank-x (game-tank g)) (- HEIGHT TANK-HEIGHT/2)
              (invader-image (game-invaders g) (missile-image (game-missiles g))))) 

;; listofinvaders img -> img
;; render the invaders
(define (invader-image loi img)
  (cond [(empty? loi) img]
        [else (draw-invader (first loi)
                            (invader-image (rest loi) img))]))

;; invader img -> img
;; draw a single invader
;(define (draw-invader i img) img) ; stub
(define (draw-invader i img)
  (place-image INVADER (invader-x i) (invader-y i) img))

;; listofmissiles -> image
;; draw missiles on background
(define (missile-image lom)
  (cond [(empty? lom) MTS]
        [else (draw-missile (first lom)
                 (missile-image (rest lom)))]))

;; misisle image -> image
;; helper to draw individual missiles
;(define (draw-missile m img) img) ;stub

(define (draw-missile m img) (place-image MISSILE (missile-x m) (missile-y m) img))

;; game keyevent -> game
;; key handler
;(define (handle-key g ke) g) ;stub

(define (handle-key g ke)
  (cond [(key=? "left" ke)
           (make-game (game-invaders g) (game-missiles g)
                      (make-tank (tank-x (game-tank g)) -1))]
        [(key=? "right" ke)
           (make-game (game-invaders g) (game-missiles g)
                      (make-tank (tank-x (game-tank g)) 1))]
        [(key=? " " ke)
           (make-game (game-invaders g)
                      (cons (make-missile (tank-x (game-tank g)) (- HEIGHT TANK-HEIGHT)) (game-missiles g))
                      (game-tank g))]
        [else g]))
































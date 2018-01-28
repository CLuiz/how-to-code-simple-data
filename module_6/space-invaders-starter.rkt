;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

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

(define MISSILE (ellipse 5 15 "solid" "red"))



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

;; Template Rules used:
;; - compound: 2 fields


;; ListOfInvader is one of:
;;  - empty
;;  - (cons Drop ListOfInvader)
;; interp. a list of invaders

(define LOI1 empty)
(define LOI2 (cons I1 (cons (make-invader 150 100 12) empty)))
(define LOI3 (cons I1 (cons I2 (cons I3 empty))))
(define LOI4 (cons I2 (cons I3 empty)))

#;
(define (fn-for-loi loi)
  (cond [(empty? lod) empty]
        [else
         (... (fn-for-invader (first lod))
              (fn-for-loi (rest lod)))]))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; Template Rules used:
;; - compound: 2 fields


;; ListOfMissile is one of:
;;  - empty
;;  - (cons Drop ListOfMissile)
;; interp. a list of missiles

(define LOM1 empty)
(define LOM2 (cons M1 (cons (make-missile 150 300) empty)))
(define LOM3 (cons M2 (cons M2 (cons M3 empty))))
(define LOM4 (cons M2 (cons M3 empty)))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) empty]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


; Functions:
;; Game -> Game
;; start space invaders program by evaluating (main empty)
(define (main game)
  (big-bang game
            (on-key handle-keys)
            (on-tick next-game)
            (to-draw render-game)))


;; ListOfMissile Tank KeyEvent -> ListOfMissile
;; if keyevent is "space bar" create a new missile at tank location
(check-expect (handle-keys G0 "left")
              (make-game empty empty (make-tank (/ WIDTH 2) -1)))
(check-expect (handle-keys
               (make-game empty empty (make-tank (/ WIDTH 2) -1)) "left")
              (make-game empty empty (make-tank (/ WIDTH 2) -1)))
(check-expect (handle-keys (make-game empty empty T2) "right")
              (make-game empty empty (make-tank 50 1)))
(check-expect (handle-keys G2 " ")
              (fire-missile (game-tank G2)))
(check-expect (handle-keys G2 "up")
              (make-game (list I1) (list M1) T1))
;(define (handle-keys g k) g) ;stub

(define (handle-keys g k)
  (cond
    [(key=? k "left")  (tank-changer k g)]
    [(key=? k "right") (tank-changer k g)]
    [(key=? k " ") (fire-missile (game-tank g))]
    [else g]))


;; Tank -> Tank
;; changes the direction of the tank
(check-expect (tank-changer "right" G0) G0)   
(check-expect (tank-changer "left" G0) (make-game
                                        (game-invaders G0)
                                        (game-missiles G0)
                                        (make-tank (/ WIDTH 2)-1)))
(check-expect (tank-changer "right" G2) (make-game
                                        (game-invaders G2)
                                        (game-missiles G2)
                                        (make-tank 50 1)))
(check-expect (tank-changer "right" (make-game
                                     (game-invaders G2)
                                     (game-missiles G2)
                                     T2))
              (make-game
               (game-invaders G2)
               (game-missiles G2)
               (make-tank 50 1)))
; (define (tank-changer k g) g)  ;stub

;<used template for tank>
(define (tank-changer k g)
  (cond [(change-direction? k (game-tank g))
         (make-game
          (game-invaders g)
          (game-missiles g)
          (make-tank (tank-x (game-tank g))
                     (- (tank-dir (game-tank g)))))]
        [else g]))


;; Key Tank -> Boolean
;; produces true if tank should change direction else false
(check-expect (change-direction? "right" T0) false)
(check-expect (change-direction? "left" T0) true)
(check-expect (change-direction? "left" T2) false)
(check-expect (change-direction? "right" T2) true)
;(define (change-direction? k t) false) ;stub

;<took template from tank>
(define (change-direction? k t)
  (cond [(or (and (key=? k "right")
                  (= (tank-dir t) -1))
             (and (key=? k "left")
                  (= (tank-dir t) 1)))
         true]
        [else false]))


;;Tank ListOfMissile -> ListOfMissile
;; fires a missile at current tank location
(check-expect (fire-missile T0) (make-missile (tank-x T0) 10))
(check-expect (fire-missile T1) (make-missile (tank-x T1) 10))
(check-expect (fire-missile T2) (make-missile (tank-x T2) 10))              
; (define (fire-missile t lom) t lom);; stub

;<used function for missile>
(define (fire-missile t)
  (make-missile (tank-x t) 10))

;; Game -> Game
;; produces a new game from game
(define (next-game g) 0)


;; Game -> Game
;; renders the current game
(define (render-game g ) 0)
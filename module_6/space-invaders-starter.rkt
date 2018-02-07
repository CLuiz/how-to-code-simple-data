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
(define TANK-Y (- HEIGHT 10))
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

(define TANK-HEIGHT (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loi (game-invaders s))
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

(define M1 (make-missile 150 300))                               ;not hit U1
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
;; start space invaders program by evaluating (main game)
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
; (define (fire-missile t lom) t lom); stub

;<used function for missile>
(define (fire-missile t)
  (make-missile (tank-x t) 10))

;; Game -> Game
;; produces a new game from game
(check-expect (next-game G0)
              (make-game
               empty empty
               (make-tank
                (+ (tank-x T0) (* TANK-SPEED (tank-dir T0)))(tank-dir T0))))
(check-expect (next-game
               (make-game empty empty T2))
               (make-game empty empty
                          (make-tank
                           (+ (tank-x T2) (* TANK-SPEED (tank-dir T2)))(tank-dir T2))))

(check-expect (next-game
               (make-game (list I1) empty T2))
               (make-game (list (make-invader (+ (invader-x I1) INVADER-X-SPEED) (+ (invader-y I1) INVADER-Y-SPEED) (invader-dx I1)))
                          empty
                          (make-tank
                           (+ (tank-x T2) (* TANK-SPEED (tank-dir T2)))(tank-dir T2))))
(check-expect (next-game
               (make-game (list I1) (list M1) T2))
               (make-game (list (make-invader (+ (invader-x I1) INVADER-X-SPEED) (+ (invader-y I1) INVADER-Y-SPEED) (invader-dx I1)))
                          (list (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED)))
                          (make-tank
                           (+ (tank-x T2) (* TANK-SPEED (tank-dir T2)))(tank-dir T2))))
;(define (next-game g) g) ;stub

;<took template for game>
(define (next-game g)
  (make-game (next-invaders (game-invaders g)) (next-missiles (game-missiles g)) (next-tank (game-tank g))))


;; ListOfInvaders -> ListOfInvaders
;; produces new invader for each invader in list at
;(check-expect (next-invaders I1) (make-invader
; (define (next-invaders loi) loi) ;stub

(define (next-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (next-invader (first loi))
              (next-invaders (rest loi)))]))

; Invader -> Invader
; produces new invader
;<took template for invaders>
(define (next-invader i)
  (make-invader (+ (invader-x i) INVADER-X-SPEED) (+ (invader-y i) INVADER-Y-SPEED) (invader-dx i)))

;; ListOfInvader -> ListOfInvader
;; produce a list containing only those invaders in loi that are onscreen?
(check-expect (invaders-onscreen empty) empty)
(check-expect (invaders-onscreen (list (make-invader 150 100 12)))
              (list (make-invader 150 100 12)))
(check-expect (invaders-onscreen (list (make-invader 150 -5 12)))
              empty)
(check-expect (invaders-onscreen (list I1 I2 I3))
              (list I1 I2 I3))
(check-expect (invaders-onscreen (list I1 I2 I3 (make-invader 150 -10 12)))
              (list I1 I2 I3))
; (define (invader-onscreen loi) loi)  ;stub

; template from ListOfInvader
(define (invaders-onscreen loi)
  (cond [(empty? loi) empty]
        [else
         (if (invader-onscreen? (first loi))
             (cons (first loi) (invaders-onscreen (rest loi)))
             (invaders-onscreen (rest loi)))]))

;; Invader -> Boolean
;; produce true if i has not fallen off the bottom of background
(check-expect (invader-onscreen? (make-invader 2 -1 12)) false)
(check-expect (invader-onscreen? (make-invader 2 0 12)) true)
(check-expect (invader-onscreen? (make-invader 2 1 12)) true)
(check-expect (invader-onscreen? (make-invader 2 (- HEIGHT 1) 12)) true)
(check-expect (invader-onscreen? (make-invader 2 HEIGHT 12)) true)

;template from Invader

(define (invader-onscreen? i)
  (<= 0 (invader-y i)))

;; ListOfMissiles -> ListOfMissiles
;; produces new list of missiles with each advanced by MISSILE-SPEED
; (define (next-missiles lom) lom) ; stub

(define (next-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (next-missile (first lom))
              (next-missiles (rest lom)))]))


;; Missile -> Missile
; increase missile y by Missile speed 
;(define (next-missile m) m) ; stub

;<used template for missile>
(define (next-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))


; ListOfMissile -> ListOfMissile
; produce list of missiles with offscreen missiles removed
(check-expect (missiles-onscreen empty) empty)
(check-expect (missiles-onscreen (list M1 M2)) (list M1 M2))
(check-expect (missiles-onscreen (list M1 (make-missile 10 (+ HEIGHT 1)))) (list M1))
;(define (missiles-onscreen lom) lom) ;stub

;<template from ListOfMissile>
(define (missiles-onscreen lom)
  (cond [(empty? lom) empty]
        [else
         (if (missile-onscreen? (first lom))
             (cons (first lom) (missiles-onscreen (rest lom)))
             (missiles-onscreen (rest lom)))]))

; Missile -> Boolean
; produce true if missile is on screen else false
(check-expect (missile-onscreen? (make-missile 10 (+ HEIGHT 1))) false)
(check-expect (missile-onscreen? (make-missile 10 HEIGHT)) true)
(check-expect (missile-onscreen? M1) true)
(check-expect (missile-onscreen? M3) true)
;(define (missile-onscreen? m) true) ; stub

;<took template from missile>
(define (missile-onscreen? m)
  (<= (missile-y m) HEIGHT))

;; Tank -> Tank
;; produces tank advanced in it's current direction by TANK-SPEED
(check-expect (next-tank (make-tank WIDTH 1)) (make-tank WIDTH -1))
(check-expect (next-tank (make-tank 0 -1)) (make-tank 0 1))
(check-expect (next-tank T1) (make-tank (+ (tank-x T1) TANK-SPEED) 1))
(check-expect (next-tank T2) (make-tank (- (tank-x T2) TANK-SPEED) -1))
;(define (next-tank t) t) ;stub

;<took template from tank>
(define (next-tank t)
  (cond [(> (+ (tank-x t) (* TANK-SPEED (tank-dir t))) WIDTH) (make-tank WIDTH (- (tank-dir t)))]
        [(< (+ (tank-x t) (* TANK-SPEED (tank-dir t))) 0) (make-tank 0 (- (tank-dir t)))]
        [else
         (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t)))(tank-dir t))]))

;; Game -> Game
;; renders the current game
(check-expect (render-game G0) (place-image TANK (tank-x (game-tank G0)) TANK-Y BACKGROUND))
(check-expect (render-game G1) (place-image TANK (tank-x (game-tank G1)) TANK-Y BACKGROUND))
(check-expect (render-game G2) (place-image INVADER (invader-x (first (game-invaders G2))) (invader-y (first (game-invaders G2)))
                                            (place-image MISSILE (missile-x (first (game-missiles G2)))
                                                         (missile-y (first (game-missiles G2)))
                                                         (place-image TANK (tank-x (game-tank G2)) TANK-Y BACKGROUND))))
(check-expect (render-game G3) (place-image INVADER (invader-x (first (game-invaders G3))) (invader-y (first (game-invaders G3)))
                                            (place-image INVADER (invader-x (first (rest (game-invaders G3)))) (invader-y (first (rest (game-invaders G3))))
                                                         (place-image MISSILE (missile-x (first (game-missiles G3)))
                                                                      (missile-y (first (game-missiles G3)))
                                                                      (place-image MISSILE (missile-x (first (rest (game-missiles G3))))
                                                                                   (missile-y (first (rest (game-missiles G3))))
                                                                                   (place-image TANK (tank-x (game-tank G3)) TANK-Y BACKGROUND))))))
;(define (render-game g ) g)

;<took template for game>

(define (render-game g)
  (render-invaders (game-invaders g)
                   (render-missiles (game-missiles g)
                                    (render-tank (game-tank g) BACKGROUND))))

;(define G0 (make-game empty empty T0))
;(define G1 (make-game empty empty T1))
;(define G2 (make-game (list I1) (list M1) T1))
;(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;ListOfInvaders -> Image
; produces overlayed image of all invaders
(check-expect (render-invaders empty BACKGROUND) BACKGROUND)
(check-expect (render-invaders LOI1 BACKGROUND) BACKGROUND)
(check-expect (render-invaders LOI3 BACKGROUND)
              (place-image INVADER 150 100
                            (place-image INVADER 150 500
                                         (place-image INVADER 150 510 BACKGROUND))))
;(define (render-invaders loi) loi) ;stub

;took template for ListOfInvaders w/extra parameter

(define (render-invaders loi img)
  (cond [(empty? loi) img]
        [else
         (render-invader (first loi)
         (render-invaders (rest loi) img))]))

; Invader -> image
; produces image of invader at invader-x invader-y
(check-expect (render-invader I1 BACKGROUND)
              (place-image INVADER (invader-x I1) (invader-y I1) BACKGROUND))
(check-expect (render-invader I2 BACKGROUND)
              (place-image INVADER (invader-x I2) (invader-y I2) BACKGROUND))
(check-expect (render-invader I3 BACKGROUND)
              (place-image INVADER (invader-x I3) (invader-y I3) BACKGROUND))
;(define (render-invader i) i) ; stub

;<took template from invader w/extra param>
(define (render-invader i img)
  (place-image INVADER (invader-x i) (invader-y i) img))

; ListOfMissile -> Image
; produce image of each missile in list
(check-expect (render-missiles empty BACKGROUND) BACKGROUND)                                                              
(check-expect (render-missiles LOM2 BACKGROUND) (place-image MISSILE 150 300
                                                  (place-image MISSILE 150 300 BACKGROUND)))  ;exactly hit U1
(check-expect (render-missiles LOM3 BACKGROUND) (place-image MISSILE 150 110
                                                  (place-image MISSILE 150 110
                                                               (place-image MISSILE 150 105 BACKGROUND))))
;(define (render-missiles lom) lom) ;stub

;<took template from ListOfMissile w/extra parameter
(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else
         (render-missile (first lom)
         (render-missiles (rest lom) img))]))

; Missile -> Image
; produce image of missile
(check-expect (render-missile M1 BACKGROUND) (place-image MISSILE (missile-x M1) (missile-y M1) BACKGROUND))
(check-expect (render-missile M2 BACKGROUND) (place-image MISSILE (missile-x M2) (missile-y M2) BACKGROUND))
(check-expect (render-missile M3 BACKGROUND) (place-image MISSILE (missile-x M3) (missile-y M3) BACKGROUND))
;(define (render-missile m) m) ;stub

;<took template from missile w/extra parameter
(define (render-missile m img)
  (place-image MISSILE (missile-x m) (missile-y m) img))


;Tank -> Image
;produce image of tank
(check-expect (render-tank T0 BACKGROUND) (place-image TANK (/ WIDTH 2) TANK-Y BACKGROUND))
(check-expect (render-tank T1 BACKGROUND) (place-image TANK (tank-x T1) TANK-Y BACKGROUND))
(check-expect (render-tank T2 BACKGROUND) (place-image TANK (tank-x T2) TANK-Y BACKGROUND))
;(define (render-tank t) t) ; stub

;<took template from missile w/extra parameter
(define (render-tank t img)
  (place-image TANK (tank-x t) TANK-Y img))
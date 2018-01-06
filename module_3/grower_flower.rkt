;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grower_flower) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;PROBLEM:

;Design a World Program with Compound Data. You can be as creative as you like,
;but keep it simple. Above all, follow the recipes!
;You must also stay within the scope of the first part of the course.
;Do not use language features we have not seen in the videos. 

;If you need inspiration, you can choose to create a program that allows you
;to click on a spot on the screen to create a flower, which then grows over time.
;If you click again the first flower is replaced by a new one at the new position.

;You should do all your design work in DrRacket. Following the step-by-step recipe
;in DrRacket will help you be sure that you have a quality solution.

(require 2htdp/image)
(require 2htdp/universe)

;; ====================
;; Constants

(define WIDTH 400)
(define HEIGHT 200)

(define FLOWER-WIDTH-MAX 20)
(define FLOWER-HEIGHT-MAX 20)
(define FLOWER (star 40 "solid" "firebrick"))
(define GROWTH 10)
(define MTS (empty-scene WIDTH HEIGHT))

;; =====================
;; Data definitions
(define-struct flower (x y h w g))
;; Flower is (make-flower Natural[0, HEIGHT] Natural[0, WIDTH] Natural[0, 20] Natural[0, 20] Integer)
;; interp. (make-flower x y h w) is a flower with coordinates x, y and size h, w and growth rate g
;;         x, y is the center of the flower
;;         x is in the screen coordinates (pixels)
;;         y is in the screen coordinates (pixels)
;;         h is in the screen coordinates (pixels)
;;         w is in the screen coordinates (pixels)
;;         g is rate of growth
(define F1 (make-flower 10 10 10 10 GROWTH)) ; flower centered at 10,10 with height 10 and width 10,
;;                                        growing at a rate of GROWTH
(define F2 (make-flower 20 20 15 5 GROWTH)) ; flower centered at 20, 20 with height 15 and width 5,
;;                                        growing at a rate of GROWTH
#;
(define (fn-for-flower f)
  (... (flower-x f)       ;Natural[0, HEIGHT]
       (flower-y f)       ;Natural[0, WIDTH]
       (flower-h f)       ;Natural[0, 20]
       (flower-w f)       ;Natural[0, 20]
       (flower-g f)))     ;Integer

;;Template rules used:
;;  - compound: 5 fields

;; =====================
;; Functions:

;; Flower -> Flower
;; called to grow the first flower; start with (main (make-flower))
;; no tests for main function
(define (main f)
  (big-bang f
    (on-tick next-flower)      ; Flower -> Flower
    (to-draw render-flower)    ; Flower -> Image
    (on-mouse handle-mouse)))  ; Flower MouseEvent -> Flower

;; Flower -> Flower
;; increase flower size by g;
(check-expect (next-flower (make-flower 10 10 20 20 GROWTH))
              (make-flower 10 10 10 10 GROWTH)) ;shrink

(check-expect (next-flower F1)
              (make-flower 10 10 (+ 10 GROWTH) (+ 10 GROWTH) GROWTH)) ;grow

;(define (next-flower f) f)       ;stub

; took template from Flower

(define (next-flower f)
  (cond [(and (<= (+ (flower-h f) (flower-g f)) FLOWER-HEIGHT-MAX)
              (<= (+ (flower-w f) (flower-g f)) FLOWER-WIDTH-MAX))
         (make-flower (flower-x f)
                      (flower-y f)
                      (+ (flower-h f) (flower-g f))
                      (+ (flower-w f) (flower-g f))
                      (flower-g f))]
        [(or (> (+ (flower-h f) (flower-g f)) FLOWER-HEIGHT-MAX)
             (> (+ (flower-w f) (flower-g f)) FLOWER-WIDTH-MAX))
         (make-flower 10 10 10 10 GROWTH)]))

;; Cow -> Image
;; place appropriate sized flower image on MTS at (flower-x f) and (flower-y f)
(check-expect (render-flower (make-flower 20 20 10 10 GROWTH))
              (place-image FLOWER 20 20 MTS))

;(define (render-flower f) MTS)   ;stub

; took template from flower
(define (render-flower f)
  (place-image FLOWER
               (flower-x f)
               (flower-y f)
               MTS))    

;;Flower -> Flower

;(define (grow-flower FLOWER)
;  (if (> (flower-x f) 0)
;      RCOW
;      LCOW)))

; Flower MouseEvent -> Flower
; Grow new flower at x, y coordinates of mouse click

;(define (handle-mouse f me) f)  ;stub

(define (handle-mouse f x y me)
  (cond [(mouse=? me "button-down") (make-flower x y 20 20 GROWTH)]))

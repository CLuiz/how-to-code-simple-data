;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grower_flower) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;PROBLEM:

;Design a World Program with Compound Data. You can be as creative as you like,
;but keep it simple. Above all, follow the recipes!
;You must also stay within the scope of the first part of the course.
;Do not use language features we have not seen in the videos. 

;If you need inspiration, you can choose to create a program that allows you
;to click on a spot on the screen to create a star, which then grows over time.
;If you click again the first star is replaced by a new one at the new position.

;You should do all your design work in DrRacket. Following the step-by-step recipe
;in DrRacket will help you be sure that you have a quality solution.

(require 2htdp/image)
(require 2htdp/universe)

;; ====================
;; Constants

(define WIDTH 400)
(define HEIGHT 200)

(define STAR-SIZE-MAX 200)
(define COLOR "firebrick")
(define IMG-TYPE "solid")
(define FIRST-STAR (star 40 "solid" "COLOR"))
(define GROWTH 10)
(define MTS (empty-scene WIDTH HEIGHT))

;; =====================
;; Data definitions
(define-struct gstar (x y s g))
;; Gstar is (make-gstar Natural[0, HEIGHT] Natural[0, WIDTH] Natural[0, 20] Natural[0, 20] Integer)
;; interp. (make-gstar x y h w) is a gstar with coordinates x, y and size h, w and growth rate g
;;         x, y is the center of the gstar
;;         x is in the screen coordinates (pixels)
;;         y is in the screen coordinates (pixels)
;;         s is in the screen coordinates (pixels)
;;         g is rate of growth
(define GS1 (make-gstar 10 10 10 GROWTH)) ; gstar centered at 10,10 with size 10,
;;                                        growing at a rate of GROWTH
(define GS2 (make-gstar 20 20 80 GROWTH)) ; gstar centered at 20, 20 with size 80,
;;                                        growing at a rate of GROWTH
#;
(define (fn-for-gstar gs)
  (... (gstar-x gs)       ;Natural[0, HEIGHT]
       (gstar-y gs)       ;Natural[0, WIDTH]
       (gstar-s gs)       ;Natural[0, STAR-SIZE-MAX]
       (gstar-g gs)))     ;Integer

;;Template rules used:
;;  - compound: 5 fields

;; =====================
;; Functions:

;; Gstar -> Gstar
;; called to grow the first gstar; gstart with (main (make-gstar))
;; no tests for main function
(define (main gs)
  (big-bang gs
    (on-tick next-gstar)      ; gstar -> gstar
    (to-draw render-gstar)    ; gstar -> Image
    (on-mouse handle-mouse))) ; gstar MouseEvent -> gstar

;; Gstar -> Gstar
;; increase gstar size by growth rate g;
(check-expect (next-gstar (make-gstar 10 10 STAR-SIZE-MAX GROWTH))
              (make-gstar 10 10 10 GROWTH)) ;shrink
(check-expect (next-gstar GS1)
              (make-gstar 10 10 (+ 10 GROWTH) GROWTH)) ;grow
;(define (next-gstar gs) gs)       ;stub

; took template from gstar
(define (next-gstar gs)
  (if (< (+ (gstar-s gs) (gstar-g gs)) STAR-SIZE-MAX)
      (make-gstar (gstar-x gs) (gstar-y gs) (+ (gstar-s gs) (gstar-g gs)) (gstar-g gs))
      (make-gstar (gstar-x gs) (gstar-y gs) 10 (gstar-g gs))))
  
;; Gstar -> Image
;; place appropriate sized gstar image on MTS at (gstar-x gs) and (gstar-y gs)
(check-expect (render-gstar GS1)
              (place-image (star 10 IMG-TYPE COLOR) 10 10 MTS))
(check-expect (render-gstar GS2)
              (place-image (star 80 IMG-TYPE COLOR) 20 20 MTS))
;(define (render-gstar gs) MTS)   ;stub

; took template from gstar
(define (render-gstar gs)
  (place-image (star (gstar-s gs) IMG-TYPE COLOR) (gstar-x gs) (gstar-y gs) MTS))    

; gstar MouseEvent -> gstar
; Grow new gstar at x, y coordinates of mouse click
(check-expect (handle-mouse 10 40 40 "button-down") (make-gstar 40 40 20 10))
(check-expect (handle-mouse 10 40 40 "button-up") 10)
(check-expect (handle-mouse 10 10 40 "button-down") (make-gstar 10 40 20 10))
(check-expect (handle-mouse 10 10 40 "button-up") 10)

;(define (handle-mouse gs x y me) gs)  ;stub

; took template from gstar
(define (handle-mouse gs x y me)
  (cond [(mouse=? me "button-down") (make-gstar x y 20 GROWTH)]
        [else gs]))
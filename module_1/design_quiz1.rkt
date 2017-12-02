;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname design_quiz1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;PROBLEM:

;Design a function that consumes two images and produces true if the first is larger than the second.

;Complete your design using DrRacket. When you are done, you must submit something in this box in order to unlock the assessment rubric, but when you are doing your assessment, grade your submission in DrRacket where indentation and formatting will be preserved.

;Be sure to watch the evaluation video before completing your assessment.


;;Image, Image -> Boolean
;; Take two images and produce true if the first is larger (area)
(check-expect (first-bigger (rectangle 1 2 "solid" "red") (rectangle 3 4 "solid" "red")) false)

;(define (first-bigger img1 img2) ;stub

;(define (first-bigger img1 img2) ;template
;  (...img1 img2))


(define (first-bigger img1 img2)
  (> (* (image-width img1) (image-height img1)) (* (image-width img2) (image-height img2))))

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname shots-fired) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define HEIGHT 220) ; Distances in terms of pixels
(define WIDTH 30)
(define XSHOTS (/ WIDTH 3))

; graphical constants
(define BACKGROUND (rectangle WIDTH HEIGHT "solid" "green"))
(define SHOT (rectangle 6 18 "solid" "black"))

; A List-of-shots is one of:
; - '()
; - (cons Shot List-of-shots)
; interpretation the collection of shots fired

; A Shot is a Number
; interpretation represents the shot's y-coordinate

; A ShotWorld is List-of-numbers.
; interpretation each number on such a list
;     represents the y-coordinate of a shot

; ShotWorld -> Image
; adds the image of a shot for each y on w
; at (MID,y} to the background image
;(check-expect (to-image '()) BACKGROUND)
;(check-expect (to-image (cons 9 '()))
;              (place-image SHOT XSHOTS 9 BACKGROUND))
(define (to-image w)
  (cond
    [(empty? w) BACKGROUND]
    [else (place-image SHOT XSHOTS (first w) (to-image (rest w)))]))

; ShotWorld -> ShotWorld
; removes shots from world after they exit the canvas
(define (remove-shot w)
  (cond
    [(empty? w) '()]
    [(positive? (first w)) w]
    [else (remove-shot (rest w))]))

; ShotWorld -> ShotWorld
; moves each shot up by one pixel
(check-expect (tock (list 1 -2 3 4)) (tock (list 1 -2 3 4)))
(define (tock w)
  (cond
    [(empty? w) '()]
    [else (cons (sub1 (first w)) (tock (remove-shot (rest w))))]))

; ShotWorld KeyEvent -> ShotWorld
; adds a shot to the world
; if the player presses the space bar
(define (keyh w ke)
  (if (key=? ke " ") (cons HEIGHT w) w))

; ShotWorld -> ShotWorld
(define (main w0)
  (big-bang w0
    [on-tick tock]
    [on-key keyh]
    [to-draw to-image]))
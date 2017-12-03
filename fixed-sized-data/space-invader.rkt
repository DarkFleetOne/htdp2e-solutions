;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname space-invader) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define WIDTH 200)
(define HEIGHT 200)
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define UFO (overlay/align
             "center"
             "bottom"
             (circle 10 "solid" "green")
             (ellipse 40 10 "solid" "green")))

(define TANK (rectangle 50 10 "solid" "black"))
(define MISSLE (triangle 5 "solid" "red"))
(define INI-X-UFO 175)
(define INI-Y-UFO 50)
(define INI-X-TANK 30)
(define INI-Y-TANK 180)
(define initial-scene (place-images (list UFO TANK)
                                    (list (make-posn INI-X-UFO INI-Y-UFO)
                                          (make-posn INI-X-TANK INI-Y-TANK))
                                    BACKGROUND))

; A UFO is a Posn.
; interpretation (make-posn x y) is the UFO's location
; (using the top-down, left-to-right convention)

(define-struct vel [dx dy])
; A vel is a structure:
;    (make-vel Number Number).
; interpretation, describes a given velocity

(define-struct tank [loc vel])
; A Tank is a structure:
;    (make-tank Number Number).
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixel/tick

; A Missile is a Posn.
; interpretation (make-posn x y) is the missile's place

(define-struct aim [ufo tank])
; interpretation time period when player is aiming their shot

(define-struct fired [ufo tank missile])
; interpretation time preiod when player has fired missile

; A SIGS is one of:
; - (make-aim UFO TANK)
; - (make-fired UFO TANK MISSILE)
; interpretation represents the complete state of a
; space invader game

; SIGS -> IMAGE
; adds TANK, UFO, and possibly MISSILE to
; the BACKGROUND scene
(define (si-render s) BACKGROUND)
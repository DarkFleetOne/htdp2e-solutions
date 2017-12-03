;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Cruisin-n-Boozin) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; Constant Definitions

; Initial World State
(define ws 0)
; Initial Size of Scene
(define WIDTH-OF-WORLD 200)
(define HEIGHT-OF-WORLD 100)
; Initial Size of Wheels
(define WHEEL-RADIUS 5)
; Basic Structure of Wheels
(define WHEEL-DISTANCE
  (* WHEEL-RADIUS 5))
; Graphical Constant for Car Initial Height
(define Y-CAR 50)
; Wheel Graphical Constant
(define WHEEL
  (circle WHEEL-RADIUS "solid" "black"))
; Graphical Constant Defining Space Between Wheels
(define SPACE
  (rectangle WHEEL-DISTANCE WHEEL-RADIUS "solid" "white"))
; Graphical Constant Defining Lower Body of CAR
(define BOTH-WHEELS
  (beside WHEEL SPACE WHEEL))
; Graphical Constant Defining Middle Body of CAR
(define CAR-HEIGHT (* WHEEL-RADIUS 2))
(define CAR-MIDDLE
  (rectangle (+ WHEEL-DISTANCE 20) CAR-HEIGHT "solid" "red"))
; Graphical Constant for Lower Car
(define CAR-LOWER
  (overlay/offset BOTH-WHEELS 0 -5 CAR-MIDDLE))
; Graphical Constant for CAR, places a cabin over the existing car body
(define CAR
  (overlay/offset CAR-LOWER 0 -5 (rectangle WHEEL-DISTANCE (+ CAR-HEIGHT 5) "solid" "red")))
; Graphical Constant for Scene Background
(define BACKGROUND
  (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD))
;Graphical Constant for Tree in Scene
(define TREE
  (underlay/xy (circle 10 "solid" "green")
               9 15
               (rectangle 2 20 "solid" "brown")))

; Data Definitions

; A WorldState is a Number.
; Interpretation: the number of pixels between
; the left border of the scene and the car

; WorldState -> Image
; Places the objects into the BACKGROUND scene,
; according to the given world state
(define (render ws)
  (place-images
   (list CAR TREE)
   (list (make-posn (+ ws (image-width CAR)) Y-CAR)
         (make-posn 50 (/ Y-CAR 2)))
   BACKGROUND))

; An Animation State is a Number
; Interpretation: the number of clock ticks
; since the animation started

; Number -> Image
; Animates the CAR according to time past

; WorldState -> WorldState
; Moves the car by 3 pixels for every clock tick
(check-expect (tock 20) 23)
(check-expect (tock 78) 81)
(define (tock ws)
  (+ ws 3))

; WorldState Number Number String -> WorldState
; places the CAR at X-MOUSE
; if the give ME is "button-down"
(check-expect (hyper 21 10 20 "enter") 21)
(check-expect (hyper 42 10 20 "button-down") 10)
(check-expect (hyper 42 10 20 "move") 42)
(define (hyper x-position-of-car x-mouse y-mouse me)
  (cond
    [(string=? "button-down" me) x-mouse]
    [else x-position-of-car]))

; WorldState -> Boolean
; Checks WorldState against Scene Size
; Returns #true when CAR leaves SCENE
(define (finished ws)
  (> ws (- WIDTH-OF-WORLD WHEEL-DISTANCE)))

; WorldState -> WorldState
; launches the program from some initial state
(define (main ws)
  (big-bang ws
            [on-tick tock]
            [on-mouse hyper]
            [to-draw render]
            [stop-when finished]
            [close-on-stop (finished ws)]
            [name "Cruisin n Boozin"]))
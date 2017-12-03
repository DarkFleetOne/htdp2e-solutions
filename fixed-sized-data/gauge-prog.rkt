;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname gauge-prog) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; Constant Definitions

; Width of Gauge
(define GAUGE-WIDTH 250)
; Height of Gauge
(define GAUGE-HEIGHT (/ GAUGE-WIDTH 5))
; Initial State of World
(define ws GAUGE-WIDTH)

; Data Definitions

; WorldState -> WorldState
; Reduces Happiness by 0.1 with each clock tick
(define (tock ws)
  (- ws 0.1))

; WorldState -> WorldState
; Increases Happiness based on key events
(define (happy-boost ws a-key)
  (cond
    [(key=? a-key "up") (+ (* GAUGE-WIDTH 1/3) ws)]
    [(key=? a-key "down") (+ (* GAUGE-WIDTH 1/5) ws)]
    [else ws]))

; WorldState -> Image
; Fills Gauge based on Cat Happiness
(define (render ws)
  (cond
  [(and (<= ws GAUGE-WIDTH) (>= ws 0)) (overlay/align "left" "middle"
                                        (rectangle ws GAUGE-HEIGHT "solid" "red")
                                        (rectangle GAUGE-WIDTH GAUGE-HEIGHT "outline" "black"))]
  [(> ws GAUGE-WIDTH) (frame (rectangle GAUGE-WIDTH GAUGE-HEIGHT "solid" "red"))]
  [(<= ws 0) (rectangle GAUGE-WIDTH GAUGE-HEIGHT "outline" "black")]))

; WorldState -> WorldState
; Launches the program from some initial state
(define (gauge-prog ws)
  (big-bang ws
            [on-tick tock]
            [on-key happy-boost]
            [to-draw render]
            [name "gauge-prog"]))
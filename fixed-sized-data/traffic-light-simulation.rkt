;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname traffic-light-simulation) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; Constant Definitions
(define RADIUS 10)
(define BACKGROUND (empty-scene 80 30))

; A TrafficLight is one of the following Strings:
; - "red"
; - "green"
; - "yellow"
; interpretation the three strings represent the three
; possible states that a traffic light may assume

; TrafficLight -> TrafficLight
; yields the next state, given current state cs
;(define (tl-next cs)
;  (+ 1 cs))

; ALT A N-TrafficLight is one of:
; - 0 interpretation the traffic light shows red
; - 1 interpretation the traffic light shows green
; - 2 interpretation the traffic light shows yellow
(define (tl-next-numeric cs) (modulo (+ cs 1) 3))

; TrafficLight -> Image
; draws single color bulb based on state
(define (tl-render-bulb current-state)
  (circle RADIUS "solid" (cond
                           [(= current-state 0) "red"]
                           [(= current-state 1) "green"]
                           [(= current-state 2) "yellow"])))

; TrafficLight -> Image
; renders the current state cs as an image
(define (tl-render current-state)
  (place-image (tl-render-bulb current-state) (cond
                                                [(= 0 (modulo current-state 3)) 20]
                                                [(= 0 (modulo current-state 2)) 40]
                                                [(= 0 (modulo current-state 1)) 60])
               15 BACKGROUND))

; TrafficLight -> TrafficLight
; simulates a clock-based American traffic light
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
            [to-draw tl-render]
            [on-tick tl-next-numeric 1]))
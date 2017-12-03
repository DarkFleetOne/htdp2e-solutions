;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname UFO-descending) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; A WorldState is a Number.
; interpretation number of pixels between the top and the UFO
 
(define WIDTH 300) ; distances in terms of pixels
(define HEIGHT 100)
(define HALFWAY (/ WIDTH 2))
(define CLOSE (/ HEIGHT 3))
(define MTSCN (empty-scene WIDTH HEIGHT))
(define UFO (overlay/align
             "center"
             "bottom"
             (circle 10 "solid" "green")
             (ellipse 40 10 "solid" "green")))
(define CENTER-IMAGE (- HEIGHT (/ (image-height UFO) 2)))
(define LANDING-ZONE (/ HEIGHT 10))
 
; WorldState -> WorldState
(define (main y0)
  (big-bang y0
     [on-tick nxt]
     [to-draw render/status]
     [stop-when finished]))
 
; WorldState -> WorldState
; computes next location of UFO 
(check-expect (nxt 11) 14)
(define (nxt y)
  (+ y 3))
 
; WorldState -> Image
; place UFO at given height into the center of MTSCN
(check-expect (render 11) (place-image UFO HALFWAY 11 MTSCN))
(define (render y)
  (place-image UFO HALFWAY y MTSCN))

; WorldState -> Image
; adds a status line to the scene created by render
(check-expect (render/status 10)
              (place-image (text "descending" 11 "green")
                           50 10
                           (render 10)))
; Original rendering handler
;(define (render/status y)
;  (cond [(<= 0 y CLOSE)
;         (place-image (text "descending" 11 "green")
;                      50 10
;                      (render y))]
;        [(and (< CLOSE y) (<= y HEIGHT))
;         (place-image (text "closing in" 11 "orange")
;                      50 10
;                      (render y))]
;        [(>= y CENTER-IMAGE)
;         (place-image (text "landed" 11 "red")
;                      50 10
;                      (render y))]))

; Handler after refactoring cond expression
(define (render/status y)
  (place-image
   (cond
     [(<= 0 y CLOSE)
      (text "descending" 11 "green")]
     [(and (< CLOSE y) (<= y HEIGHT))
      (text "closing in" 11 "orange")]
     [(> y HEIGHT)
      (text "landed" 11 "red")])
   50 10
   (render y)))

; WorldState -> Boolean
; returns #true when bottom of UFO touches bottom of scene
; Scene stops when #true
(define (finished ws)
  (if (> ws CENTER-IMAGE) #true #false))
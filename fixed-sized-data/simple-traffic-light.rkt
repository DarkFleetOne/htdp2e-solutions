;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname simple-traffic-light) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define RADIUS 25)

(check-expect (tock 0) 1)
(define (tock ws)
  (+ ws 1))

(check-expect (render 1) (circle RADIUS "solid" "yellow"))
(check-expect (render 6) (circle RADIUS "solid" "green"))
(check-expect (render 17) (circle RADIUS "solid" "red"))
(define (render ws)
  (circle RADIUS "solid" (cond
                           [(= 0 (modulo ws 3)) "green"]
                           [(= 1 (modulo ws 3)) "yellow"]
                           [(= 2 (modulo ws 3)) "red"])))

(define (traffic-light ws)
  (big-bang ws
            (on-tick tock)
            (to-draw render)
            (name "traffic-light")))
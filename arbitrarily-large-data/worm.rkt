;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname worm) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t quasiquote mixed-fraction #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))

(define-struct worm (x y dir))
; A Worm is a structure:
;  (make-worm Number Number String)
; interpretation (make-worm x y dir) combines
; the position (* x SIZE) pixels from left and (* y SIZE) from top
; and the direction the worm is facing (N, S, E, W)

(define (worm ws)
  (big-bang (make-worm ws 1 "E")
    (on-tick tock)
    (on-key press)
    (to-draw render)))
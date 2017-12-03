;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname time-to-seconds) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct time [h m s])
; A time is  a structure:
;    (make-time Number Number Number)
; interpretation hours minutes seconds after midnight

; Time -> Number
; converts time into seconds
(check-expect (time->seconds (make-time 12 30 2)) 45002)
(define (time->seconds t)
  (+ (* (time-h t) 3600) (* (time-m t) 60) (time-s t)))
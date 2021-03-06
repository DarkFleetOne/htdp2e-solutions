;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname russian-dolls) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct layer [color doll])

; A Russian Doll (RD) is one of:
; - String
; - (make-layer String RD)

(define doll0 (make-layer "green" "red"))
(define doll1 (make-layer "pink" (make-layer "black" "white")))
(define doll2 (make-layer "orange" (make-layer "blue" (make-layer "green" "red"))))

; RD -> Number
; how many dolls are part of an-rd
;(check-expect (depth "red") 1)
;(check-expect (depth doll1) 3)
(define (depth an-rd)
  (cond
    [(string? an-rd) 1]
    [else (+ (depth (layer-doll an-rd)) 1)]))

; RD -> String
; produces a string of colors, separated by a comma and space,
; that make up an RD
;(check-expect (colors doll2) "orange, blue, green, red")
(define (colors an-rd)
  (cond
    [(string? an-rd) an-rd]
    [(layer? an-rd) (string-append (layer-color an-rd) ", " (colors (layer-doll an-rd)))]))

; RD -> String
; produces the color of the innermost doll
;(check-expect (inner doll1) "white")
(define (inner an-rd)
  (cond
    [(string? an-rd) an-rd]
    [(layer? an-rd) (inner (layer-doll an-rd))]))
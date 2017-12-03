;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname simple-abstraction) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))

; [Number -> Number] Number -> [List-of Number]
; tabulates R between n and 0 (incl.) in a list
(define (tabulate F n)
  (cond
    [(= n 0) (list (F 0))]
    [else (cons (F n) (tabulate F (sub1 n)))]))

; Number -> [List-of Number]
; tabulates n by sin
(define (tab-sin n)
  (tabulate sin n))

; Number -> [List-of Number]
; tabulates n by sqrt
(define (tab-sqrt n)
  (tabulate sqrt n))

; [Number -> Number] [List-of Number] -> Number
; computes F of the numbers on l
(define (fold1 F l)
  (cond
    [(empty? l) 0]
    [else (F (first l) (fold1 F (rest l)))]))

; [X Y] [List-of X] Y [X Y -> Y] -> Y
(define (fold2 l bs jn)
  (cond
    [(empty? l) bs]
    [else (jn (first l) (fold2 jn bs (rest l)))]))

; [List-of Number] -> Number
(define (product l)
  (fold2 l 1 *))

; graphical constants
(define emt (empty-scene 100 100))
(define dot (circle 3 "solid" "red"))

; Posn Image -> Image
(define (place-dot p img)
  (place-image
   dot
   (posn-x p) (posn-y p)
   img))

; [List-of Posn] -> Image
(define (image* l)
  (fold2 l emt place-dot))

; [X Y] [List-of X] Y [X Y -> Y] -> Y
(define (reduce l base combine)
  (cond
    [(empty? l) base]
    [else (combine (first l) (reduce (rest l) base combine))]))

; [List-of Number] -> Number
(define (sum lon)
  (reduce lon 0 +))

; [List-of Number] -> Number
(define (product lon)
  (reduce lon 1 *))
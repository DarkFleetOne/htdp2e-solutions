
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")
((modname abstractions-by-example)
 (read-case-sensitive #t)
 (teachpacks ((lib "image.rkt" "teachpack" "2htdp")
              (lib "universe.rkt" "teachpack" "2htdp")
              (lib "batch-io.rkt" "teachpack" "2htdp")))
 (htdp-settings #(#t constructor repeating-decimal #f #t none #f
                  ((lib "image.rkt" "teachpack" "2htdp")
                   (lib "universe.rkt" "teachpack" "2htdp")
                   (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))

; A Shape is a function:
;  [Posn -> Boolean]
; interpretation if s is a shape and p a Posn, (s p)
; produces #true if p is in s, #false otherwise

; Shape Posn -> Boolean
(define (inside? s p)
  (s p))

; Posn -> Boolean
; simplistic element of the Shape Class
; (λ (p) (and (= (posn-x p) 3) (= (posn-y p) 4)))

; Number Number -> Shape
; represents a point at (x,y)

(check-expect (inside? (mk-point 3 4)
                       (make-posn 3 4))
              #true)
(check-expect (inside? (mk-point 3 4)
                       (make-posn 3 0))
              #false)

(define (mk-point x y)
  (λ (p)
    (and (= (posn-x p) x) (= (posn-y p) y))))

; Number Number Posn -> Number
; computes the distance between (x,y) and p
(define (distance-between x y p)
  (sqrt (+ (expt (- (posn-x p) x) 2)
           (expt (- (posn-y p) y) 2))))

; Number Number Number -> Shape
; creates a representation for a circle of radius r
;  located at (center-x, center-y)

(check-expect (inside? (mk-circle 3 4 5)
                       (make-posn 0 0))
              #true)
(check-expect (inside? (mk-circle 3 4 5)
                       (make-posn 0 9))
              #false)
(check-expect (inside? (mk-circle 3 4 5)
                       (make-posn -1 3))
              #true)

(define (mk-circle center-x center-y r)
  ; [Posn -> Boolean]
  (λ (p)
    (<= (distance-between center-x center-y p) r)))


; Number Number Number Number -> Shape
; represents a width by height rectangle whose
; upper-left corner is located at (ul-x, ul-y)

(check-expect (inside? (mk-rect 0 0 10 3)
                       (make-posn 0 0))
              #true)
(check-expect (inside? (mk-rect 0 0 10 3)
                       (make-posn -1 -1))
              #false)
(check-expect (inside? (mk-rect 2 3 10 3)
                       (make-posn 4 5))
              #true)

(define (mk-rect ul-x ul-y width height)
  (λ (p)
    (and (<= ul-x (posn-x p) (+ ul-x width))
         (<= ul-y (posn-y p) (+ ul-y height)))))


; Shape Shape -> Shape
; combines two shapes into one

(define (mk-combination s1 s2)
  ; Posn -> Boolean
  (λ (p)
    (or (inside? s1 p) (inside? s2 p))))

(define circle1 (mk-circle 3 4 5))
(define rectangle1 (mk-rect 0 3 10 3))
(define union1 (mk-combination circle1 rectangle1))

(check-expect (inside? union1 (make-posn 0 0)) #true)
(check-expect (inside? union1 (make-posn 0 9)) #false)
(check-expect (inside? union1 (make-posn -1 3)) #true)

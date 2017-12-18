
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


; [List-of Posn] -> [List-of Posn]
; adds 3 to each x-coordinate on the given list

(check-expect
 (add-3-to-all
  (list (make-posn 3 1) (make-posn 0 0)))
 (list (make-posn 6 1) (make-posn 3 0)))

(define (add-3-to-all lop)
  (local (; Posn -> Posn
          ; adds 3 to the x-coordinate of p
          (define (add-3-to-1 p)
            (make-posn (+ (posn-x p) 3) (posn-y p))))
    (map add-3-to-1 lop)))


; [List-of Posn] -> [List-of Posn]
; eliminates Posns whose y-coordinate is > 100

(check-expect
 (keep-good (list (make-posn 0 110) (make-posn 0 60)))
 (list (make-posn 0 60)))

(define (keep-good lop)
  (local (; Posn -> Boolean
          ; should this Posn stay on the list?
          (define (good? p)
            (not (> (posn-y p) 100))))
    (filter good? lop)))


; Posn Posn Number -> Boolean
; is the distance between p and q less than d

(define (close-to p q d)
  (local ((define (distance p q)
            (sqrt (+ (expt (- (posn-x q) (posn-x p)) 2)
                     (expt (- (posn-y q) (posn-y p)) 2)))))
      (<= (distance p q) d)))

; [List-of Posn] Posn -> Boolean
; is any Posn on lop close to pt

(check-expect
 (close? (list (make-posn 47 54) (make-posn 0 60)) (make-posn 50 50))
 #true)

(define (close? lop pt)
  (local ((define CLOSENESS 5) ; in terms of pixels
          ; Posn -> Boolean
          ; is one shot close to pt
          (define (is-one-close? p)
            (close-to p pt CLOSENESS)))
    (ormap is-one-close? lop)))


(define DOT (circle 5 "solid" "red"))

(define MT-SCENE (empty-scene 200 200))

; [List-of Posn] -> Image
; adds the Posns on lop to the empty scene

(check-expect (dots (list (make-posn 12 31)))
              (place-image DOT 12 31 MT-SCENE))

(define (dots lop)
  (local (; Posn Image -> Image
          (define (add-one-dot p scene)
            (place-image DOT
                         (posn-x p) (posn-y p)
                         scene)))
    (foldr add-one-dot MT-SCENE lop)))


(define (convert-euro lod)
  (local (; dollar -> euro
          (define (dollar->euro d)
            (* 1.06 d)))
    (map dollar->euro lod)))


(define (convertFC lof)
  (local (; fahrenheit -> celsius
          (define (f->c f)
            (/ (* (- f 32) 5) 9)))
    (map f->c lof)))


(define (translate lop)
  (local (; posn -> pair
          (define (posn->pair p)
            `(,(posn-x p) ,(posn-y p))))
   (map posn->pair lop)))

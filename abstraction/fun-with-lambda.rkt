
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

(define BACKGROUND (empty-scene 100 100))
(define DOT (circle 5 "solid" "red"))

(define (dots lop)
  (foldr (λ (a-posn scene)
           (place-image DOT
                        (posn-x a-posn)
                        (posn-y a-posn)
                        scene))
         BACKGROUND lop))


; [List-of Posn] -> [List-of Posn]
(define (add-3-to-all lop)
  (map (λ (p)
         (make-posn (+ (posn-x p) 3) (posn-y p)))
       lop))


; [List-of Posn] -> [List-of Posn]
(define (keep-good lop)
  (filter (λ (p) (<= (posn-y p) 100)) lop))


(define (close-to p pt dis) #t)
(define CLOSENESS 5)

; [List-of Posn] -> Boolean
(define (close? lop pt)
  (ormap (λ (p) (close-to p pt CLOSENESS))
         lop))


(define (convert-euro lod)
  (map (λ (d)
         (* 1.06 d))
       lod))


(define (convertFC lof)
  (map (λ (f)
         (/ (* (- f 32) 5) 9))
       lof))


(define (translate lop)
  (map (λ (p)
         `(,(posn-x p) ,(posn-y p)))
       lop))


; [X X -> Boolean] -> [[List-of X] -> Boolean]
; is the given list sorted according to cmp

(check-expect [(sorted string<?) '("b" "c")] #true)
(check-expect [(sorted <) '(1 2 3 4 5 6)] #true)

(define (sorted cmp)
  (λ (l0)
    (local (; [NEList-of X] -> Boolean
            ; is l sorted according to cmp
            (define (sorted/l l)
              (cond
                [(empty? (rest l)) #true]
                [else (and (cmp (first l) (second l))
                           (sorted/l (rest l)))])))
      (if (empty? l0) #true (sorted/l l0)))))


; [List-of X] [List-of X] -> Boolean
; are all items in list k members of list l

(check-expect (contains? '(1 2 3) '(1 4 3)) #false)
(check-expect (contains? '(1 2 3 4) '(1 3)) #true)

(define (contains? l k)
  (andmap (λ (in-k) (member? in-k l)) k))


; [List-of X] [X X -> Boolean] -> [[List-of X] -> Boolean]
; is l0 sorted according to cmp
; are all items in list k members of list l0

(check-expect [(sorted-variant-of '(3 2) <) '(2 3)] #true)
(check-expect [(sorted-variant-of '(3 2) <) '(3)] #false)

(define (sorted-variant-of k cmp)
  (λ (l0)
    (and ((sorted cmp) l0)
         (contains? k l0)
         (contains? l0 k))))

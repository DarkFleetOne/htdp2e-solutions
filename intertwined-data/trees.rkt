#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")
((modname test)
 (read-case-sensitive #t)
 (teachpacks ((lib "image.rkt" "teachpack" "2htdp")
              (lib "universe.rkt" "teachpack" "2htdp")
              (lib "batch-io.rkt" "teachpack" "2htdp")
              (lib "abstraction.rkt" "teachpack" "2htdp")))
 (htdp-settings #(#t constructor repeating-decimal #f #t none #f
                  ((lib "image.rkt" "teachpack" "2htdp")
                   (lib "universe.rkt" "teachpack" "2htdp")
                   (lib "batch-io.rkt" "teachpack" "2htdp")
                   (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))


(define-struct no-parent [])
(define NP (make-no-parent))
(define-struct child [father mother name date eyes])
; An FT (short for family tree) is one of:
; - NP
; - (make-child FT FT String N String)

; Oldest Genreation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))

; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child Carl Bettina "Fred" 1966 "pink"))

; Youngest Generation:
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

; FT -> Boolean
; does an-ftree contain a child
; structure with "blue" in the eyes field

(check-expect (blue-eyed-child? Carl) #false)
(check-expect (blue-eyed-child? Gustav) #true)

(define (blue-eyed-child? an-ftree)
  (cond
    [(no-parent? an-ftree) #false]
    [else (or (string=? (child-eyes an-ftree) "blue")
              (blue-eyed-child? (child-father an-ftree))
              (blue-eyed-child? (child-mother an-ftree)))]))

; FT -> Number
; consumes a family tree and returns the number of child structures

(check-expect (count-persons Carl) 1)
(check-expect (count-persons Fred) 3)
; (check-expect (count-persons Gustav) 5)

(define (count-persons ft)
  (cond
    [(and (no-parent? (child-father ft)) (no-parent? (child-mother ft))) 1]
    [else (+ 1
             (count-persons (child-father ft))
             (count-persons (child-mother ft)))]))

; FT -> Number
; consumes a family tree and returns the average age of all child structures

(check-expect (average-age Carl 2017) 91)
(check-expect (average-age Gustav 2017) 62)
(check-expect (average-age Eva 2017) 78)

(define (average-age ftree year)
  (cond
    [(and (no-parent? (child-father ftree))
          (no-parent? (child-mother ftree)))
     (- year (child-date ftree))]
    [else (round (/ (+ (average-age (child-father ftree) year)
                       (average-age (child-mother ftree) year)
                       (- year (child-date ftree))) 3))]))


; FT -> List-of Strings
; consumes a family tree and returns a list of all eye colors

(check-expect (eye-colors Carl) '("green"))
(check-expect (eye-colors Fred) '("pink" "green" "green"))

(define (eye-colors ftree)
  (cond
    [(no-parent? ftree) '()]
    [else (append (list (child-eyes ftree))
                  (eye-colors (child-father ftree))
                  (eye-colors (child-mother ftree)))]))


; FT -> Boolean
; Like blue-eyed-child, but responds #true only when
; a proper ancestor, not the given child itself,
; has blue eyes

(check-expect (blue-eyed-ancestor? Eva) #false)
(check-expect (blue-eyed-ancestor? Gustav) #true)

(define (blue-eyed-ancestor? ftree)
  (cond
    [(and (no-parent? (child-father ftree))
          (no-parent? (child-mother ftree))) #false]
    [else (or (string=? (child-eyes (child-father ftree)) "blue")
              (string=? (child-eyes (child-mother ftree)) "blue")
              (blue-eyed-ancestor? (child-father ftree))
              (blue-eyed-ancestor? (child-mother ftree)))]))


; An FF (short for family forest) is one of:
; - '()
; - (cons FT FF)
; e.g. a List-of Family Trees
; interpretation a family forest represents several
; families (say, a town) and their ancestor trees

(define ff1 `(,Carl ,Bettina))
(define ff2 `(,Fred ,Eva))
(define ff3 `(,Fred ,Eva ,Carl))

; [List-of FT] -> Boolean
; does the forest contain any child with "blue" eyes

(check-expect (blue-eyed-child-in-forest? ff1) #false)
(check-expect (blue-eyed-child-in-forest? ff2) #true)
(check-expect (blue-eyed-child-in-forest? ff3) #true)

(define (blue-eyed-child-in-forest? forest)
  (cond
    [(empty? forest) #false]
    [else (ormap blue-eyed-child? forest)]))


; [List-of FT] Number -> Number
; consumes Family Forest, returns average age

(check-expect (average-forest-age ff1 2017) 91)
(check-expect (average-forest-age ff2 2017) 78)
(check-expect (average-forest-age ff3 2017) 81.25)

(check-expect (average-forest-age ff1 2017) 91)

(define (average-forest-age forest year)
  (cond
    [(empty? (rest forest)) (average-age (first forest) year)]
    [else (/ (+ (average-age (first forest) year)
                (average-forest-age (rest forest) year))
             2)]))

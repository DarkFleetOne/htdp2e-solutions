;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chapter10-exercises-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List-of-posns -> Number
; consumes a list of posns and sums posn-x
(check-expect (sum (list (make-posn 1 2) (make-posn 2 3) (make-posn 3 4))) 6)
(define (sum lop)
  (cond
    [(empty? lop) 0]
    [else (+ (posn-x (first lop)) (sum (rest lop)))]))

; List-of-posns -> List-of-posns
; for each (make-posn x y), produce (make-posn x (+ y 1))
; interpretation translates a point along the y-axis
(check-expect (translate (cons (make-posn 4 5)
                               (cons (make-posn 5 6) '())))
                         (cons (make-posn 4 6)
                               (cons (make-posn 5 7) '())))
(define (translate lop)
  (cond
    [(empty? lop) '()]
    [else (cons (make-posn (posn-x (first lop)) (+ 1 (posn-y (first lop))))
                           (translate (rest lop)))]))

; List-of-posns -> List-of-posns
; result contains all these posns
; - (< (> posn-x 0) 100)
; - (< (> posn-y 0) 200)
(define (legal lop)
  (cond
    [(empty? lop) '()]
    [(and (< (> (posn-x (first lop)) 0) 100) (< (> (posn-y (first lop)) 0) 200))
     (cons (make-posn (posn-x (first lop)) (posn-y (first lop))) (legal (rest lop)))]
    [else (legal (rest lop))]))

(define-struct phone [area switch four])
; A Phone is a structure:
;    (make-phone Three Three Four)
; A Three is a Number between 100 and 99.
; A Four is a Number between 1000 and 9999.

; List-of-phones -> List-of-phones
; replaces all occurences of area code 713 with 281
(define (replace lop)
  (cond
    [(empty? lop) '()]
    [(= (phone-area (first lop)) 713)
     (cons (make-phone 813 (phone-switch lop) (phone-four lop)) (replace (rest lop)))]
    [else (cons (first lop) (replace (rest lop)))]))
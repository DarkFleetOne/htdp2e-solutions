;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname count-equal-strings) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define test-los (list "hello" "goodbye" "hello" "world" "hello"))

; List-of-string String -> N
; determines how often s occurs in los

(define (count los s)
  (cond
    [(empty? los) 0]
    [(string=? (first los) s) (add1 (count (rest los) s))]
    [else (count (rest los) s)]))

(check-expect (count test-los "hello") 3)
(check-expect (count test-los "foo") 0)
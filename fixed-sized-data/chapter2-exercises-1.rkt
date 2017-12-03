;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chapter2-exercises-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(define (f x) 1)
(define (ff x) (* 10 x))
(define (distance x y) (sqrt (+ (expt x 2) (expt y 2))))
(define (sqarea l) (* 4 l))
(define (csurface l) (* 6 (sqarea l)))
(define (cvolume l) (* l (sqarea l)))
(define (string-first s) (if (> (string-length s) 0) (string-ith s 0) "Empty String!"))
(define (string-last s) (if (> (string-length s) 0) (string-ith s (string-length s)) "Empty String!"))
(define (==> sunny friday) (or (not sunny) friday))
(define (image-area i) (* (image-height i) (image-width i)))
(define (image-classify i) (cond [(> (image-height i) (image-width i)) "tall"]
                                 [(< (image-height i) (image-width i)) "wide"]
                                 [(= (image-height i) (image-width i)) "square"]))
(define (string-join str1 str2) (string-append str1 "_" str2))
(define (string-insert str i) (string-append (substring str 0 i) "_" (substring str i)))
(define (string-delete s i) ;Removes 1String from ith position
  (if
   (and (< i (string-length s)) (> i 0))
           (string-append
            (substring s 0 i) (substring s (+ i 1)))
           (substring s (+ i 1))))
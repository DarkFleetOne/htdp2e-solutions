;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname remove-string-ith) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define str "helloworld")
(define shave 0)
(define shear 3)
(define cut 5)
(define slice 7)
(define (string-scissors s i) ;Removes 1String from ith position
  (if
   (and (< i (string-length s)) (> i 0))
           (string-append
            (substring s 0 i) (substring s (+ i 1)))
           (substring s (+ i 1))))
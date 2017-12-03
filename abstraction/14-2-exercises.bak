;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 14-2-exercises) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))

(define LIST1 '(25 24 23 22 21 20 19 18 17 16 15 14 13
                   12 11 10 9 8 7 6 5 4 3 2 1))

(define LIST2 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
                  17 18 19 20 21 22 23 24 25))

(define (extract.v1 R l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (if (R (first l)
                 (extract.v1 R (rest l)))
              (first l)
              (extract.v1 R (rest l)))]))

(define (extract R l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (R (first l) (extract R (rest l)))]))

(check-expect (inf-1 LIST1) 1)
(check-expect (sup-2 LIST2) 25)

(define (inf-1 l)
  (extract.v1 < l))

(define (inf-2 l)
  (extract min l))

(define (sup-1 l)
  (extract.v1 > l))

(define (sup-2 l)
  (extract max l))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lists-of-lists-of-files) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; A List-of-strings is:
; - '()
; - (cons String List-of-strings)
; interpretation (read-lines file) creates a list-of-strings, one per line
; (read-words file) creates a list-of-strings, one per word
(define line0 (cons "hello" (cons "world" '())))
(define line1 '())

; A List-of-list-of-strings is
; - '()
; - (cons List-of-strings List-of-strings)
; interpretation (read-words/line file) creates a list-of-list-of-strings
; each line is a list-of-strings, one string per word.
(define lls0 '())
(define lls1 (cons line0 (cons line1 '())))



; List-of-list-of-strings -> List-of-numbers
; determines the number of words on each line
(check-expect (words-on-line lls0) '())
(check-expect (words-on-line lls1)
              (cons 2 (cons 0 '())))

(define (words-on-line lls)
  (cond
    [(empty? lls) '()]
    [else (cons (length (first lls))
                (words-on-line (rest lls)))]))

; String -> List-of-numbers
; counts the words on each line in the given file
(define (file-statistic file-name)
  (words-on-line
   (read-words/line file-name)))
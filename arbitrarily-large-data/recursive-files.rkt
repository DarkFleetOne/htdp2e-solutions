;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname recursive-files) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; list-of-strings -> String
(define (parse-line los)
  (cond
    [(empty? los) ""]
    [else (string-append (first los) " " (parse-line (rest los)))]))

; List-of-lines -> String
(define (collapse lol)
  (cond
    [(empty? lol) ""]
    [else (string-append (parse-line (first lol)) "\n" (collapse (rest lol)))]))

; List-of-strings -> List-of-Strings
; Removes articles, "a" "an" "the" from a list of strings
(define (r-articles-line los)
  (cond
    [(empty? los) '()]
    [(or (or (string=? "a" (first los)) (string=? "an" (first los))) (string=? "the" (first los)))
     (r-articles-line (rest los))]
    [else (cons (first los) (r-articles-line (rest los)))]))

; LLS -> LLS
; Removes articles, "a" "an" "the" from an lls
(define (remove-articles lls)
  (cond
    [(empty? lls) '()]
    [else (cons (r-articles-line (first lls)) (remove-articles (rest lls)))]))

; File -> File
; Removes all articles from a text file
(define (no-articles file)
  (write-file (string-append "no-articles-" file)
              (collapse (remove-articles (read-words/line file)))))

; 1String -> String
; converts the given 1String to a 3-letter numeric String
(check-expect (encode-letter "z") (code1 "z"))
(check-expect (encode-letter "\t")
              (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a")
              (string-append "0" (code1 "a")))

(define (encode-letter s)
  (cond
    [(>= (string->int s) 100) (code1 s)]
    [(< (string->int s) 10)
     (string-append "00" (code1 s))]
    [(< (string->int s) 100)
     (string-append "0" (code1 s))]))

; 1String -> String
; converts the given 1String into a numeric String
(check-expect (code1 "z") "122")

(define (code1 c)
  (number->string (string->int c)))

; List-of-1String -> List-of-string
; (encode-letter s) but recursive
(define (encode-letter* lo1)
  (cond
    [(empty? lo1) '()]
    [else (cons (encode-letter (first lo1)) (encode-letter* (rest lo1)))]))

; String -> String
; converts given string into its numeric equivalent
(define (encode-word s)
  (implode (encode-letter* (explode s))))

; List-of-Strings -> List-of-Strings
; encodes a line of strings into its numeric equivalent
(define (encode-line los)
  (cond
    [(empty? los) '()]
    [else (cons (encode-word (first los)) (encode-line (rest los)))]))

; LLS -> LLS
; converts an lls into its numerically encoded equivalent
(define (encode-lls lls)
  (cond
    [(empty? lls) '()]
    [else (cons (encode-line (first lls)) (encode-lls (rest lls)))]))

; File -> File
; encodes text files numerically
(define (numeric-file file)
  (write-file (string-append "numeric-file-" file)
              (collapse (encode-lls (read-words/line file)))))
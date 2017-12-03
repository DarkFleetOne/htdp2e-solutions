;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname dictionary-count) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t quasiquote mixed-fraction #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; A Dictionary is a List-of-strings.
; Linux: /usr/share/dict/words or /var/lib/dict/words
; Windows: no inbuilt dictionary, used txt file from GitHub
(define DICTIONARY-AS-LIST
  (read-lines "C:\\Users\\Will\\Desktop\\notes\\english-words-master\\words.txt"))

(define TEST-DICT (list "alpha" "apple" "bravo" "charlie" "chi" "chrono"))

; A Letter is one of the following 1Strings:
; - "a"
; - ...
; - "z"
; or, equivalently, a member? of this list:
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

; Letter String -> Boolean
; Determines if the first letter in the given String
; is before the given letter

(check-expect (first-letter-before? "a" "beta") #true)
(check-expect (first-letter-before? "c" "beta") #false)

(define (first-letter-before? ltr str)
  (cond
    [(string<? ltr (first (explode str))) #true]
    [else #false]))

; Letter String -> Boolean
; Determines if the first letter in the given String
; is after the given letter

(check-expect (first-letter-after? "a" "beta") #false)
(check-expect (first-letter-after? "c" "beta") #true)

(define (first-letter-after? ltr str)
  (cond
    [(string>? ltr (first (explode str))) #true]
    [else #false]))

; Letter String -> Boolean
; Determines if the first letter in the given String
; matches the given letter

(check-expect (first-letter=? "h" "hello") #true)
(check-expect (first-letter=? "g" "world") #false)

(define (first-letter=? ltr str)
  (cond
    [(string=? ltr (first (explode str))) #true]
    [else #false]))

; Letter Dictionary -> Number
; Determines how many words in the given Dictionary
; start with the given Letter

(check-expect (starts-with# "a" TEST-DICT) 2)
(check-expect (starts-with# "c" TEST-DICT) 3)
(check-expect (starts-with# "d" TEST-DICT) 0)

(define (starts-with# ltr dict)
  (cond
    [(empty? dict) 0]
    [(first-letter=? ltr (first dict)) (+ 1 (starts-with# ltr (rest dict)))]
    [else (starts-with# ltr (rest dict))]))

; A Letter-Count is:
; (list Letter Number)
; where the number is the instances of letter counted

; Letter Dictionary -> Letter-Count
; creates a letter-count for the number of words in Dictionary
; that start with the given letter

(check-expect (letter-count "a" TEST-DICT) (list "a" 2))
(check-expect (letter-count "d" TEST-DICT) (list "d" 0))

(define (letter-count ltr dict)
  (list ltr (starts-with# ltr dict)))

; List-of-letters Dictionary -> List-of-letter-counts
; counts how often each letter is used as the first one of a word
; in the given dictionary, returning an Lolc

(check-expect (count-by-letter (explode "abcd") TEST-DICT)
              (list
               (list "a" 2)
               (list "b" 1)
               (list "c" 3)
               (list "d" 0)))

(define (count-by-letter lol dict)
  (cond
    [(empty? lol) '()]
    [else (cons (letter-count (first lol) dict) (count-by-letter (rest lol) dict))]))

; letter-count List-of-letter-counts -> List-of-letter-counts
; inserts letter-count into the sorted List-of-letter-counts

(check-expect (insert-letter-count (list "a" 2) (list (list "c" 3) (list "b" 1)))
              (list (list "c" 3) (list "a" 2) (list "b" 1)))

(define (insert-letter-count lc lolc)
  (cond
    [(empty? lolc) (cons lc '())]
    [else (if (>= (second lc) (second (first lolc)))
              (cons lc lolc)
              (cons (first lolc) (insert-letter-count lc (rest lolc))))]))

; List-of-letter-counts -> List-of-letter-counts
; Returns a List-of-letter-counts sorted highest to lowest letter

(check-expect (sort-letter-count (list (list "a" 1) (list "b" 3) (list "c" 2) (list "d" 4)))
              (list (list "d" 4) (list "b" 3) (list "c" 2) (list "a" 1)))

(define (sort-letter-count lolc)
  (cond
    [(empty? lolc) '()]
    [(cons? lolc)
     (insert-letter-count (first lolc) (sort-letter-count (rest lolc)))]))

; Dictionary -> Letter-Count
; returns the letter-count of the letter that
; has the highest number of occurences in Dictionary
(define (most-frequent dict)
  (first (sort-letter-count (count-by-letter LETTERS dict))))

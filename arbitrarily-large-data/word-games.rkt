;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname word-games) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t quasiquote mixed-fraction #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; A Word is one of:
; - '() or
; - (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)

; A List-of-words is one of:
; - '() or
; - (cons Word List-of-Words)
; interpretation a List-of-words is a list of lists of 1Strings (letters)

; A Dictionary is a List-of-strings.
; Linux: /usr/share/dict/words or /var/lib/dict/words
; Windows: no inbuilt dictionary, used txt file from GitHub
;(define DICTIONARY-AS-LIST
;  (read-lines "C:\\Users\\Will\\Desktop\\notes\\english-words-master\\words.txt"))

(define TEST-DICT
  (list "alpha" "act" "apple" "art"
        "bravo"
        "cat" "charlie" "chi" "chrono"
        "kites"
        "moor"
        "rat"
        "room"
        "skite"
        "tar" "tikes"))

; String -> Word
; converts s to the chosen word representation
(define (string->word s)
  (explode s))

; Word -> String
; converts w to a string
(define (word->string w)
  (implode w))

; List-of-words -> List-of-strings
; turns all Words in low into Strings
(define (words->strings low)
  (cond
    [(empty? low) '()]
    [else (cons (word->string (first low)) (words->strings (rest low)))]))

; List-of-strings -> List-of-strings
; picks out all those Strings that occur in the dictionary

(check-expect (in-dictionary (list "cat" "tac" "act" "cta" "tca" "atc"))
              (list "cat" "act"))

(define (in-dictionary los)
  (cond
    [(empty? los) '()]
    [(member? (first los) TEST-DICT)
     (cons (first los) (in-dictionary (rest los)))]
    [else (in-dictionary (rest los))]))

; 1String Word -> Word
; inserts 1String at end of Word

(check-expect (insert-last "t" '("c" "a"))
              '("c" "a" "t"))

(define (insert-last ltr word)
  (reverse (cons ltr (reverse word))))

; 1String Word -> List-of-words
; inserts 1String between each letter, returning a string
; for each insertion

(define (insert-middle ltr word)
  (cond
    [(empty? (rest word)) word]
    [else (cons (first word) (cons ltr (insert-middle ltr (rest word))))]))

; 1String Word -> Word
; inserts 1String at beginning of Word

(check-expect (insert-first "h" '("o" "u" "s" "e"))
              '("h" "o" "u" "s" "e"))

(define (insert-first ltr word)
  (cons ltr word))


; 1String Word -> List-of-words
; Inserts 1String into Word, at beginning, middle, and end

(check-expect (insert-everywhere "d" (list "e"))
              (cons (list "d" "e")
                    (cons (list "e" "d")
                          '())))

(define (insert-everywhere ltr word)
  (cond
    [(empty? word) (list (insert-first ltr word))]
    [(= (length word) 1) (list (list ltr (first word))
                               (list (first word) ltr))]
    [else (list (insert-first ltr word)
                (insert-middle ltr word)
                (insert-last ltr word))]))

; 1String List-of-words -> List-of-words
; creates a list of words like the second argument,
; but with the first argument inserted at the beginning,
; between all letters, and at the end of all words

(check-expect (insert-everywhere/in-all-words "d" (list '()))
              (cons (list "d") '()))

(check-expect (insert-everywhere/in-all-words "d" (cons (list "e") '()))
              (cons (list "d" "e")
                    (cons (list "e" "d")
                          '())))

(check-expect (insert-everywhere/in-all-words "d"
                                              (cons (list "e" "r")
                                                    (cons (list "r" "e")
                                                          '())))
              (cons (list "d" "e" "r")
                    (cons (list "e" "d" "r")
                          (cons (list "e" "r" "d")
                                (cons (list "d" "r" "e")
                                      (cons (list "r" "d" "e")
                                            (cons (list "r" "e" "d")
                                                  '())))))))

(define (insert-everywhere/in-all-words ltr low)
  (cond
    [(empty? low) '()]
    [else (append (insert-everywhere ltr (first low))
                  (insert-everywhere/in-all-words ltr (rest low)))]))

; Word -> List-of-words
; finds all rearrangements of the letters in w
(define (arrangements word)
  (cond
    [(empty? word) (list '())]
    [else (insert-everywhere/in-all-words (first word)
                                          (arrangements (rest word)))]))

; String -> List-of-strings
; finds all words that use the same letters as s

(check-member-of (alternative-words "cat")
                 (list "act" "cat")
                 (list "cat" "act"))


; List-of-strings -> Boolean
(define (all-words-from-rat? w)
  (and (member? "rat" w)
       (member? "art" w)
       (member? "tar" w)))

(check-satisfied (alternative-words "rat")
                 all-words-from-rat?)

(define (alternative-words s)
  (in-dictionary
   (words->strings (arrangements (string->word s)))))

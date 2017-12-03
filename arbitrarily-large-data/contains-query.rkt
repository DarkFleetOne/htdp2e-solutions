;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname contains-query) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-names is one of:
; - '()
; - (cons String List-of-names)
; interpretation a list of contacts, by last name

(define name-test (list "Fagan" "Findler" "Fisler" "Flanagan" "Flatt" "Felleisen" "Friedman"))

; List-of-names -> Boolean
; determines whether "Flatt" is on a-list-of-names (alon)
(check-expect (contains-flatt? '()) #false)
(check-expect (contains-flatt? (cons "Find" '())) #false)
(check-expect (contains-flatt? (cons "Flatt" '())) #true)
(check-expect
 (contains-flatt?
  (cons "A" (cons "Flatt" (cons "C" '()))))
 #true)
(check-expect
 (contains-flatt?
  (cons "A" (cons "B" (cons "C" '()))))
 #false)
(define (contains-flatt? alon)
  (cond
    [(empty? alon) #false]
    [(cons? alon)
     (or (string=? (first alon) "Flatt")
         (contains-flatt? (rest alon)))]))

; List-of-names -> Boolean
; determines whether a given String (s) is present in a-list-of-names (alon)
(check-expect (contains? "A" name-test) #false)
(check-expect (contains? "Flatt" name-test) #true)
(check-expect (contains? "" name-test) #false)
(check-error (contains? name-test "Flatt") "Expected list as second argument.")
(check-error (contains? '() name-test) "Expected string as first argument.")
(define (contains? s alon)
  (cond
    [(empty? alon) #false]
    [(cons? alon)
     (cond
       [(not (string? s)) (error "Expected string as first argument.")]
       [(string=? (first alon) s) #true]
       [else (contains? s (rest alon))])]
    [else (error "Expected list as second argument.")]))

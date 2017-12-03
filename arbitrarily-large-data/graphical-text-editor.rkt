;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname graphical-text-editor) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t quasiquote mixed-fraction #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define HEIGHT 20)
(define WIDTH 200)
(define FONT-SIZE 16)
(define FONT-COLOR "black")
(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

(define-struct editor [pre post])
; An Edtor is a structure:
;    (make-editor Lo1S Lo1S)
; An Lo1S is one of:
; - '()
; - (cons 1String Lo1S)

(define good
  (cons "g" (cons "o" (cons "o" (cons "d" '())))))
(define all
  (cons "a" (cons "l" (cons "l" '()))))
(define lla
  (cons "l" (cons "l" (cons "a" '()))))

; data example 1:
(define editor0 (make-editor all good))
; data example 2:
(define editor1 (make-editor lla good))

; Lo1s 1String -> Lo1s
; creates a new list by adding s to the end of l

(check-expect
 (add-at-end (cons "c" (cons "b" '())) "a")
 (cons "c" (cons "b" (cons "a" '()))))

(define (add-at-end l s)
  (cond
    [(empty? l) (cons s '())]
    [else
     (cons (first l) (add-at-end (rest l) s))]))

; Lo1s -> Lo1s
; produces a reverse version of the given list

(check-expect
 (rev (cons "a" (cons "b" (cons "c" '()))))
 (cons "c" (cons "b" (cons "a" '()))))

(define (rev l)
  (cond
    [(empty? l) '()]
    [else (add-at-end (rev (rest l)) (first l))]))

; String String -> Editor
(define (create-editor s1 s2)
  (make-editor (rev (explode s1)) (explode s2)))

; Lo1s -> Image
; renders a list of 1Strings as a text image
(check-expect
 (editor-text
  (cons "p" (cons "o" (cons "s" (cons "t" '())))))
 (text "post" FONT-SIZE FONT-COLOR))

(define (editor-text s)
  (text (implode s) FONT-SIZE FONT-COLOR))

; Editor -> Image
; renders an editor as an image of the two texts
; separated by the cursor
(define editor-image0
  (place-image/align
   (beside (text "pre" FONT-SIZE FONT-COLOR)
           CURSOR
           (text "post" FONT-SIZE FONT-COLOR))
   1 1
   "left" "top"
   MT))
(check-expect (editor-render (create-editor "pre" "post")) editor-image0)

(define (editor-render e)
  (place-image/align
   (beside (editor-text (reverse (editor-pre e)))
           CURSOR
           (editor-text (editor-post e)))
   1 1
   "left" "top"
   MT))

; Editor -> Editor
; moves the cursor position one 1String left
; if possible
(define (editor-lft ed)
  (cond
    [(empty? (editor-pre ed)) ed]
    [else (make-editor (rest (editor-pre ed))
                       (cons (first (editor-pre ed)) (editor-post ed)))]))

; Editor -> Editor
; moves the cursor position one 1String right,
; if possible
(define (editor-rgt ed)
  (cond
    [(empty? (editor-post ed)) ed]
    [else (make-editor (cons (first (editor-post ed)) (editor-pre ed))
                       (rest (editor-post ed)))]))

; Editor -> Editor
; deletes a 1String to the left of the cursor,
; if possible
(define (editor-del ed)
  (cond
    [(empty? (editor-pre ed)) ed]
    [else (make-editor (rest (editor-pre ed)) (editor-post ed))]))

; Editor 1String -> Editor
; insert the 1String k between pre and post
(check-expect (editor-ins (make-editor '() '()) "e")
              (make-editor (cons "e" '()) '()))
(check-expect (editor-ins (make-editor (cons "d" '())
                                       (cons "f" (cons "g" '())))
                          "e")
              (make-editor (cons "e" (cons "d" '()))
                           (cons "f" (cons "g" '()))))

(define (editor-ins ed k)
  (make-editor (cons k (editor-pre ed))
               (editor-post ed)))

; Editor KeyEvent -> Editor
; deals with a key event, given some editor
(check-expect (editor-kh (create-editor "" "") "e")
              (create-editor "e" ""))
(check-expect (editor-kh (create-editor "cd" "fgh") "e")
              (create-editor "cde" "fgh"))
(check-expect (editor-kh (create-editor "cde" "fgh") "\b")
              (create-editor "cd" "fgh"))
(check-expect (editor-kh (create-editor "abc" "def") "left")
              (create-editor "ab" "cdef"))

(define (editor-kh ed ke)
  (cond
    [(key=? ke "left") (editor-lft ed)]
    [(key=? ke "right") (editor-rgt ed)]
    [(key=? ke "\b") (editor-del ed)]
    [(key=? ke "\t") ed]
    [(key=? ke "\r") ed]
    [(= (string-length ke) 1) (editor-ins ed ke)]
    [else ed]))

; main : String -> Editor
; launches the editor given some initial string

(define (main s)
  (big-bang (create-editor s "")
    (on-key editor-kh)
    (to-draw editor-render)))
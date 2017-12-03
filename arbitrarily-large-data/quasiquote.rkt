;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname quasiquote) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t quasiquote mixed-fraction #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))

(require web-io)

; String String -> ... deeply nested list ...
; produces a web page with given author and title
(define (my-first-web-page author title)
  `(html
     (head
       (title ,title)
       (meta ((http-equiv "content-type")
              (content "text-html"))))
     (body
       (h1 ,title)
       (p "I, " ,author ", made this page."))))

; Number -> ... nested list ...
; creates a cell for an HTML table from a number 
(define (make-cell n)
  `(td ,n))

; List-of-numbers -> ... nested list ...
; creates a row for an HTML table from l
(define (make-row l)
  (cond
    [(empty? l) '()]
    [else (cons (make-cell (first l))
                (make-row (rest l)))]))

; List-of-numbers List-of-numbers -> ... nested list ...
; creates an HTML table from two lists of numbers
(define (make-table row1 row2)
  `(table ((border "1"))
          (tr ,@(make-row row1))
          (tr ,@(make-row row2))))

(check-expect `(0 ,@'(1 2 3) 4)
              (list 0 1 2 3 4))

(check-expect `(("alan" ,(* 2 500))
                ("barb" 2000)
                (,@'("carl" " , the great")   1500)
                ("dawn" 2300))
              (list (list "alan" 1000)
                    (list "barb" 2000)
                    (list "carl" " , the great" 1500)
                    (list "dawn" 2300)))

(check-expect `(html
                (body
                 (table ((border "1"))
                        (tr ((width "200"))
                            ,@(make-row '( 1 2)))
                        (tr ((width "200"))
                            ,@(make-row '(99 65))))))
              (list 'html
                    (list 'body
                          (list 'table (list (list 'border "1"))
                                (list 'tr (list (list 'width "200"))
                                                (list 'td "1") (list 'td "2"))
                                (list 'tr (list (list 'width "200"))
                                                (list 'td "99") (list 'td "65"))))))

(define one-list
  '("Asia: Heat of the Moment"
    "Jay Z: 99 Problems"
    "We are: what we are"))

; List-of-strings -> List-of-ranks
; adds numerical ranks to a list-of-strings

(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los))
                (add-ranks (rest los)))]))

; List-of-strings -> List-of-strings
; converts a list-of-strings into ranked rows
; for an HTML table

(define (ranking los)
  (reverse (add-ranks (reverse los))))

; List-of-strings -> ...deeply nested list...
; Formats a list of strings into a table ranked by index

(define (make-ranking los)
  `(table ((border "1"))
          (tr ,@(make-row (first los)))
          (tr ,@(make-row (second los)))
          (tr ,@(make-row (third los)))))
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")
((modname test)
 (read-case-sensitive #t)
 (teachpacks ((lib "image.rkt" "teachpack" "2htdp")
              (lib "universe.rkt" "teachpack" "2htdp")
              (lib "batch-io.rkt" "teachpack" "2htdp")
              (lib "abstraction.rkt" "teachpack" "2htdp")))
 (htdp-settings #(#t constructor repeating-decimal #f #t none #f
                  ((lib "image.rkt" "teachpack" "2htdp")
                   (lib "universe.rkt" "teachpack" "2htdp")
                   (lib "batch-io.rkt" "teachpack" "2htdp")
                   (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))


; [List-of X] [List-of Y] -> [List-of [List X Y]]
; generates all pairs of items from l1 and l2

(check-satisfied (cross '(a b c) '(1 2))
                 (λ (c) (= (length c) 6)))

(define (cross l1 l2)
  (for*/list ([x1 l1] [x2 l2])
    (list x1 x2)))


; [List-of X] -> [List-of [List-of X]]
; creates a list of all rearrangements of the items in w

(define (arrangements w)
  (cond
    [(empty? w) '(())]
    [else (for*/list ([item w]
                      [arrangement-without-item
                       (arrangements (remove item w))])
            (cons item arrangement-without-item))]))

; [List-of X] -> Boolean
(define (all-words-from-rat? w)
  (and (member? (explode "rat") w)
       (member? (explode "art") w)
       (member? (explode "tar") w)))

(check-satisfied (arrangements '("r" "a" "t"))
                 all-words-from-rat?)


; Simplified Enumeration
(define (enumerate lx)
  (for/list ([item lx] [ith (in-naturals 1)])
    (list ith item)))


; N -> Number
; adds the even numbers between 0 and n (exclusive)
(check-expect (sum-evens 2) 0)
(check-expect (sum-evens 4) 2)

(define (sum-evens n)
  (for/sum ([i (in-range 0 n 2)]) i))


; A [Non-empty-list X] is one of:
; - (cons X '())
; - (cons X [Non-empty-list X])

; [Non-empty-list X] -> X
; retrieves the last item of ne-l

(check-expect (last-item '(a b c)) 'c)
(check-error (last-item '()))

(define (last-item ne-l)
  (match ne-l
    [(cons lst '()) lst]
    [(cons fst rst) (last-item rst)]))


(define-struct layer [color doll])
; An RD (short for Russian doll) is one of:
; - "doll"
; - (make-layer String RD)

; RD -> N
; how many dolls are a part of an-rd

(check-expect (depth (make-layer "red" "doll")) 1)

(define (depth a-doll)
  (match a-doll
    ["doll" 0]
    [(layer c inside) (+ (depth inside) 1)]))


; [List-of Posn] -> [List-of Posn]
; moves each object right by delta-x pixels

(check-expect (move-right `(,(make-posn 1 1) ,(make-posn 10 14)) 3)
              `(,(make-posn 4 1) ,(make-posn 13 14)))

(define (move-right lop delta-x)
  (for/list ((p lop))
    (match p [(posn x y) (make-posn (+ x delta-x) y)])))

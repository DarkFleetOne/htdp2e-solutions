


#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")
((modname stock-alert)
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

; CASE 1 - Atomic Treatment

; [List-of Number] [List-of Number] -> [List-of Number]
; replaces the final '() in front with end

(check-expect (replace-eol-with '() '(a b)) '(a b))
(check-expect (replace-eol-with (cons 1 '()) '(a)) (cons 1 '(a)))
(check-expect (replace-eol-with (cons 2 (cons 1 '())) '(a)) (cons 2 (cons 1 '(a))))

(define (replace-eol-with front end)
  (cond
    [(empty? front) end]
    [else (cons (first front)
                (replace-eol-with (rest front) end))]))


; CASE 2 - Lockstep Treatment

; [List-of Number] [List-of Number] -> [List-of Number]
; multiplies the corresponding items on
; hours and wages/h
; assume the two lists are of equal length

(check-expect (wages* '() '()) '())
(check-expect (wages* (list 5.65) (list 40)) (list 226.0))
(check-expect (wages* '(5.65 8.75) '(40.0 30.0)) '(226.0 262.5))

(define (wages* hours wages/h)
  (local (; Number Number -> Number
          ; computes the weekly wage from pay-rate and hours
          (define (weekly-wage pay-rate hours)
            (* pay-rate hours)))
    (cond
      [(empty? hours) '()]
      [else (cons (weekly-wage (first hours) (first wages/h))
                  (wages* (rest hours) (rest wages/h)))])))


; CASE 3 - All possible cases

; N is one of:
; - 0
; - (add1 N)

; [List-of Symbol] N -> Symbol
; extracts the nth symbol from l;
; signals an error if there is no symbol

(check-expect (list-pick '(a b c) 2) 'c)
(check-error (list-pick '() 0) "list too short")
(check-expect (list-pick (cons 'a '()) 0) 'a)
(check-error (list-pick '() 3) "list too short")
(check-expect (list-pick '(a b) 1) 'b)

(define (list-pick l n)
  (cond
    [(and (= n 0) (empty? l)) (error "list too short")]
    [(and (> n 0) (empty? l)) (error "list too short")]
    [(and (= n 0) (cons? l)) (first l)]
    [(and (> n 0) (cons? l)) (list-pick (rest l) (sub1 n))]))

(define (list-pick.v2 l n)
  (cond
    [(empty? l) (error "list too short")]
    [(= n 0) (first l)]
    [(> n 0) (list-pick (rest l) (sub1 n))]))

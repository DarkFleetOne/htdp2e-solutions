

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


; An S-expr is one of:
; - Atom
; - SL

; An Atom is one of:
; - Number
; - String
; - Symbol

; An SL is one of:
; - '()
; - (cons S-expr SL)

; S-expr -> Boolean
; determines if the given S-expression
; is an Atom

(define (atom? sexp)
  (or (number? sexp)
      (string? sexp)
      (symbol? sexp)))


; S-expr Symbol -> N
; counts all occurances of sy in sexp

(check-expect (count 'world 'world) 1)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)

; S-expr Symbol -> N
; counts all occurences of sy in sexp
(define (count.v1 sexp sy)
  (cond
    [(atom? sexp) (count-atom sexp sy)]
    [else (count-sl sexp sy)]))

; SL Symbol -> N
; counts all occurences of sy in sl
(define (count-sl sl sy)
  (cond
    [(empty? sl) 0]
    [else (+ (count.v1 (first sl) sy)
             (count-sl (rest sl) sy))]))

; Atom Symbol -> N
; counts all occurences of sy in at
(define (count-atom at sy)
  (cond
    [(number? at) 0]
    [(string? at) 0]
    [(symbol? at) (if (symbol=? at sy) 1 0)]))

; S-expr Symbol -> N
; counts all occurences of sy in sexp
(define (count sexp sy)
  (local (; SL Symbol -> N
          ; counts all occurences of sy in sl
          (define (count-sl sl)
            (cond
              [(empty? sl) 0]
              [else (+ (count (first sl) sy)
                       (count-sl (rest sl)))]))
          ; Atom Symbol -> N
          ; counts all occurences of sy in at
          (define (count-atom at)
            (cond
              [(number? at) 0]
              [(string? at) 0]
              [(symbol? at) (if (symbol=? at sy) 1 0)])))
    (cond
      [(atom? sexp) (count-atom sexp)]
      [else (count-sl sexp)])))


; S-expr -> N
; consumes an sexp and determines its depth
; with an atom having a depth of 1 and the
; depth of a list-of sexp is the maximum depth
; of its items plus 1

(check-expect (depth 'hello) 1)
(check-expect (depth '(world hello)) 2)
(check-expect (depth '(world '(hello world))) 4)

(define (depth sexp)
  (local (; Atom -> N
          ; determines the depth of an atom
          (define atom-depth 1)
          (define (depth-sl sl)
            (cond
              [(empty? sl) 0]
              [else (+ (depth (first sl))
                       (depth-sl (rest sl)))])))
    (cond
      [(atom? sexp) atom-depth]
      [else (depth-sl sexp)])))


; S-expr Symbol Atom -> S-expr
; replaces all occurences of old in sexp with new

(check-expect (substitute '(((world) bye) bye) 'bye '42)
              '(((world) 42) 42))

(define (substitute sexp old new)
  (local (; S-expr -> S-expr
          (define (for-sexp sexp)
            (cond
              [(atom? sexp) (for-atom sexp)]
              [else (for-sl sexp)]))
          ; SL -> S-expr
          (define (for-sl sl)
            (cond
              [(empty? sl) '()]
              [else (cons (for-sexp (first sl))
                          (for-sl (rest sl)))]))
          ; Atom -> S-expr
          (define (for-atom at)
            (cond
              [(number? at) at]
              [(string? at) at]
              [(symbol? at) (if (equal? at old) new at)])))
    (for-sexp sexp)))

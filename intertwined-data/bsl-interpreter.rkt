


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

; A BSL-expr is one of:
; - Number
; - (make-add BSL-var-expr BSL-var-expr)
; - (make-mul BSL-var-expr BSL-var-expr)

; A BSL-var-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-var-expr BSL-var-expr)
; - (make-mul BSL-var-expr BSL-var-expr)

(define-struct add [left right])
(define-struct mul [left right])

; A Boolean-expr is a BSL-expr:
(define-struct aand [left right])
(define-struct oorr [left right])
(define-struct nnot [bool])

; A Value is:
; - Number
; - #true
; - #false
; - BSL-var-expr

; Value -> Boolean
; determines if the given value is an atom
(define (atom? s)
  (or (number? s)
      (string? s)
      (symbol? s)
      (boolean? s)))

; BSL-expr -> Value
; consumes a representation of a BSL
; expression, computes its value

(check-expect (eval-num-expression 3) 3)
(check-expect (eval-num-expression (make-add 1 1)) 2)
(check-expect (eval-num-expression (make-mul 3 10)) 30)
(check-expect (eval-num-expression (make-add (make-mul 1 1) 10)) 11)

(define (eval-num-expression bexp)
  (cond
    [(add? bexp) (+ (eval-num-expression (add-left bexp)) (eval-num-expression (add-right bexp)))]
    [(mul? bexp) (* (eval-num-expression (mul-left bexp)) (eval-num-expression (mul-right bexp)))]
    [else bexp]))

; BSL-expr -> Value
; consumes a Boolean BSL representation
; computes its value as #true or #false

(check-expect (eval-bool-expression (make-aand #true #true)) #true)
(check-expect (eval-bool-expression (make-aand #true #false)) #false)
(check-expect (eval-bool-expression (make-aand #false #false)) #false)
(check-expect (eval-bool-expression (make-oorr #true #true)) #true)
(check-expect (eval-bool-expression (make-oorr #true #false)) #true)
(check-expect (eval-bool-expression (make-oorr #false #false)) #false)
(check-expect (eval-bool-expression (make-nnot #true)) #false)
(check-expect (eval-bool-expression (make-nnot #false)) #true)

(define (eval-bool-expression bexp)
  (cond
    [(aand? bexp) (and (eval-bool-expression (aand-left bexp))
                       (eval-bool-expression (aand-right bexp)))]
    [(oorr? bexp) (or (eval-bool-expression (oorr-left bexp))
                      (eval-bool-expression (oorr-right bexp)))]
    [(nnot? bexp) (not (eval-bool-expression (nnot-bool bexp)))]
    [else bexp]))

; S-expr -> BSL-expr
; consumes an S-expr and produces a
; BSL-expr iff a valid representation exists

(check-expect (parse 2) 2)
(check-expect (parse #true) #t)
(check-expect (parse '(+ 2 2)) (make-add 2 2))
(check-expect (parse '(* 2 2)) (make-mul 2 2))
(check-expect (parse '(and #true #false)) (make-aand #t #f))
(check-expect (parse '(or #true #false)) (make-oorr #t #f))
(check-expect (parse '(not #true)) (make-nnot #t))

(define WRONG "error!")

(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))

; SL -> BSL-expr
(define (parse-sl s)
  (local ((define L (length s)))
    (cond
      [(< L 2) (error "too short")]
      [(and (= L 2) (symbol? (first s)))
       (cond
         [(symbol=? (first s) 'not)
          (make-nnot (parse (second s)))]
         [else (error "arity-1 function not found")])]
      [(and (= L 3) (symbol? (first s)))
       (cond
         [(symbol=? (first s) '+)
          (make-add (parse (second s)) (parse (third s)))]
         [(symbol=? (first s) '*)
          (make-mul (parse (second s)) (parse (third s)))]
         [(symbol=? (first s) 'and)
          (make-aand (parse (second s)) (parse (third s)))]
         [(symbol=? (first s) 'or)
          (make-oorr (parse (second s)) (parse (third s)))]
         [else (error "arity-2 function not found")])]
      [else (error "invalid symbolic list")])))

; Atom -> BSL-expr
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(boolean? s) s]
    [(string? s) (error "strings not yet implemented")]
    [(symbol? s) s]))

; S-expr -> Value
; accepts S-expressions
; if recognized by parse, produce their value
; else signal error raised by parse

(check-expect (interpreter-expr 2) 2)
(check-expect (interpreter-expr #t) #t)
(check-expect (interpreter-expr '(+ 2 2)) 4)
(check-expect (interpreter-expr '(* 2 3)) 6)
(check-expect (interpreter-expr '(not #t)) #f)
(check-expect (interpreter-expr '(and #t #f)) #f)
(check-expect (interpreter-expr '(or #t #f)) #t)
(check-error (interpreter-expr '("hello")) "too short")

(define (interpreter-expr sexp)
  (local ((define bexp (parse sexp)))
    (cond
      [(add? bexp)
       (+ (eval-num-expression (add-left bexp)) (eval-num-expression (add-right bexp)))]
      [(mul? bexp)
       (* (eval-num-expression (mul-left bexp)) (eval-num-expression (mul-right bexp)))]
      [(nnot? bexp)
       (not (eval-bool-expression (nnot-bool bexp)))]
      [(aand? bexp)
       (and (eval-bool-expression (aand-left bexp))
            (eval-bool-expression (aand-right bexp)))]
      [(oorr? bexp)
       (or (eval-bool-expression (oorr-left bexp))
           (eval-bool-expression (oorr-right bexp)))]
      [else bexp])))

; BSL-var-expr Symbol Number -> BSL-var-expr
; creates a BSL-var-expr like the original,
; but with Symbol replaced by Number

(check-expect (subst '(+ x 3) 'x 3) '(+ 3 3))
(check-expect (subst '(* 1/2 (* x 3)) 'x 3) '(* 0.5 (* 3 3)))

(define (subst ex x v)
  (cond
    [(empty? ex) '()]
    [(and (symbol? (first ex))
          (symbol=? x (first ex)))
     (cons v (subst (rest ex) x v))]
    [(cons? (first ex))
     (cons (subst (first ex) x v) (subst (rest ex) x v))]
    [else (cons (first ex) (subst (rest ex) x v))]))

; BSL-var-expr -> Boolean
; Determines whether a BSL-var-expr
; is also a BSL-expr

(check-expect (numeric? '(+ 2 2)) #t)
(check-expect (numeric? '(* 1/2 (* 3 3))) #t)
(check-expect (numeric? '(* 1/2 (* x 3))) #f)

(define (numeric? expr)
  (cond
    [(empty? expr) #t]
    [(cons? (first expr)) (and (numeric? (first expr)) (numeric? (rest expr)))]
    [(or (number? (first expr))
         (symbol=? '+ (first expr))
         (symbol=? '* (first expr)))
     (numeric? (rest expr))]
    [else #f]))

; BSL-var-expr -> Value
; Determines BSL-var-expr value
; if numeric? yields true

(check-expect (eval-variable '(+ 2 2)) 4)
(check-expect (eval-variable '(* 1/2 (* 3 3))) 4.5)
(check-error (eval-variable '(* 1/2 (* x 3))) "no eval for type")

(define (eval-variable expr)
  (cond
    [(empty? expr) '()]
    [(numeric? expr) (eval-num-expression (parse expr))]
    [else (error "no eval for type")]))

; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
;  (cons Symbol (cons Value '())).

(define al1 '((x 3) (y 2) (z 1)))
(define al2 '((k (+ 1 1))))
(define al3 '((* 5 (k (+ 1 1)))))
(define al4 '((* (i 5) (k (+ 1 1)))))

; BSL-var-expr AL -> Value
; replaces variables in expression with their defined values
; returns the value of the given expression

(check-expect (eval-variable* '(* x (+ y z)) al1) 9)

(define (eval-variable* expr da)
  (local ((define (subst* expr da)
            (cond
              [(empty? da) expr]
              [else (subst* (match (first da)
                              [(list key value) (subst expr key value)]) (rest da))]))
          (define bexp (subst* expr da)))
    (eval-variable bexp)))

; BSL-var-expr AL -> Value

(check-expect (eval-var-lookup '(* x (+ y z)) al1) 9)

(define (eval-var-lookup expr da)
  (local ((define bexp (parse expr))
          (define (lookup expr da)
            (if (symbol? expr)
                (second (assq expr da))
                expr))
          (define (eval expr)
            (cond
              [(add? expr) (+ (eval (lookup (add-left expr) da))
                              (eval (lookup (add-right expr) da)))]
              [(mul? expr) (* (eval (lookup (mul-left expr) da))
                              (eval (lookup (mul-right expr) da)))]
              [else expr])))
    (eval bexp)))

; BSL-fun-expr Symbol Symbol BSL-fun-expr
; determines the value of ex
; applies f substituting x in b
;; Example eval-definition1 from HtDP
;; (define (eval-definition1 ex f x b)
;;   (local ((define value (eval-definition1 arg f x b))
;;           (define plugd (subst b x arg-value)))
;;     (eval-definition1 plugd f x b)))

; A BSL-fun-def is a structure:
(define-struct fun [name param body])
;  (make-fun Symbol Symbol BSL-var-expr)
; interpretation a definition of an arity-1 function

(define fun1 (make-fun 'f 'x '(+ 3 x)))
(define fun2 (make-fun 'g 'y '(* 2 y)))
(define fun3 (make-fun 'h 'v '(+ (f v) (g v))))

; A BSL-fun-def* is a List-of BSL-fun-def
; (list BSL-fun-def BSL-fun-def2 ...)
;  interpretation a definitions area

(define da-fgh (list fun1 fun2 fun3))

; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da
; signals an error if there is none

(check-expect (lookup-def da-fgh 'g) '(* 2 y))

(define (lookup-def da f)
  (cond
    [(empty? da) (error "definition not found")]
    [(symbol=? f (fun-name (first da))) (fun-body (first da))]
    [else (lookup-def (rest da) f)]))

(define (lookup-param da f)
  (cond
    [(empty? da) (error "definition not found")]
    [(symbol=? f (fun-name (first da))) (fun-param (first da))]
    [else (lookup-def (rest da) f)]))

; BSL-fun-expr BSL-fun-def* -> Value

(check-expect (eval-function* '(g 2) da-fgh) 4)

(define (eval-function* expr dl)
  (local ((define value (eval-num-expression (second expr)))
          (define body (lookup-def dl (first expr)))
          (define param (lookup-param dl (first expr)))
          (define plugd (subst body param value)))
    (eval-function* plugd dl)))

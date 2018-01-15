


#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")
((modname FSM-XML)
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

; An Xexpr is one of:
; - Symbol
; - String
; - Number
; - (cons Symbol (cons Attribute* [List-of Xexpr]))
; - (cons Symbol [List-of Xexpr])

; An Attribute* is a [List-of Attribute].

; An Attribute is a list of two items:
;   (list Symbol String)

; [List-of Attribute] or Xexpr -> Boolean
; is x a list of attributes

(define (list-of-attributes? x)
  (cond
    [(empty? x) #t]
    [else (local ((define possible-attribute (first x)))
            (cons? possible-attribute))]))

; Xexpr -> [List-of Attribute]
; retrieves the list of attributes of xe

(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else (local ((define loa-or-x
                      (first optional-loa+content)))
              (if (list-of-attributes? loa-or-x)
                  loa-or-x
                  '()))])))

; Xexpr -> Symbol
; retrieves the name of xe

(define (xexpr-name xe)
  (first xe))

; Xexpr -> [List-of Xexpr]
; retrieves the list of content of xe

(define (xexpr-content xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else (local ((define loa-or-content
                      (first optional-loa+content)))
              (if (list-of-attributes? loa-or-content)
                  (if (empty? loa-or-content)
                      (first (rest optional-loa+content))
                      (rest optional-loa+content))
                  optional-loa+content))])))

; [List-of Attribute] Symbol -> String
; searches loa for s, returns string associated with s

(define (find-attr loa s)
  (local ((define att (assq s loa)))
    (if (cons? att)
        (second att)
        att)))

; An FSM is a [List-of 1Transition]
; A 1Transition is a list of two items:
; - (cons FSM-State (cons FSM-State '()))
; - (cons key-event (cons FSM-State '()))
; An FSM-State is a String that specifies a color

; An XMachine is a nested list of this shape:
;   `(machine ((initial ,FSM-State)) [List-of X1T])
; An X1T is a nested list of this shape:
;   `(action ((state ,FSM-State) (next ,FSM-State)))

; data examples
(define fsm-traffic
  '(("red" "green") ("green" "yellow") ("yellow" "red")))
(define fsm-bw
  '(("black" "white") ("white" "black")))

; <machine initial="red">
;   <action state="red"    next="green" />
;   <action state="green"  next="yellow" />
;   <action state="yellow" next="red" />
(define xm0
  '(machine ((initial "red"))
            (action ((state "red") (next "green")))
            (action ((state "green") (next "yellow")))
            (action ((state "yellow") (next "red")))))
; <machine initial="black">
;   <action state="black" next="white" />
;   <action state="white" next="black" />
(define xm1
  '(machine ((initial "black"))
            (action ((state "black") (next "white")))
            (action ((state "white") (next "black")))))

; FSM FSM-State -> FSM-State
; matches the keys pressed by a player with the given FSM
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
            [to-draw (λ (current)
                       (overlay/align "middle" "bottom"
                                      (text current 24 (cond
                                                         [(string=? "black" current) "white"]
                                                         [else "black"]))
                                      (square 100 "solid" current)))]
            [on-key (λ (current key-event) (find transitions current))]))

; [X Y] [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in alist

(check-expect (find fsm-traffic "red") "green")
(check-expect (find fsm-traffic "green") "yellow")
(check-expect (find fsm-traffic "yellow") "red")

(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))

; XMachine -> FSM-State
; extracts and translates the transition table from xm0

(check-expect (xm-state0 xm0) "red")

(define (xm-state0 xm0)
  (find-attr (xexpr-attr xm0) 'initial))

; XMachine -> [List-of 1Transition]
; extracts & translates the transition table from xm

(check-expect (xm->transitions xm0) fsm-traffic)

(define (xm->transitions xm)
  (local (; X1T -> 1Transition
          (define (xaction->action xa)
            (list (find-attr (xexpr-attr xa) 'state)
                  (find-attr (xexpr-attr xa) 'next))))
    (map xaction->action (xexpr-content xm))))

; XMachine -> FSM-State
; simulates an FSM via the given configuration
(define (simulate-xmachine xm)
  (simulate (xm-state0 xm) (xm->transitions xm)))

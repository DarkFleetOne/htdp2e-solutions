;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname finite-state-machines) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; An FSM is one of:
; - '()
; - (cons Transition FSM)

(define-struct transition (current next))
; A Transition is a structure:
;  (make-transition FSM-State FSM-State)

; FSM-State is a Color.

; interpretation An FSM represents the transitions that
; a finite state machine can take from one state to another
; in reaction to keystrokes

; A SimulationState.v1 is an FSM-State

(define-struct fs (fsm current))
; A SimulationState.v2 is a structure:
;  (make-fs FSM FSM-State)

(define (state=? state1 state2)
  (string=? state1 state2))

(define fsm-traffic
  (list (make-transition "red" "green")
        (make-transition "green" "yellow")
        (make-transition "yellow" "red")))

(define bw-machine
  (list (make-transition "black" (make-transition "white" "black"))))

; SimulationState -> Image
; renders a world state as an image
(define (render-state.v1 s)
  empty-image)

; SimulationState.v1 KeyEvent -> SimulationState.v1
; finds the next state from ke and cs
(define (find-next-state.v1 cs ke)
  cs)

; SimulationState.v2 -> Image
; renders current world state as a colored square

(check-expect (state-as-colored-square.v1 (make-fs fsm-traffic "red"))
              (square 100 "solid" "red"))

(define (state-as-colored-square.v1 an-fsm)
  (cond
    [(transition? (fs-current an-fsm))
     (square 100 "solid" (transition-next (fs-current an-fsm)))]
    [else (square 100 "solid" (fs-current an-fsm))]))

; FSM FSM-State -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field

(check-expect (find.v1 fsm-traffic "red") "green")
(check-expect (find.v1 fsm-traffic "green") "yellow")
(check-error (find.v1 fsm-traffic "black")
             "not found: black")

(define (find.v1 transitions current)
  (cond
    [(empty? transitions) (error (string-append "not found: " current))]
    [else (if (string=? (transition-current (first transitions)) current)
              (transition-next (first transitions))
              (find.v1 (rest transitions) current))]))

; SimulationState.v2 KeyEvent -> SimulationState.v2
; finds the next state from ke and cs

(check-expect
 (find-next-state.v2 (make-fs fsm-traffic "red") "n")
 (make-fs fsm-traffic "green"))

(check-expect
 (find-next-state.v2 (make-fs fsm-traffic "red") "a")
 (make-fs fsm-traffic "green"))

(check-expect
 (find-next-state.v2 (make-fs fsm-traffic "green") "q")
 (make-fs fsm-traffic "yellow"))

(define (find-next-state.v2 an-fsm ke)
  (make-fs
   (fs-fsm an-fsm)
   (find.v1 (fs-fsm an-fsm) (fs-current an-fsm))))

; FSM -> ???
; match the keys pressed with the given FSM
;(define (simulate.v1 an-fsm)
;  (big-bang ;initial-state
;    (to-draw render-state.v1)
;    (on-key find-next-state.v1)))

; FSM FSM-State -> SimulationState.v2
; match the keys pressed with the given FSM
(define (simulate.v2 an-fsm s0)
  (big-bang (make-fs an-fsm s0)
    (to-draw state-as-colored-square.v1)
    (on-key find-next-state.v2)))



; Abstraction

; FSM-State -> Image
; renders current state as colored square
(define (state-as-colored-square s)
  (square 100 "solid" s))

; FSM FSM-State -> FSM-State
; finds the current state in fsm
(define (find transitions current)
  (cond
    [(empty? transitions) (error "not found")]
    [else
     (local [(define s (first transitions))]
       (if (state=? (transition-current s) current)
           (transition-next s)
           (find (rest transitions) current)))]))

; FSM FSM-State -> FSM-State
; matches the keys pressed by a player with the given FSM
(define (simulate fsm s0)
  (local [; State of the World: FSM-State
          ; FSM-State KeyEvent -> FSM-State
          (define (find-next-state s key-event)
            (find fsm s))]
    (big-bang s0
      (to-draw state-as-colored-square)
      (on-key find-next-state))))
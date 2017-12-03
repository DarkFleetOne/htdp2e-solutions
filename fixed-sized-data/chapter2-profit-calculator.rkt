;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chapter2-profit-calculator) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define AVG-ATTENDENCE 120)
(define AVG-PRICE 5.00)
(define AVG-CHG-ATTENDENCE 15)
(define FLX-PRICE 0.10)
(define FLT-FXD-COST 180.00)
(define VAR-FXD-COST 0.04)
(define VAR-FXD-COST2 1.50)

(define (attendees ticket-price)
  (- AVG-ATTENDENCE (* (- ticket-price AVG-PRICE) (/ AVG-CHG-ATTENDENCE FLX-PRICE))))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  (+ FLT-FXD-COST (* VAR-FXD-COST (attendees ticket-price))))
(define (cost2 ticket-price)
  (* VAR-FXD-COST2 (attendees ticket-price)))

(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))
(define (profit2 ticket-price)
  (- (revenue ticket-price)
     (cost2 ticket-price)))

(define (portable-profit-example price) ;The same as above composed into a single function.
  (- (* (+ AVG-ATTENDENCE
           (* (/ AVG-PRICE FLX-PRICE)
              (- AVG-PRICE price)))
        price)
     (+ FLT-FXD-COST
        (* VAR-FXD-COST
           (+ AVG-ATTENDENCE
              (* (/ AVG-CHG-ATTENDENCE FLX-PRICE)
                 (- AVG-PRICE price)))))))

(define (portable-profit-example2 price) ;The same as above composed into a single function.
  (- (* (+ AVG-ATTENDENCE
           (* (/ AVG-PRICE FLX-PRICE)
              (- AVG-PRICE price)))
        price)
        (* VAR-FXD-COST2
           (+ AVG-ATTENDENCE
              (* (/ AVG-CHG-ATTENDENCE FLX-PRICE)
                 (- AVG-PRICE price))))))

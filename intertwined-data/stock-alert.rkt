


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

(define PREFIX "https://www.google.com/finance?q=")
(define SIZE 22) ; font size
;; (define mu (read-xexpr "mu-google.html"))

(define-struct data [price delta])
; A StockWorld is a structure: (make-data String String)
; interpretation data is a representation of the StockWorld's properties

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

; Xexpr String -> [Maybe String]
; searches arbitrary Xexpr for attribute and produces String
(define (get-xexpr x s)
  (find-attr (xexpr-attr x) s))

; Xexpr String -> String
; retrieves the value of the "content" attribute
; from a 'meta element that has attribute "itemprop"
; with value s

(check-expect (get '(meta ((content "+1") (itemprop "F"))) "F") "+1")

(define (get x s)
  (local ((define result (get-xexpr x s)))
    (if (string? result)
        result
        (error "not found"))))

; String -> StockWorld
; retrieves the stock price of co and its change every 15s
(define (stock-alert co)
  (local ((define url (string-append PREFIX co))
          ; [StockWorld -> StockWorld]
          ; creates a data with the Xexpr at url
          (define (retrieve-stock-data __w)
            (local ((define x (read-xexpr/web url)))
              (make-data (get x "price")
                         (get x "priceChange"))))
          ; StockWorld -> Image
          (define (render-stock-data w)
            (local (; [StockWorld -> String] -> Image
                    ; creates and displays an image
                    ; showing the properties of current data
                    (define (word sel col)
                      (text (sel w) SIZE col)))
              (overlay (beside (word data-price 'black)
                               (text " " SIZE 'white)
                               (word data-delta 'red))
                       (rectangle 300 35 'solid 'white)))))
    (big-bang (retrieve-stock-data 'no-use)
              [on-tick retrieve-stock-data 15]
              [to-draw render-stock-data])))

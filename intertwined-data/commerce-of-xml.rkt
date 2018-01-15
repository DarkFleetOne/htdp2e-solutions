#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")
((modname commerce-of-xml)
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

; An Xexpr.v0 (short for X-expression) is a one-item list:
;  (cons Symbol '())

; An Xepr.v1 is a list:
;  (cons Symbol [List-of Xexpr.v1])

; An Xepr is a list:
; - (cons Symbol Body)
; - XWord
; - XEnum
; where Body is:
; - (cons [List-of Attribute] [List-of Xexpr]))
; - [List-of Xexpr]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))
; XWord is a Body and single Attribute:
;   '(word ((text String)))
; An XEnum.v1 is one of:
; - (cons 'ul [List-of XItem.v1])
; - (cons 'ul (cons Attributes [List-of XItem.v1]))
; An XItem.v1 is one of:
; - (cons 'li (cons XWord '()))
; - (cons 'li (cons Attributes (cons XWord '())))
; An XItem.v2 is one of:
; - (cons 'li (cons XWord '()))
; - (cons 'li (cons [List-of Attribute] (list XWord)))
; - (cons 'li (cons XEnum.v2 '()))
; - (cons 'li (cons [List-of Attribute] (list XEnum.v2)))
; An XEnum.v2 is one of:
; - (cons 'ul [List-of XItem.v2])
; - (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))

(define xml1 '(transition ((from "seen-e") (to "seen-f"))))
(define xml2 '(ul (li (word) (word)) (li (word))))
(define xml3 '(server ((name "example.org"))))
(define xml4 '(carcas (board (grass)) (player ((name "sam")))))
(define xml5 '(start))
(define a0 '((initial "X")))
(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))
(define xw1 '(word ((text "hello"))))
(define xw2 '(word ((text "world"))))
(define xw3 '(word ((text "goodbye"))))
(define it1 '(li (word ((text "one")))))
(define it2 '(li (word ((text "two")))))
(define en0 '(ul (li (word ((text "one"))))
                 (li (word ((text "two"))))))
(define SIZE 12)    ; Font Size
(define COLOR 'red) ; Font Color
(define BT (beside (circle 1 "solid" "red") (text " " SIZE COLOR)))
(define en0-rendered (above/align 'left
                                  (beside/align 'center BT (text "one" SIZE COLOR))
                                  (beside/align 'center BT (text "two" SIZE COLOR))))
(define xw1-rendered (text "hello" SIZE COLOR))
(define hello-pic (text "hello" SIZE COLOR))
(define bulletized-hello-pic (beside/align 'center BT hello-pic))
(define it1-rendered (beside/align 'center BT (text "one" SIZE COLOR)))

; [List-of Attribute] or Xexpr -> Boolean
; is x a list of attributes

(define (list-of-attributes? x)
  (cond
    [(empty? x) #t]
    [else (local ((define possible-attribute (first x)))
            (cons? possible-attribute))]))

; Xexpr -> [List-of Attribute]
; retrieves the list of attributes of xe

(check-expect (xexpr-attr e0) '())
(check-expect (xexpr-attr e1) '((initial "X")))
(check-expect (xexpr-attr e2) '())
(check-expect (xexpr-attr e3) '())
(check-expect (xexpr-attr e4) '((initial "X")))

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

(check-expect (xexpr-name e0) 'machine)
(check-expect (xexpr-name xml4) 'carcas)
(check-expect (xexpr-name xml3) 'server)

(define (xexpr-name xe)
  (first xe))

; Xexpr -> [List-of Xexpr]
; retrieves the list of content of xe

(check-expect (xexpr-content e0) '())
(check-expect (xexpr-content e2) '((action)))
(check-expect (xexpr-content e3) '(action))
(check-expect (xexpr-content e4) '((action) (action)))
(check-expect (xexpr-content en0) '((li (word ((text "one"))))
                                    (li (word ((text "two"))))))

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

(check-expect (find-attr (xexpr-attr e1) 'initial) "X")
(check-expect (find-attr (xexpr-attr e1) 'final) #f)
(check-expect (find-attr (xexpr-attr xml1) 'to) "seen-f")

(define (find-attr loa s)
  (local ((define att (assq s loa)))
    (if (cons? att)
        (second att)
        att)))

; Xexpr -> Boolean
; Determines if given xexpr is an xword

(check-expect (word? xw1) #t)
(check-expect (word? e1) #f)
(check-expect (word? xml3) #f)
(check-expect (word? xw2) #t)

(define (word? w)
  (cond
    [(empty? w) #f]
    [(symbol=? 'word (first w)) #t]
    [else #f]))

; XWord -> String
; Given an XWord, returns the value
; of its text attribute

(check-expect (word-text xw1) "hello")
(check-expect (word-text xw2) "world")
(check-error (word-text xml3) "Given Xexpr not XWord")

(define (word-text w)
  (if (word? w)
      (find-attr (xexpr-attr w) 'text)
      (error "Given Xexpr not XWord")))

; XItem.v1 -> Image
; renders an item as a "word" prefixed by a bullet

; TODO: Tests

(define (render-item1.v1 i)
  (local ((define content (xexpr-content i)) ; Extra line suggested by HtDP
          ; (define element (first content))   ; word-text assumes incompatible representation?
          (define a-word (word-text content))  ; Fixed in render-item below
          (define item (text a-word 12 'red)))
    (beside/align 'center BT item)))

; XEnum.v1 -> Image
; renders a simple enumeration as an image

;; (check-expect (render-enum1 en0) en0-rendered)  ; see render-enum below

(define (render-enum1.v1 xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v1 Image -> Image
          (define (deal-with-one item so-far)
            (above/align 'left
                         (render-item1.v1 item)
                         so-far)))
    (foldr deal-with-one empty-image content)))

; Image -> Image
; marks item with bullet

(check-expect (bulletize hello-pic) bulletized-hello-pic)

(define (bulletize item)
  (beside/align 'center BT item))

; XEnum.v2 -> Image
; renders an XEnum.v2 as an image

(check-expect (render-enum en0) en0-rendered)

(define (render-enum xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v2 Image -> Image
          (define (deal-with-one item so-far)
            (above/align 'left (render-item item) so-far)))
    (foldr deal-with-one empty-image content)))

; XItem.v2 -> Image
; renders one XItem.v2 as an Image

(check-expect (render-item it1) it1-rendered)

(define (render-item an-item)
  (local ((define content (first (xexpr-content an-item))))
    (bulletize
     (cond
       [(word? content) (text (word-text content) SIZE COLOR)]
       [else (render-enum content)]))))

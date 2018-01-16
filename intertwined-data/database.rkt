


#reader(lib "htdp-advanced-reader.ss" "lang")
((modname database)
 (read-case-sensitive #t)
 (teachpacks ((lib "image.rkt" "teachpack" "2htdp")
              (lib "universe.rkt" "teachpack" "2htdp")
              (lib "batch-io.rkt" "teachpack" "2htdp")
              (lib "abstraction.rkt" "teachpack" "2htdp")))
 (htdp-settings #(#t quasiquote repeating-decimal #t #t none #t
                  ((lib "image.rkt" "teachpack" "2htdp")
                   (lib "universe.rkt" "teachpack" "2htdp")
                   (lib "batch-io.rkt" "teachpack" "2htdp")
                   (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))

(define-struct db [schema content])
; A DB is a structure:
;   (make-db Schema Content)

; A Schema is a [List-of Spec]
; A Spec is a [List Label Predicate]
; A Label is a String
; A Predicate is a [Any -> Boolean]

; A (piece of) Content is a [List-of Row]
; A Row is a [List-of Cell]
; A Cell is Any
; constraint cells do not contain functions

; integrity constraint In (make-db sch con),
; for every row in con,
; (I1) its length is the same as sch's, and
; (I2) its ith Cell satisfies the ith Predicate in sch

(define school-schema
  `(("Name"    ,string?)
    ("Age"     ,integer?)
    ("Present" ,boolean?)))

(define presence-schema
  `(("Present"     ,boolean?)
    ("Description" ,string?)))

(define school-content
  `(("Alice" 35 #t)
    ("Bob"   25 #f)
    ("Carol" 30 #t)
    ("Dave"  32 #f)))

(define presence-content
  `((#t "presence")
    (#f "absence")))

(define school-db
  (make-db school-schema school-content))

(define presence-db
  (make-db presence-schema presence-content))

(define-struct spec [label predicate])
; Spec is a structure: (make-spec Label Predicate)

(define school-spec
  `((,make-spec "Name"    ,string?)
    (,make-spec "Age"     ,integer?)
    (,make-spec "Present" ,boolean?)))

(define presence-spec
  `((,make-spec "Present"     ,boolean?)
    (,make-spec "Description" ,string?)))

(define school-db1
  (make-db school-spec school-content))

(define presence-db1
  (make-db presence-spec presence-content))

(define projected-content
  `(("Alice" #t)
    ("Bob"   #f)
    ("Carol" #t)
    ("Dave"  #f)))

(define projected-schema
  `(("Name" ,string?) ("Present" ,boolean?)))

(define projected-db
  (make-db projected-schema projected-content))

; DB -> Boolean
; do all rows in db satisfy (I1) and (I2)

(check-expect (integrity-check school-db) #t)
(check-expect (integrity-check presence-db) #t)

(define (integrity-check db)
  (let* ([schema (db-schema db)]
         [content (db-content db)]
         [width (length schema)]
         ; Cell -> Boolean
         [cell-integrity-check (λ (spec column) [(second spec) column])]
         ; Cell -> Boolean
         [check-every-cell (λ (row) (andmap cell-integrity-check schema row))]
         ; Row -> Boolean
         [length-of-row-check (λ (row) (= (length row) width))]
         ; Row -> Boolean
         [row-integrity-check (λ (row) (and (length-of-row-check row)
                                            (check-every-cell row)))])
    (andmap row-integrity-check content)))

; DB [List-of Label] -> DB
; retains a column from db if its label is in labels

(check-expect (db-content (project school-db '("Name" "Present"))) projected-content)

(define (project db labels)
  (letrec ([schema (db-schema db)]
           [content (db-content db)]
           ; Spec -> Boolean
           ; does this spec belong to the new schema
           [keep? (λ (c) (member? (first c) labels))]
           [mask (map keep? schema)]
           ; Row -> Row
           ; retains those columns whose name is in labels
           [row-project (λ (row) (foldr (λ (cell m c) (if m (cons cell c) c))
                                        '()
                                        row
                                        mask))])
    (make-db (filter keep? schema)
             (map row-project content))))

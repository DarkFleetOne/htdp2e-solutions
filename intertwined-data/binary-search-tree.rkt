

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


(define-struct no-info [])
(define NONE (make-no-info))

(define-struct node [ssn name left right])
; A BT (short for Binary Tree) is one of:
; - NONE
; - (make-node Number Symbol BT BT)

(define bt1 (make-node 15 'd NONE (make-node 24 'i NONE NONE)))
(define bt2 (make-node 15 'd (make-node 87 'h NONE NONE) NONE))

; BT Number -> Boolean
; determines whether a given number occurs
; in some given BT

(check-expect (contains-bt? bt1 15) #true)
(check-expect (contains-bt? bt1 24) #true)
(check-expect (contains-bt? bt2 69) #false)

(define (contains-bt? bt n)
  (cond
    [(no-info? bt) #false]
    [else (or (= (node-ssn bt) n)
              (contains-bt? (node-left bt) n)
              (contains-bt? (node-right bt) n))]))

; BT Number -> [String or Boolean]
; if the tree contains a node whose ssn is n,
; produces the value of name in that node
; Otherwise, produces #false

(check-expect (search-bt bt1 15) 'd)
(check-expect (search-bt bt1 24) 'i)
(check-expect (search-bt bt2 87) 'h)
(check-expect (search-bt bt2 69) #false)

(define (search-bt bt n)
  (cond
    [(not (contains-bt? bt n)) #false]
    [else (cond
            [(= (node-ssn bt) n) (node-name bt)]
            [(contains-bt? (node-left bt) n) (search-bt (node-left bt) n)]
            [(contains-bt? (node-right bt) n) (search-bt (node-right bt) n)])]))

; A BT is a BST (Binary Search Tree) if and only if:
; - NONE
; - (make-node ssn0 name0 L R) if:
; - - L is BST
; - - R is BST
; - - all ssn in L < ssn0
; - - all ssn in R > ssn0

; BT -> List-of Numbers
; produces the sequence of all ssn numbers in bt
; as they show up from left to right
; when looking at a tree diagram

(check-expect (inorder bt1) '(15 24))
(check-expect (inorder bt2) '(87 15))

(define (inorder bt)
  (cond
    [(no-info? bt) '()]
    [else (append (inorder (node-left bt))
                  (list (node-ssn bt))
                  (inorder (node-right bt)))]))

; BST Number -> [String or NONE]
; if BST contains ssn, return name else NONE

(check-expect (search-bst bt1 15) 'd)
(check-expect (search-bst bt1 24) 'i)
(check-expect (search-bst bt2 69) NONE)

(define (search-bst bst n)
  (cond
    [(no-info? bst) NONE]
    [else (cond
            [(= (node-ssn bst) n) (node-name bst)]
            [(> (node-ssn bst) n) (search-bst (node-left bst) n)]
            [(< (node-ssn bst) n) (search-bst (node-right bst) n)])]))

; BST Number Symbol -> BST
; produces a bst like the original
; in place of one NONE a node structure

(check-expect (create-bst bt1 12 'f)
              (make-node 15 'd (make-node 12 'f NONE NONE) (make-node 24 'i NONE NONE)))
(check-expect (create-bst bt1 17 'g)
              (make-node 15 'd NONE (make-node 24 'i (make-node 17 'g NONE NONE) NONE)))

(define (create-bst b n s)
  (cond
    [(no-info? b) (make-node n s NONE NONE)]
    [else (cond
            [(> (node-ssn b) n)
             (make-node (node-ssn b) (node-name b) (create-bst (node-left b) n s) (node-right b))]
            [(< (node-ssn b) n)
             (make-node (node-ssn b) (node-name b) (node-left b) (create-bst (node-right b) n s))])]))

; [List-of [List Number Symbol]] -> BST
; consumes a list of numbers and names
; produces a BST

(define bst-list '((99 o) (77 l) (24 i) (10 h) (95 g) (15 d) (89 c) (29 b) (63 a)))
(define bst-a (make-node 63 'a
                         (make-node 29 'b
                                    (make-node 15 'd
                                               (make-node 10 'h
                                                          NONE
                                                          NONE)
                                               (make-node 24 'i
                                                          NONE
                                                          NONE))
                                    NONE)
                         (make-node 89 'c
                                    (make-node 77 'l
                                               NONE
                                               NONE)
                                    (make-node 95 'g
                                               NONE
                                               (make-node 99 'o
                                                          NONE
                                                          NONE)))))

(check-expect (create-bst-from-list bst-list) bst-a)

(define (create-bst-from-list l)
  (cond
    [(empty? l) NONE]
    [else (create-bst (create-bst-from-list (rest l)) (caar l) (cadar l))]))

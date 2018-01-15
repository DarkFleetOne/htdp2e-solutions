


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

; A Dir.v1 (short for directory) is one of:
; - '()
; - (cons File.v1 Dir.v1)
; - (cons Dir.v1 Dir.v1)

; A File.v1 is a String

(define dir1 (list "read!"
                   (list "part1" "part2" "part3")
                   (list (list "hang" "draw")
                         (list "read!"))))

; Dir.v1 -> Number
; determines how many files Dir.v1 contains

(check-expect (how-many.v1 dir1) 7)

(define (how-many.v1 dir)
  (cond
    [(empty? dir) 0]
    [(string? (first dir)) (+ 1 (how-many.v1 (rest dir)))]
    [(cons? (first dir)) (+ (how-many.v1 (first dir)) (how-many.v1 (rest dir)))]))



(define-struct no-dir [])
(define ND (make-no-dir))
(define-struct dir [name content])

; A Dir.v2 is a structure:
;   (make-dir String LOFD)

; An LOFD (short for list of files and directories) is one of:
; - '()
; - (cons File.v2 LOFD)
; - (cons Dir.v2 LOFD)

; A File.v2 is a String

(define dir2 (make-dir "TS"
                       (list (make-dir "Text" (list "part1" "part2" "part3"))
                             "read!"
                             (make-dir "Libs" (list (make-dir "Code" (list "hang" "draw"))
                                                          (make-dir "Docs" (list "read!")))))))


; Dir.v2 -> Number
; determines how many files given Dir contains

; (check-expect (how-many.v2 dir2) 7)

(define (how-many.bad dir)
  (cond
    [(empty? (dir-content dir)) 0]
    [(dir? (first (dir-content dir))) (+ (how-many.bad (first (dir-content dir)))(how-many.bad (rest (dir-content dir))))]
    [(string? (first (dir-content dir))) (+ (length (dir-content dir)) (how-many.bad (dir-content dir)))]))

(define (how-many.v2 dir)
  (cond
    [(dir? dir) (count-dir (dir-content dir))]
    [else (count-file dir)]))

(define (count-dir dir)
  (cond
    [(empty? dir) 0]
    [(string? (first dir)) (count-file dir)]
    [else (+ (how-many.v2 (first dir)) (count-dir (rest dir)))]))

(define (count-file dir)
  (cond
    [(empty? dir) 0]
    [(string? (first dir)) (add1 (how-many.v2 (rest dir)))]
    [else (how-many.v2 (rest dir))]))


(define-struct file [name size content])
; A File.v3 is a structure:
;  (make-file String N String)

(define-struct dir.v3 [name dirs files])
; A Dir.v3 is a structure:
;  (make-dir.v3 String List-of Dir List-of File)

; A List-of Dir is one of:
; - '()
; - (cons Dir.v3 List-of Dir)

; A List-of File is one of:
; - '()
; - (cons File.v3 List-of File)

(define dir3
  (make-dir.v3 "TS" `(,(make-dir.v3 "Text" '()
                                    `(,(make-file "part1" 99 "")
                                      ,(make-file "part2" 52 "")
                                      ,(make-file "part3" 17 "")))
                      ,(make-dir.v3 "Libs"
                                    `(,(make-dir.v3 "Code" '()
                                                    `(,(make-file "hang" 8 "")
                                                      ,(make-file "draw" 2 "")))
                                      ,(make-dir.v3 "Docs" '()
                                                    `(,(make-file "read!" 19 ""))))
                                    '()))
               `(,(make-file "read!" 10 ""))))

; Dir.v3 -> Number
; counts how many files in a given directory

(check-expect (how-many dir3) 7)

(define (count-dir.v3 lod)
  (foldl + 0 (map count-file.v3 lod)))

(define (count-file.v3 dir)
  (cond
    [(empty? (dir.v3-files dir)) (count-dir.v3 (dir.v3-dirs dir))]
    [else (length (dir.v3-files dir))]))

(define (how-many.v3 dir)
  (cond
    [(empty? (dir.v3-dirs dir)) (count-file.v3 (dir.v3-files dir))]
    [else (+ (count-dir.v3 (dir.v3-dirs dir)) (length (dir.v3-files dir)))]))

(define (how-many dir)
  (local ((define (reduce-dir dir)
            (+ (count-dir (dir.v3-dirs dir)) (length (dir.v3-files dir))))
          (define (count-file dir)
            (cond
              [(empty? (dir.v3-files dir)) (count-dir (dir.v3-dirs dir))]
              [else (reduce-dir dir)]))
          (define (count-dir lod)
            (foldl + 0 (map count-file lod))))
    (cond
      [(empty? (dir.v3-dirs dir)) (count-file (dir.v3-files dir))]
      [else (reduce-dir dir)])))

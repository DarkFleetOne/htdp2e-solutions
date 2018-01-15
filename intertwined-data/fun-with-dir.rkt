


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

(require htdp/dir)

(define dir1
  (make-dir "TS" `(,(make-dir "Text" '()
                              `(,(make-file "part1" 99 "")
                                ,(make-file "part2" 52 "")
                                ,(make-file "part3" 17 "")))
                   ,(make-dir "Libs"
                              `(,(make-dir "Code" '()
                                           `(,(make-file "hang" 8 "")
                                             ,(make-file "draw" 2 "")))
                                ,(make-dir "Docs" '()
                                           `(,(make-file "read!" 19 ""))))
                              '()))
            `(,(make-file "read!" 10 "")
              ,(make-file "write!" 11 ""))))

; Dir -> Number
; counts how many files in a given directory

(check-expect (how-many dir1) 8)

(define (how-many dir)
  (local ((define (reduce-dir dir)
            (+ (count-dir (dir-dirs dir)) (length (dir-files dir))))
          (define (count-file dir)
            (cond
              [(empty? (dir-files dir)) (count-dir (dir-dirs dir))]
              [else (reduce-dir dir)]))
          (define (count-dir lod)
            (foldl + 0 (map count-file lod))))
    (cond
      [(empty? (dir-dirs dir)) (count-file (dir-files dir))]
      [else (reduce-dir dir)])))

; Dir String -> Boolean
; determines whether the named file is
; in the given directory

(check-expect (find? dir1 "draw") #true)
(check-expect (find? dir1 "melange") #false)

(define (find? dir n)
  (local (; List-of Files String -> Boolean
          ; consumes a list-of make-file and a name
          ; determines if a file with name is in list
          (define (find-file lof n)
            (ormap (λ (file) (string=? n (file-name file)))
                   lof))
          ; List-of Dirs String -> Boolean
          ; consumes a list-of make-dir and a name
          ; determines if file with name is in current directory
          ; or nested directory
          (define (find-dir lod n)
            (ormap (λ (dir) (or (find-dir (dir-dirs dir) n)
                                (find-file (dir-files dir) n)))
                   lod)))
    (cond
     [(empty? (dir-dirs dir)) (find-file (dir-files dir) n)]
     [else (or (find-file (dir-files dir) n)
               (find-dir (dir-dirs dir) n))])))

; Dir -> List-of List-of String
; consumes a make-dir, produces a
; List-of List-of names of the files and
; directories reflecting the dir structure

(check-expect (ls dir1) '("TS" "read!" "write!"
                               ("Text" "part1" "part2" "part3")
                               ("Libs"
                                ("Code" "hang" "draw")
                                ("Docs" "read!"))))
(check-expect (ls (make-dir "test" '() '())) '("test"))

(define (ls dir)
  (local (; List-of File -> List-of String
          ; consumes a list of make-file
          ; returns a list of the file's names
          (define (ls-files lof)
            (map (λ (file) (file-name file))
                 lof))
          ; List-of Dir -> List-of String
          ; consumes a list-of make-dir
          ; returns a list of the names of the
          ; dir, files, and nested directory contents
          (define (ls-dir lod)
            (map (λ (dir) (append (list (dir-name dir))
                                  (ls-dir (dir-dirs dir))
                                  (ls-files (dir-files dir))))
                 lod)))
      (append (list (dir-name dir))
              (ls-files (dir-files dir))
              (ls-dir (dir-dirs dir)))))

; Dir -> Number
; consumes a make-dir, produces the
; size of all files and directories in dir

(check-expect (du dir1) 223)

(define (du dir)
  (local ((define (du-files lof)
            (foldl + 0 (map (λ (file) (file-size file))
                            lof)))
          (define (du-dirs lod)
            (foldl + 0 (map (λ (dir) (+ 1
                                        (du-files (dir-files dir))
                                        (du-dirs (dir-dirs dir))))
                  lod))))
    (+ 1
       (du-files (dir-files dir))
       (du-dirs (dir-dirs dir)))))

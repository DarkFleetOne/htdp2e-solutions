


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

; S-expr Symbol Atom -> S-expr
; replaces all occurences of old in sexp with new

(check-expect (substitute '(((world) bye) bye) 'bye '42)
              '(((world) 42) 42))

(define (substitute sexp old new)
  (cond
    [(or (number? sexp)
         (string? sexp)
         (symbol? sexp)) (if (equal? sexp old) new sexp)]
    [else (map (λ (s) (substitute s old new)) sexp)]))

#lang racket

(define (sum dat)
  (apply + dat))

;; XXX make this an arg/lambda in the final fn
(define (process-group dat)
  (sum (map string->number dat)))

(define (make-group dat)
  (define (helper dat current res)
    ;; dat is complete list
    ;; current is accumulator for current section
    ;; res is result alist (number . result)
    (if (empty? dat)
        (cons (cons (length res)
                    (process-group current))
              res)
        ;; handle next item in the list
        (let ([nxt (string-trim (first dat))])
          (if (equal? 0 (string-length nxt))
              (helper (rest dat)
                      empty
                      (cons (cons (length res)
                                  (process-group current))
                            res))
              ;; regular item, just add to accumulator
              (helper (rest dat)
                      (cons nxt current)
                      res)))))
  (helper dat empty empty))


(define (aoc-1-1 input-file)
    (let* ([res (make-group (file->lines input-file))]
           [sorted-res (sort res > #:key cdr)]
           [winner (first sorted-res)])
      (format "Elf ~a is carrying ~a calories" (+ 1 (car winner)) (cdr winner))))

(define (aoc-1-2 input-file)
  (let* ([res (make-group (file->lines input-file))]
         [sorted-res (sort res > #:key cdr)])
    (sum (map cdr (take sorted-res 3)))))
         

#lang racket

(define nchar-a (char->integer #\a))
(define nchar-z (char->integer #\z))
(define nchar-A (char->integer #\A))
(define nchar-Z (char->integer #\Z))

(define (priority ch)
  (let [(n (char->integer ch))]
    (cond
      [(and (>= n nchar-a)
            (<= n nchar-z)) (+ 1 (- n nchar-a))]
      [(and (>= n nchar-A)
            (<= n nchar-Z)) (+ 27 (- n nchar-A))])))

(define (sum dat)
  (apply + dat))

(define (process-line line)
  (let* ([len (string-length line)]
         [mid (/ len 2)]
         [s1 (substring line 0 mid)]
         [s2 (substring line mid len)]
         [set1 (list->set (string->list s1))]
         [set2 (list->set (string->list s2))])
    (sum (map priority (set->list (set-intersect set1 set2))))))

;; expects a list of raw strings from the input file
(define (process-group dat)
  (sum (map process-line
       dat)))

(define (aoc-3-1 input-file)
  (process-group (file->lines input-file)))

;; part 2

;; https://stackoverflow.com/a/23394290
(define (split-by lst n)
   (if (not (empty? lst))
       (cons (take lst n) (split-by (drop lst n) n))
       '() ))

;; expects a list of 3, returns a list of common chars
(define (process-group-2 dat)
  (set->list (apply set-intersect
         (map (lambda (line)
                (list->set (string->list line)))
              dat))))
   

(define (aoc-3-2 input-file)
  (sum (map priority 
            (flatten (map process-group-2
                          (split-by (file->lines input-file) 3))))))
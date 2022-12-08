#lang racket

;; try just using pairs to represent ranges

(define (parse-pair s)
  (let ([split (string-split s "-")])
    (cons (string->number (first split))
          (string->number (second split)))))

(define (parse-line s)
  (map parse-pair (string-split s ",")))

;; does pair p1 fully contain p2?
(define (fully-contains? p1 p2)
  (and (<= (car p1) (car p2))
       (>= (cdr p1) (cdr p2))))

(define (either-contains? p1 p2)
  (or (fully-contains? p1 p2)
      (fully-contains? p2 p1)))

;; a little silly
(define (count-true dat)
  (length (filter (lambda (b) (boolean=? b true)) dat)))

(define (aoc-4-1 input-file)
  (count-true (map (lambda (line)
                     (apply either-contains? (parse-line line)))
                   (file->lines input-file))))

;; do p1 and p2 overlap?
(define (overlap? p1 p2)
  ;; p1.1 is between p2.1 and p2.2
  ;; OR
  ;; p1.2 is between p2.1 and p2.2
  (or (and (>= (car p1) (car p2))
           (<= (car p1) (cdr p2)))
      (and (>= (cdr p1) (car p2))
           (<= (cdr p1) (cdr p2)))))

(define (o? p1 p2)
  ;; needed to catch one range completely within another
  (or (overlap? p1 p2) (overlap? p2 p1)))

(define (aoc-4-2 input-file)
  (count-true (map (lambda (line)
                     (apply o? (parse-line line)))
                   (file->lines input-file))))
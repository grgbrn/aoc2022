#lang racket

(define (input-to-sym s)
  (match s
    ["A" 'rock]
    ["B" 'paper]
    ["C" 'scissors]
    ["X" 'rock]
    ["Y" 'paper]
    ["Z" 'scissors]))

;; does s1 beat s2?
(define (win? s1 s2)
  (or (and (eq? s1 'rock) (eq? s2 'scissors))
      (and (eq? s1 'paper) (eq? s2 'rock))
      (and (eq? s1 'scissors) (eq? s2 'paper))))

;; does s1 lose to s2?
(define (loss? s1 s2)
  (win? s2 s1))

(define (draw? s1 s2)
  (eq? s1 s2))

(define (shape-score s)
  (match s
    ['rock 1]
    ['paper 2]
    ['scissors 3]))

(define (score-round s1 s2)
  (cond
    [(win? s1 s2) (+ 6 (shape-score s1))]
    [(draw? s1 s2) (+ 3 (shape-score s1))]
    [(loss? s1 s2) (shape-score s1)]))

(define (sum dat)
  (apply + dat))

;; expects a list of raw strings from the input file
(define (process-group dat)
  (sum (map (lambda (line)
         (let* ([tmp (string-split line)]
                [my-move (second tmp)]
                [their-move (first tmp)])
           (score-round (input-to-sym my-move)
                        (input-to-sym their-move))))
       dat)))

(define (aoc-2-1 input-file)
  (process-group (file->lines input-file)))

;; rules have changed but we can't redefine functions
;; so i guess just rename?
(define (input-to-sym-2 s)
  (match s
    ["A" 'rock]
    ["B" 'paper]
    ["C" 'scissors]
    ["X" 'lose]
    ["Y" 'draw]
    ["Z" 'win]))

(define shape-priority (list 'rock 'scissors 'paper 'rock))

(define (what-shape s1 outcome)
  (match outcome
    ['win (second (member s1 (reverse shape-priority)))]
    ['draw s1]
    ['lose (second (member s1 shape-priority))]))

;; expects a list of raw strings from the input file
(define (process-group-2 dat)
  (sum (map (lambda (line)
         (let* ([tmp (string-split line)]
                [their-move (input-to-sym-2 (first tmp))]
                [outcome (input-to-sym-2 (second tmp))]
                [my-move (what-shape their-move outcome)])
           (score-round my-move their-move)))
       dat)))

(define (aoc-2-2 input-file)
  (process-group-2 (file->lines input-file)))
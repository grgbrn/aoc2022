#lang racket

(module+ test
  (require rackunit)

  (define board
    (list
     (list 3 0 3 7 3)
     (list 2 5 5 1 2)
     (list 6 5 3 3 2)
     (list 3 3 5 4 9)
     (list 3 5 3 9 0))))

;; return a vertical row from a board
;; TODO: memoize? cache these results? this may be expensive
(define (vertical board ix)
  (map (lambda (row) (list-ref row ix))
       board))

;; return a horizontal row from a board
(define (horizontal board ix)
  (list-ref board ix))

(define (height board)
  (length (vertical board 0)))

(define (width board)
  (length (horizontal board 0)))

(module+ test
  (check-equal? (vertical board 0) '(3 2 6 3 3))
  (check-equal? (vertical board 1) '(0 5 5 3 5))
  (check-equal? (vertical board 4) '(3 2 2 9 0))

  (check-equal? (horizontal board 0) '(3 0 3 7 3))

  (check-equal? (height board) 5)
  (check-equal? (width board) 5))

;; get a single value. point is (x . y)
(define (get-at board point)
  (list-ref (horizontal board (cdr point))
            (car point)))

;; return horizontal/vertical paths from point to edge
(define (edge-paths board point)
  (let* ([x (car point)]
         [y (cdr point)]
         [h (horizontal board y)]
         [v (vertical board x)])
    (list
     (take h x)
     (drop h (add1 x))
     (take v y)
     (drop v (add1 y)))))

(module+ test
  (check-equal? (get-at board '(0 . 0)) 3)
  (check-equal? (get-at board '(4 . 3)) 9)
  (check-equal? (get-at board '(3 . 1)) 1)

  (check-equal? (edge-paths board '(1 . 1))
                (list '(2)
                      '(5 1 2)
                      '(0)
                      '(5 3 5)))

  (check-equal? (edge-paths board '(0 . 0))
                (list '()
                      '(0 3 7 3)
                      '()
                      '(2 6 3 3))))

(define (check-point-visibility board pt)
  (let ([tree-height (get-at board pt)])
    (for/or ([path (edge-paths board pt)])
      (for/and ([elt path])
        (< elt tree-height)))))

(module+ test
  (check-equal? (check-point-visibility board '(1 . 1)) #t)
  (check-equal? (check-point-visibility board '(2 . 1)) #t)
  (check-equal? (check-point-visibility board '(3 . 1)) #f)

  (check-equal? (check-point-visibility board '(1 . 2)) #t)
  (check-equal? (check-point-visibility board '(2 . 2)) #f)
  (check-equal? (check-point-visibility board '(3 . 2)) #t)

  (check-equal? (check-point-visibility board '(1 . 3)) #f)
  (check-equal? (check-point-visibility board '(2 . 3)) #t)
  (check-equal? (check-point-visibility board '(3 . 3)) #f))

(define (check-inner-visibility board)
  (for*/list ([x (range 1 (sub1 (width board)))]
              [y (range 1 (sub1 (height board)))])
    (check-point-visibility board (cons x y))))

(define (inner-visibility-count board)
  (count-true (check-inner-visibility board)))

(define (count-true dat)
  (length (filter (lambda (x) x) dat)))

(define (visibility-count board)
  (+
   (* 2 (length board)) ;; count corner pieces here
   (* 2 (- (height board) 2)) ;; omit corners
   (inner-visibility-count board)))

;;
;; parse input
;;

(define (parse-line line)
  (let ([zero (char->integer #\0)])
    (for/list ([ch (string->list line)])
      (- (char->integer ch) zero))))

(define (parse-file input-file)
  (for/list ([line (file->lines input-file)])
    (parse-line line)))

(define (aoc-8-1 input-file)
  (visibility-count (parse-file input-file)))

;;
;; part 2
;;

(define (visible-count edge-list height)
  ;; like takef but includes the first element >= height
  (for/fold ([acc 0])
            ([elt edge-list]
             #:final (>= elt height))
    (add1 acc)))

(define (scenic-score board pt)
  ;; edge-paths returned in this order: 
  ;;    C
  ;;   A B
  ;;    D
  ;; since we must count outwards from the center point
  ;; reverse lists A and C
  (let* ([tree-height (get-at board pt)]
         [edges (edge-paths board pt)])
    (* (visible-count (reverse (first edges)) tree-height) ;; A
       (visible-count (second edges) tree-height) ;; B
       (visible-count (reverse (third edges)) tree-height) ;; C
       (visible-count (fourth edges) tree-height)))) ;; D

(module+ test
  (check-equal? (scenic-score board '(2 . 1)) 4)
  (check-equal? (scenic-score board '(2 . 3)) 8))

(define (aoc-8-2 input-file)
  (let ([board (parse-file input-file)])
    (for*/list ([x (range 1 (sub1 (width board)))]
                [y (range 1 (sub1 (height board)))])
      (scenic-score board (cons x y)))))

#lang racket

(module+ test
  (require rackunit))

(struct rope
  (head tail) ;; both (x . y) points
  #:transparent)

(define (distance p1 p2)
  (max (abs (- (car p1) (car p2)))
       (abs (- (cdr p1) (cdr p2)))))

(define (diagonal? p1 p2)
  (and (not (= (car p1) (car p2)))
       (not (= (cdr p1) (cdr p2)))))

(module+ test
  #|
    (0,0) (1,0) (2,0)
    (0,1) (1,1) (2,1)
    (0,2) (1,2) (2,2)
  |#
  
  (check-equal? (diagonal? '(0 . 0) '(1 . 1) ) #t)
  (check-equal? (diagonal? '(1 . 0) '(1 . 1) ) #f)
  (check-equal? (diagonal? '(2 . 0) '(1 . 1) ) #t)

  (check-equal? (diagonal? '(0 . 1) '(1 . 1) ) #f)
  (check-equal? (diagonal? '(1 . 1) '(1 . 1) ) #f)
  (check-equal? (diagonal? '(2 . 1) '(1 . 1) ) #f)

  (check-equal? (diagonal? '(0 . 2) '(1 . 1) ) #t)
  (check-equal? (diagonal? '(1 . 2) '(1 . 1) ) #f)
  (check-equal? (diagonal? '(2 . 2) '(1 . 1) ) #t))

;; return a vector (cons cell) to move p2 to position p1
(define (diff p1 p2)
  (cons (- (car p2) (car p1))
        (- (cdr p2) (cdr p1))))

;; limit a vector to magnitude 1
(define (clamp v)
  (define (hack n) ;; not so nice :(
    (cond
      [(= n 0) 0]
      [(>= n 1) 1]
      [(<= n 1) -1]))
  (cons (hack (car v))
        (hack (cdr v))))

;; update a point with a vector
(define (update p vec)
  (cons (+ (car p) (car vec))
        (+ (cdr p) (cdr vec))))

(define (next-head head dir)
  (case dir
    ['U (update head '(0 . -1))]
    ['D (update head '(0 . 1))]
    ['L (update head '(-1 . 0))]
    ['R (update head '(1 . 0))]
    [else (error "unrecognized direction")]))

(define (next-tail head tail)
  (if (<= (distance head tail) 1)
      tail ;; no move required
      (let ([v (diff tail head)])
        ;(displayln (format "~a ~a ~a ~a" head tail v (clamp v)))
        (update tail (clamp v)))))

(module+ test
  ;; horizontal / vertical examples
  (check-equal? (next-tail '(3 . 1) '(1 . 1)) '(2 . 1))
  (check-equal? (next-tail '(1 . 3) '(1 . 1)) '(1 . 2))

  ;; diagonal examples
  (check-equal? (next-tail '(2 . 1) '(1 . 3)) '(2 . 2))
  (check-equal? (next-tail '(4 . 2) '(2 . 3)) '(3 . 2))

  ;; sanity checks - no motion required
  (check-equal? (next-tail '(2 . 1) '(1 . 1)) '(1 . 1))
  (check-equal? (next-tail '(1 . 1) '(2 . 2)) '(2 . 2))
  (check-equal? (next-tail '(1 . 1) '(1 . 1)) '(1 . 1)))

;;
;; parse input
;;
(define (parse-line line)
  (let ([tmp (string-split line)])
    (cons (string->symbol (first tmp))
          (string->number (second tmp)))))

(define (parse-file input-file)
  (for/list ([line (file->lines input-file)])
    (parse-line line)))

;;
;; main
;;
(define (aoc-9-1 input-file)
  (for*/fold ([state (rope '(0 . 0) '(0 . 0))]
              [seen (set)]
              #:result (set-count seen))
             ([cmd (parse-file input-file)]
              [count (range (cdr cmd))])
    ;;(displayln (format "~a #~a" (car cmd) count))
    (let* ([h (next-head (rope-head state) (car cmd))]
           [t (next-tail h (rope-tail state))])
      ;;(displayln (format "  ~a ~a" h t))
      (values
       (rope h t)
       (set-add seen t))
    )))

#lang racket

;;
;; board
;;
;; implementation is a simple list of lists
;; inner lists trivially act as stacks, which is all i need
;;
;; try to be immutable, but AFAIK we don't have persistent
;; data structures. looks like i can easily clone a list
;; with build-list, hopefully that won't be a perf issue
;;

(define (make-board n)
  (build-list n (lambda (x) empty)))

;; make a copy of 'board with 'elt added at 'index
;; index is 1-based
(define (add-board board index-1 elt)
  (let ([index (sub1 index-1)])
    (build-list (length board)
                (lambda (x)
                  (if (= x index)
                      (cons elt (list-ref board index))
                      (list-ref board x))))))

;; use build-list to construct an entirely new list
;; popping 'from and adding it to 'to
;; from-1 and to-1 are 1-based indexes
(define (update-board board from-1 to-1)
  (let* ([from (sub1 from-1)]
        [to (sub1 to-1)]
        [elt (first (list-ref board from))])
    (build-list (length board)
                (lambda (n)
                  (if (= n from)
                      (rest (list-ref board from))
                      (if (= n to)
                          (cons elt (list-ref board to))
                          (list-ref board n)))))))

(define (board-height board)
  (apply max (map length board)))

(define (top-crates board)
  (map car board))

(define (print-board board)
  ;; header line with indexes
  (define (header board)
    (string-join (for/list ([n (range 1 (+ 1 (length board)))])
                 (format " ~a " n))))
  ;; reverse-list-ref, sorta?
  (define (lref dat n)
    (list-ref dat (- (sub1 (length dat)) n)))
  ;; list of strings for each line of the board
  (define (board-lines board)
    (for/list ([n (reverse (range (board-height board)))])
      (string-join
       (for/list ([elt board])
         (if (< n (length elt))
             (format "[~a]" (lref elt n))
             "   ")))))

  (for ([line (append (board-lines board)
                      (list (header board)))])
    (displayln line)))

;;
;; ascii board parsing
;;

(define (non-empty-line? l)
  (> (string-length (string-trim l)) 0))

(define (empty-line? l) (not (non-empty-line? l)))

;; list of pairs (start . end) for match offsets in 'input
(define (find-all-match-positions regex input)
  (define (helper matches start-index)
    (let ([match (regexp-match-positions regex
                                         input
                                         start-index)])
       (if (not match)
           matches
           (helper (cons (first match) matches)
                   (cdr (first match))))))
  (reverse (helper empty 0)))

;; list of pairs giving offsets of integer indexes in 'str
(define (parse-index-offsets str)
  (find-all-match-positions #px"[[:digit:]]+" str))

;; list of substrings in 'str given by 'offset-list
(define (chars-at-offsets str offset-list)
  (map (lambda (pair) (substring str (car pair) (cdr pair)))
       offset-list))

;; list of pairs (index . id) from line
(define (parse-line-with-offsets line offsets)
  (for/list ([id (chars-at-offsets line offsets)]
             [ix (range 1 (add1 (length offsets)))]
             #:when (non-empty-line? id))
    (cons ix id)))

;; list of pairs (board-index char) for initializing the board
(define (parse-init-pairs lines offsets)
  (apply append
         (for/list ([line lines])
           (parse-line-with-offsets line offsets))))

(define (parse-board input-lines)
  (let ([offsets (parse-index-offsets (first (reverse input-lines)))]
        [crate-lines (rest (reverse input-lines))])
  (foldl (lambda (init-pair board)
           (add-board board (car init-pair) (cdr init-pair)))
         (make-board (length offsets))
         (parse-init-pairs crate-lines offsets))))

;;
;; parse command strings
;;

(struct command (count from to) #:transparent)

(define (parse-command line)
  ;; XXX what about no match?
  (let ([match (regexp-match #px"^move (\\d+) from (\\d+) to (\\d+)"
                             line)])
    (command (string->number (second match))
             (string->number (third match))
             (string->number (fourth match)))))

;; generate a sequence of commands that only contains count=1
(define (expand-commands commands)
  (flatten
   (for/list ([cmd commands])
     (build-list (command-count cmd)
                 (lambda (n) (command 1 (command-from cmd) (command-to cmd)))))))
  
(define (apply-commands board commands)
  (foldl (lambda (cmd prev-board)
           (update-board prev-board
                         (command-from cmd)
                         (command-to cmd)))
         board
         (expand-commands commands)))

(define (aoc-5-1 input-file)
  (let-values ([(chunk1 chunk2)
                (splitf-at (file->lines input-file) non-empty-line?)])
    (let* ([board-lines (filter non-empty-line? chunk1)]
          [command-lines (filter non-empty-line? chunk2)]
          [board (parse-board board-lines)]
          [commands (map parse-command command-lines)])
      (displayln "=== start position ===")
      (print-board board)
      (let ([end (apply-commands board commands)])
        (displayln "=== end position ===")
        (print-board end)
        (displayln "=== top crates ===")
        (displayln (top-crates end))))))

;;
;; part 2
;;

;; update-board variation that moves multiple elements at once
;; from-1 and to-1 are 1-based indexes
(define (update-board-n board from-1 to-1 count)
  (let* ([from (sub1 from-1)]
        [to (sub1 to-1)])
    (build-list (length board)
                (lambda (n)
                  (cond
                    [(= n from) (drop (list-ref board from) count)]
                    [(= n to) (append (take (list-ref board from) count)
                                      (list-ref board to))]
                    [else (list-ref board n)])))))                         


(define sample-board (list
                      "[D]        "
                      "[N] [C]    "
                      "[Z] [M] [P]"
                      " 1   2   3 "))

(define (apply-commands-2 board commands)
  (foldl (lambda (cmd prev-board)
           (update-board-n prev-board
                           (command-from cmd)
                           (command-to cmd)
                           (command-count cmd)))
         board
         commands))

(define (aoc-5-2 input-file)
  (let-values ([(chunk1 chunk2)
                (splitf-at (file->lines input-file) non-empty-line?)])
    (let* ([board-lines (filter non-empty-line? chunk1)]
          [command-lines (filter non-empty-line? chunk2)]
          [board (parse-board board-lines)]
          [commands (map parse-command command-lines)])
      (displayln "=== start position ===")
      (print-board board)
      (let ([end (apply-commands-2 board commands)])
        (displayln "=== end position ===")
        (print-board end)
        (displayln "=== top crates ===")
        (displayln (top-crates end))))))
#lang racket

(struct file
  (name
   size) #:transparent)

(struct directory
  (name
   contents) #:transparent)

(define (directory-size d)
  (unless (directory? d)
    (error "directory-size needs a directory"))
  (for/fold ([size 0])
            ([thing (directory-contents d)])
    (cond [(file? thing)
           (+ size (file-size thing))]
          [(directory? thing)
           (+ size (directory-size thing))])))

(define (thing-name thing)
  (cond [(file? thing)
         (file-name thing)]
        [(directory? thing)
         (directory-name thing)]
        [else (error "thing-name got an unexpected type")]))

(define (thing-details thing)
  (cond [(file? thing)
         (format "(file, size=~a)" (file-size thing))]
        [(directory? thing)
         "(dir)"]
        [else (error "thing-name got an unexpected type")]))

(define (print-directory d)
  (define (print-thing thing depth)
    (displayln (format "~a- ~a ~a"
                       (make-string (* depth 2) #\space)
                       (thing-name thing)
                       (thing-details thing))))
  (define (helper d depth)
    (print-thing d depth)
    (for ([thing (directory-contents d)])
      (if (file? thing)
          (print-thing thing (add1 depth))
          (helper thing (add1 depth)))))
  (helper d 0))

(define d2
  (directory "/"
             (list
              (directory "a"
                         (list
                          (directory "e" (list
                                          (file "i" 584)))
                          (file "f" 29116)
                          (file "g" 2557)
                          (file "h.lst" 62596)))
              (file "b.txt" 14848514)
              (file "c.dat" 8504156)
              (directory "d"
                         (list
                          (file "j" 4060174)
                          (file "d.log" 8033020)
                          (file "d.ext" 5626152)
                          (file "k" 7214296))))))

;;
;; actual solution (but still need a parser)
;;

(define (filter-directories d)
  (for/fold ([results (list d)]
             #:result (reverse results))
            ([sub (filter directory? (directory-contents d))])
    (append (filter-directories sub) results)))

(define (aoc7-1-sample)
  (foldl + 0
         (filter (lambda (n) (<= n 100000))
                 (map directory-size (filter-directories d2)))))

;;
;; parsing
;;

(define (parse-file input-file)  
  ;; maintain a directory stack
  (define (update-pwd pwd arg)
    (cond
      [(string=? "/" arg) (list "/")]
      [(string=? ".." arg) (rest pwd)]
      [else (cons arg pwd)]))
  ;; parse out a file or directory struct
  (define (parse-file-line line)
    (let ([tmp (string-split line)])
      (if (string=? (first tmp) "dir")
          (directory (second tmp) empty)
          (file (second tmp) (string->number (first tmp))))))

  (for/fold
   ([pwd empty]
    [tree (directory "/" empty)]
    #:result tree)
   ([line (file->lines input-file)])
    (cond
      [(string-prefix? line "$ cd ")
       (values
        (update-pwd pwd (last (string-split line)))
        tree)]
      [(string-prefix? line "$ ls")
       ;(displayln (format "ls for ~a" pwd))
       (values pwd tree)] ;; no-op
      [else
       ;(displayln (format " ~a" (parse-file-line line)))
       (values
        pwd
        (add-file tree pwd (parse-file-line line)))])))
    
;;
;; tree construction
;;

;; returns a predicate function checking name against 's'
(define (name-equals? s)
  (lambda (arg) (string=? (thing-name arg) s)))

;; returns a file or directory entry from a directory
(define (get-helper dir dirname)
  (first (memf (name-equals? dirname)
               (directory-contents dir))))

;; returns a list of directory entries corresponding
;; to dirstack, including the root directory
;; results are in same order as dirstack (leaf -> root)
(define (get-directory-chain root dirstack)
  (let ([segments (rest (reverse dirstack))])
    (for/fold ([res (list root)])
              ([seg segments])
    (cons (get-helper (first res) seg) res))))

(define (updated-contents old-contents new-thing)
  (let ([pos (index-where old-contents
                          (name-equals? (thing-name new-thing)))])
    (list-set old-contents pos new-thing)))

(define (add-file root dirstack thing)
  (let* ([dirs (get-directory-chain root dirstack)]
         ;; add the new file to the contents of the end of the dir chain
         [old-dir (first dirs)]
         [new-dir (directory (directory-name old-dir)
                             ;; preserve insertion order
                             (append (directory-contents old-dir)
                                     (list thing)))])
    ;; replace dir contents on all directories
    ;; in (rest dirs) to update the chain to the root node
    (for/fold ([d new-dir])
              ([current (rest dirs)])
      (directory (directory-name current)
                 (updated-contents (directory-contents current) d)))))

;;
;; main
;;
(define (aoc-7-1 input-file)
  (let ([root (parse-file input-file)])
    (foldl + 0
           (filter (lambda (n) (<= n 100000))
                   (map directory-size (filter-directories root))))))

(define (aoc-7-2 input-file)
  (let* ([root (parse-file input-file)]
         [total-used (directory-size root)]
         [unused (- 70000000 total-used)]
         [target-size (- 30000000 unused)]
         [dirs (filter-directories root)])
    (displayln (format "used:~a  unused:~a  target:~a"
                       total-used unused target-size))
    (first (sort (filter (lambda (n) (>= n target-size))
                         (map directory-size dirs))
                 <))))
                  

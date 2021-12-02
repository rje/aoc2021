#lang racket

(require racket/struct)

(struct command (direction amount)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'command)
      (lambda (obj) (list (command-direction obj) (command-amount obj)))))])

(define (string->command to-convert)
  (let* ([split (string-split to-convert)]
         [dir (first split)]
         [amount (string->number (last split))])
    (command dir amount)))

(define (read-list-of-commands path)
  (map string->command (file->lines path)))

(define (command-forward cmd)
  (equal? (command-direction cmd) "forward"))

(define (command-down cmd)
  (equal? (command-direction cmd) "down"))

(define (command-up cmd)
  (equal? (command-direction cmd) "up"))

(define (problem-1)
  (let* ([commands (read-list-of-commands "part_1.txt")]
         [forward (filter command-forward commands)]
         [down (filter command-down commands)]
         [up (filter command-up commands)])
    (begin
    (println
     (*
      (foldl + 0 (map command-amount forward))
      (- (foldl + 0 (map command-amount down))
         (foldl + 0 (map command-amount up)))
    )))))

(problem-1)

(define (problem-2)
  (let* ([commands (read-list-of-commands "part_1.txt")]
         [horiz 0]
         [aim 0]
         [depth 0])
    (begin
    (for/list ([cmd commands])
      (case (command-direction cmd)
        [("forward") (begin
                       (set! horiz (+ horiz (command-amount cmd)))
                       (set! depth (+ depth (* aim (command-amount cmd)))))]
        [("down") (set! aim (+ aim (command-amount cmd)))]
        [("up") (set! aim (- aim (command-amount cmd)))])
      )
    (println (* horiz depth)))))

(problem-2)
#lang racket

(define (read-lists-of-chars path)
  (map string->list (file->lines path)))

(define (stripe-data raw-data)
  (apply map list raw-data))

(define (is-one? chr) (equal? chr #\1))

(define (get-counts striped-data)
  (map (lambda (lst)
         (let* ([total-ones (count is-one? lst)]
                [total-zeroes (- (length lst) total-ones)])
           (list total-zeroes total-ones)))
       striped-data))

(define (get-binary-as-list counts cmp)
  (map (lambda (pair) (if (cmp (first pair) (last pair)) 0 1)) counts))

(define (binary-list->number binary)
  (let ([reversed-binary (reverse binary)])
    (apply + (for/list ([i (length reversed-binary)])
               (* (expt 2 i) (list-ref reversed-binary i))))))

(define (get-value counts cmp)
  (let ([binary (get-binary-as-list counts cmp)])
    (binary-list->number binary)))

(define (problem-1)
  (let* ([data (read-lists-of-chars "part_1.txt")] ; load the data
         [striped-data (stripe-data data)] ; stripe by bit position
         [counts (get-counts striped-data)] ; get 0/1 counts per bit position
         [gamma (get-value counts >)]
         [epsilon (get-value counts <)])
    (println (* gamma epsilon))))

(problem-1)

(define (filter-entries data cmp)
  (begin
  (for ([i (length (first data))]) ; for each bit position
    (let* ([striped-data (stripe-data data)] ; stripe the remaining data by bit position
           [counts (get-counts striped-data)] ; get 0/1 counts per bit position
           [relevant-count (list-ref counts i)] ; get the count for the index we are evaluating
           [filter-bit (if (cmp (last relevant-count) (first relevant-count)) #\1 #\0)]) ; determine what we should filter by
      (if (> (length data) 1)
          (set! data (filter (lambda (entry) (equal? filter-bit (list-ref entry i))) data))
          0)))
  (first data)))

(define (char->number char)
  (- (char->integer char) (char->integer #\0)))

(define (problem-2)
  (let* ([data (read-lists-of-chars "part_1.txt")]
         [oxygen-binary (map char->number (filter-entries data >=))]
         [co2-binary (map char->number (filter-entries data <))])
    (println (*
              (binary-list->number oxygen-binary)
              (binary-list->number co2-binary)))))

(problem-2)
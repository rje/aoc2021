#lang racket

(define (read-list-of-ints path)
  (map string->number (file->lines path)))

(define (problem-1)
  (let ([data (read-list-of-ints "part_1.txt")])
    (count identity
           (for/list ([a data] [b (rest data)])
             (< a b)))))

(problem-1)

(define (calculate-windows vals)
  (for/list ([a vals] [b (rest vals)] [c (rest (rest vals))])
    (+ a b c)))

(define (problem-2)
  (let ([data (read-list-of-ints "part_1.txt")])
    (let ([windows (calculate-windows data)])
    (count identity
           (for/list ([a windows] [b (rest windows)])
             (< a b))))))

(problem-2)
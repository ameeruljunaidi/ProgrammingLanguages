#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

; put your code below

; Problem 1
; Write a function sequence that takes 3 arguments low, high, and stride, all assumed to be numbers.
; Further assume stride is positive. sequence produces a list of numbers from low to high (including
; low and possibly high) separated by stride and in sorted order. Sample solution: 4 lines.

(define (sequence lo hi stride)
  (if (> lo hi) null (cons lo (sequence (+ lo stride) hi stride))))


; Problem 2
; Write a function string-append-map that takes a list of strings xs and a string suffix and returns a
; list of strings. Each element of the output should be the corresponding element of the input appended
; with suffix (with no extra space between the element and suffix). You must use Racket-library
; functions map and string-append. Sample solution: 2 lines.

(define (string-append-map xs suffix)
  (map (lambda (string) (string-append string suffix)) xs))

; Problem 3
; Write a function list-nth-mod that takes a list xs and a number n. If the number is negative,
; terminate the computation with (error "list-nth-mod: negative number"). Else if the list is
; empty, terminate the computation with (error "list-nth-mod: empty list"). Else return the i-th
; element of the list where we count from zero and i is the remainder produced when dividing n by the
; list’s length. Library functions length, remainder, car, and list-tail are all useful – see the Racket
; documentation. Sample solution is 6 lines.

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

; Problem 4
; Write a function stream-for-n-steps that takes a stream s and a number n. It returns a list holding
; the first n values produced by s in order. Assume n is non-negative. Sample solution: 5 lines. Note:
; You can test your streams with this function instead of the graphics code.

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([pr (s)])
        (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))

; Problem 5
; Write a stream funny-number-stream that is like the stream of natural numbers (i.e., 1, 2, 3, ...)
; except numbers divisble by 5 are negated (i.e., 1, 2, 3, 4, -5, 6, 7, 8, 9, -10, 11, ...). Remember a stream
; is a thunk that when called produces a pair. Here the car of the pair will be a number and the cdr will
; be another stream.

(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= 0 (remainder x 5))
                    (cons (- x) (lambda () (f (+ x 1))))
                    (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

; Problem 6
; Write a stream dan-then-dog, where the elements of the stream alternate between the strings "dan.jpg"
; and "dog.jpg" (starting with "dan.jpg"). More specifically, dan-then-dog should be a thunk that
; when called produces a pair of "dan.jpg" and a thunk that when called produces a pair of "dog.jpg"
; and a thunk that when called... etc. Sample solution: 4 lines.

(define dan-then-dog
  (letrec ([dan (lambda () (cons "dan.jpg" dog))]
           [dog (lambda () (cons "dog.jpg" dan))])
    dan))

; Problem 7
; Write a function stream-add-zero that takes a stream s and returns another stream. If s would
; produce v for its i-th element, then (stream-add-zero s) would produce the pair (0 . v) for its i-th element.
; Sample solution: 4 lines. Hint: Use a thunk that when called uses s and recursion.
; Note: One of the provided tests in the file using graphics uses (stream-add-zero dan-then-dog)
; with place-repeatedly.

(check-equal? (stream-for-n-steps ones 2) (list 1 1) "stream-for-n-steps test")
(check-equal? (stream-for-n-steps (stream-add-zero ones) 1) (list (cons 0 1)) "stream-add-zero test")
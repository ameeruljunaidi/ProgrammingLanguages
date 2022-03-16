#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

; put your code below

; Problem 1

(define (sequence lo hi stride)
  (if (> lo hi) null (cons lo (sequence (+ lo stride) hi stride))))


; Problem 2

(define (string-append-map xs suffix)
  (map (lambda (string) (string-append string suffix)) xs))

; Problem 3

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

; Problem 4

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([pr (s)])
        (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))

; Problem 5

(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= 0 (remainder x 5))
                    (cons (- x) (lambda () (f (+ x 1))))
                    (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

; Problem 6

(define dan-then-dog
  (letrec ([dan (lambda () (cons "dan.jpg" dog))]
           [dog (lambda () (cons "dog.jpg" dan))])
    dan))

; Problem 7

(define (stream-add-zero s)
  (letrec ([pr (s)]
           [f (lambda (x) (cons (cons 0 (car pr)) (lambda () (f (cdr pr)))))])
    (lambda () (f s))))


; Problem 8

(define (cycle-lists xs ys)
  (letrec ([f (lambda (x)
                (cons (cons (list-nth-mod xs x) (list-nth-mod ys x)) (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

; Problem 9

(define (vector-assoc v vec)
  (letrec ([find-v (lambda (i)
                     (cond [(= i (- (vector-length vec) 1)) #f]
                           [(equal? (car (vector-ref vec i)) v) (vector-ref vec i)]
                           [#t (find-v (+ i 1))]))])
    (find-v 0)))

; Problem 10
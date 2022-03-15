#lang racket

(provide (all-defined-out))

; defining stuff
(define s "hello")

; basic function
(define (cube x)
  (* x x x))

; conditionsls
(define (pow x y)
  (if (= y 0)
      1
      (* x (pow x (- y 1)))))

; summing everything in a list
(define (sum xs)
  (if (null? xs)
      0
      (+ (car xs) (sum (cdr xs)))))

; summing list with coniditionals (only except numbers)
(define (sum-numbers xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum-numbers (cdr xs)))]
        [#t (+ sum-numbers (car xs) (sum-numbers (cdr xs)))]))

; since racket is dynamically typed can create function that takes anything
(define (sum-any xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum-any (cdr xs)))]
        [#t (sum-any (cdr xs))]))

; appending list together
(define appended-list (append (list 1 2 3) (list 4 5 6)))
; can also use the ' rather than typing out list
(define appended-list-2 (append '(1 2 3) '(4 5 6)))

; map function in racket
(define add-one-to-every (map (lambda (number) (+ 1 number)) '(1 2 3 4)))

(define (max-of-list xs)
  (cond [(null? xs) (error "list is empty")]
        [(null? (cdr xs)) (car xs)]
        [#t (let ([tlans (max-of-list (cdr xs))])
              (if (> tlans (car xs))
                  tlans
                  (car xs)))]))

; mutable stuff
(define mutable 4)
(set! mutable 8)

; mutable cons
(define mutable-cons (mcons "hello" "world"))
(set-mcar! mutable-cons "what up")
(set-mcdr! mutable-cons "aj")

; defining streams
; (define (f x) (cons x (lambda () (f (+ x 1)))))
; (define nats (lambda () (f 1)))

; better way to do it
; (define nats
;   (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
;     (lambda () (f 1))))

; getting the power of twos
; (define powers-of-two
;   (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
;     (lambda () (f 1))))

(define (stream-maker fn arg)
  (letrec ([f (lambda (x) (cons x (lambda () (f (fn x arg)))))])
    (lambda () (f 1))))

; nats
(define nats (stream-maker + 1))
; powers-of-two
(define power-of-two (stream-maker * 2))

(define three (car ((cdr ((cdr (nats)))))))
(define four (car ((cdr ((cdr (power-of-two)))))))
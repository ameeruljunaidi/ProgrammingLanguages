#lang racket
(require "hw4.rkt")
(require rackunit)

(define ones (lambda () (cons 1 ones)))
(define a 2)

(define tests
  (test-suite
   "Sample tests for Assignment 4"

   (check-equal? (sequence 0 5 1) (list 0 1 2 3 4 5) "Sequence test")

   (check-equal? (string-append-map
                  (list "dan" "dog" "curry" "dog2")
                  ".jpg") '("dan.jpg" "dog.jpg" "curry.jpg" "dog2.jpg") "string-append-map test")

   (check-equal? (list-nth-mod (list 0 1 2 3 4) 2) 2 "list-nth-mod test")
   (check-equal? (list-nth-mod (list 6 7 8 9 10) 2) 8 "list-nth-mod test")

   (check-equal? (stream-for-n-steps ones 2) (list 1 1) "stream-for-n-steps test")

   ))

(require rackunit/text-ui)
(run-tests tests)
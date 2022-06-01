#lang racket

(define (square n) (* n n))
(define (cube n) (* n n n))
(define (inc n) (+ n 1))

;A1

(define (twice f)
  (lambda (x) (f (f x))))

;A2
;f
;f needs to be bound to the result of an expression (not to the definition of a function)
(define f 1)
;output: 1

;(g)
;g needs to be bound to a function that takes no arguments
(define (g) 2)
;output: 2

;(h 3)
;h needs to be bound to a function that takes exactly 1 argument
(define (h x) x)
;output: value of x -> 3

;((i))
;i needs to be an argumentless function that returns an argumentless function
(define (i)
  (lambda () 4))
;output: 4

;(((j)) 3)
;j needs to be an argumentless function, that returns an argumentless function,
;that returns a function that takes one argument
(define (j)
  (lambda ()
    (lambda (x) x)))
;output: 4

;A3
(define (comp f g)
  (lambda (x)
    (f (g x))))

;A5
;(a b) car = wert a, cdr = pointer zu nächstem element (wert/pointer paar) in liste
;a
(define list5a
  (list 1 3 (list 5 7) 9))
;(car (cdr (car (cdr (cdr list5a)))))

;b
(define list5b
  (list (list 7)))
;(car (car list5b))

;c
(define list5c
  (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
;(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list5c))))))))))))

;A6
(define list6
  (list 0 1 2 3 4 5))

(define (getFirst n xs)
  (display n)
  (println xs)
  (if (or (= n 0) (empty? xs))
      empty
      (cons (car xs) (getFirst (- n 1) (cdr xs)))))

(getFirst 3 list6)

#lang racket
(define (isEven m)
  (if (= (modulo m 2) 0)
      "even"
      "odd"))
#lang racket
;A1

(define (leave? x)
  (not (pair? x)))

(define (leaflist sub-tree)
  (cond [(null? sub-tree) null]
        [(leave? sub-tree) (list sub-tree)]
        [else (append (leaflist (first sub-tree)) (leaflist (rest sub-tree)))]))

(define x (list (list 1 2) (list 3 4)))
(leaflist (list x x))

;A2

(define (nestingLevel xs depth)
  (cond [(null? xs) 0]
        [(leave? xs) depth]
        [else (max (nestingLevel (first xs) (+ depth 1)) (nestingLevel (rest xs) depth))]))

(nestingLevel (list 1 2 3) 0)
(nestingLevel (list (list 1 2) 3 4) 0)
(nestingLevel (list 1 (list 2 (list 3 4))) 0)
(nestingLevel (list (list 1 2) (list 3 4)) 0)

;A3

(define (combine op xs ys)
  (map op xs ys))

(combine + (list 1 2 3) (list 4 5 6))
(combine list (list 1 2 3) (list 4 5 6))

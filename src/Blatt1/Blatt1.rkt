#lang racket

;____________A1 a

(define a 3)

(define b (+ a 1))

(+ a b (* a b))
;19

(= a b)
;#f

(if (and (> b a) (< b (* a b)))
    b
    a)
;4

;(-4)
;error

(- 4)
; -4

-4
; -4

- 4
;#<procedure:-> 4 wird getrennt betrachtet

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;16

(+ 2 (if (> b a) b a))
;6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
;16

((if (< a b) + -) a b)
;7

;__________A1 b

(define (sign x)
        (cond ((> x 0) 1)
              ((< x 0) -1)
              (else 0)))
         
;__________A2

(define (maxpow x y z)
        (cond ((and (< x y) (< x z)) (+ (* y y) (* z z)))
              ((and (< y x) (< y z)) (+ (* x x) (* z z)))
              ((and (< z x) (< z y)) (+ (* x x) (* y y)))))

;__________A3
(define (and1)
        (and (< 0 1)
             (iwashere)))
(define (and2)
        (and (> 0 1)
             (iwashere)))

(define (or1)
        (or (< 0 1)
             (iwashere)))

(define (iwashere)
        (display "i was here")
        #t)

;__________A4

#|Programm führt durch myif eine endlosschleife aus.
Der Grund hierfür ist, dass Racket funktionsargumente evaluiert,
bevor der body ausgeführt wird. Dadurch wird cond nie ausgeführt,
da das Evaluieren der myif Argumente (der else-clause) wiederum
sqrt-iter aufruft und daduruch auch wieder myif evaluiert werden
muss, bevor cond überprüft werden kann. |#

(define (myif predicate then-clause else-clause)
        (cond (predicate then-clause)
              (else else-clause)))

  (define (square x)
    (* x x))
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (/ (+ (/ x guess) guess) 2))
  (define (sqrt-iter guess x)
    (myif (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  


;__________A5

#|
(define (square x)
    (* x x))
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (/ (+ (/ x guess) guess) 2))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 2)
|#
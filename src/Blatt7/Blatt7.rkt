#lang racket


(define (startAt x)
  (define (incrementBy y)
    (+ x y))
  incrementBy)

(define closure1 (startAt 1))
(define closure5 (startAt 5))

(closure1 9)
(closure5 9)

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (display-stream-limit s limit)
  (define (d s lim)
  (if (or (= lim 0) (stream-empty? s))
      "...?"
      (~a (stream-first s) "," (d (stream-rest s) (- lim 1)))))
  (d s limit))

(define (divisible? n x)
  (zero? (remainder n x)))


;A1

(define (sieve stream)
  (stream-cons
   (stream-first stream)
   (sieve (stream-filter
           (lambda (x) (not (divisible? x (stream-first stream))))
           (stream-rest stream)))))


(define primes
  (sieve (integers-starting-from 2)))


(print "100  :")
(time (stream-ref primes 100))
;cpu time: 16
(print "598  :")
(time (stream-ref primes 598))
;cpu time: 1063
(print "600  :")
(time (stream-ref primes 600))
;cpu time: 0
(print "599  :")
(time (stream-ref primes 599))
;cpu time: 0
(print "700  :")
(time (stream-ref primes 700))
;cpu time: 282
(print "5  :")
(time (stream-ref primes 5))

;Aufgrund von memoization werden Zwischenergebnisse gecached und damit müssen die
;Berechnungen bis zu einer bestimmten Primzahl immer nur beim ersten Aufruf durchgeführt werden.
;Wenn alle Primzahlen innerhalb 600 bereits einmal berechnet wurden, können diese Zwischenergebnisse
;bei erneuem Bedarf verwendet werden. Somit werden erst bei Übersteigen des Werts 600 neue Berechnungen
;nötig sein und alle Berechnungen bis zu diesem Wert extrem schnell laufen.

;A2

;s-Core
(define empty-s 'S-EMPTY-STREAM)

(define (s-empty? s)
  (eq? s empty-s))

(define-syntax-rule (s-cons a b)
  (cons a (delay b)))


(define (s-first s)
  (car s))

(define (s-rest s)
  (force (cdr s)))

(define (s-display s)
  (if (s-empty? s)
      ""
      (~a (s-first s) "," (s-display (s-rest s)))))

(define (s-filter p s)
  (cond ((s-empty? s) empty-s)
        ((p (s-first s))
         (s-cons (s-first s)
               (s-filter p (s-rest s))))
        (else (s-filter p (s-rest s)))))

(define (s-range low high)
  (if (> low high)
      empty-s
      (s-cons
       low
       (s-range (+ low 1) high))))

(define (s-ref s n)
  (if (= n 0)
      (s-first s)
      (s-ref (s-rest s) (- n 1))))


(define (s-sieve stream)
  (s-cons
   (s-first stream)
   (s-sieve (s-filter
           (lambda (x) (not (divisible? x (s-first stream))))
           (s-rest stream)))))

(define (s-integers-starting-from n)
  (s-cons n (s-integers-starting-from (+ n 1))))

(define s-primes
  (s-sieve (s-integers-starting-from 2)))

;(s-display (s-sieve (range 1 20)))

(print "100  :")
(time (s-ref s-primes 100))
;cpu time: 47
(print "598  :")
(time (s-ref s-primes 598))
;cpu time: 188
(print "600  :")
(time (s-ref s-primes 600))
;cpu time: 0
(print "599  :")
(time (s-ref s-primes 599))
;cpu time: 0
(print "700  :")
(time (s-ref s-primes 700))
;cpu time: 62
(print "5  :")
(time (s-ref s-primes 5))
;cpu time: 0


;A5

(define (funcList n)
  (define (buildList i)
    (if (= n i)
        null
        (cons (lambda (x) (+ x i)) (buildList (+ i 1)))))
  (buildList 0))

(funcList 5)

((list-ref (funcList 5) 0) 7)
((list-ref (funcList 5) 2) 7)
;((list-ref (funcList 5) 5) 7)

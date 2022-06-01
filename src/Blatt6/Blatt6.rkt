#lang racket
(require racket/stream)

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))


(define (display-stream-limit s limit)
  (define (d s lim)
  (if (or (= lim 0) (stream-empty? s))
      "...?"
      (~a (stream-first s) "," (d (stream-rest s) (- lim 1)))))
  (d s limit))

;1
(define (toCelsius-stream xs)
 (stream-map (lambda (x) (*  x (/ 5 9) ))
             (stream-map (lambda (x) (* (- x 32) ))
              xs)))

(stream->list (toCelsius-stream (stream 0 100)))
(display-stream-limit (toCelsius-stream (in-range 0 10)) 10)

;2
(define s1 (stream))
(define s2 empty-stream)

(stream->list s1)
(stream-empty? s1)
(stream->list s2)
(stream-empty? s2)

;3
(define (pow2-starting-from x)
  (stream-filter (lambda (y) (integer? (/ (log y) (log 2)))) (integers-starting-from x)))

(display-stream-limit (pow2-starting-from 64) 5)

#|
(define (powersOf2 x)
  (stream-cons x (powersOf2 (* x 2))))

(display-stream-limit (powersOf2 64) 5)
|#

;4
(define (stream-add s1 s2)
  (if (stream-empty? s1)
  empty-stream
  (stream-cons (+ (stream-first s1)(stream-first s2))
               (stream-add (stream-rest s1)(stream-rest s2)))))

(display-stream-limit (stream-add integers integers) 5)
(display-stream-limit (stream-add (stream 10 11 12) integers) 5)

;5

(define (mySum s)
  (stream-cons (stream-first s) (stream-add (stream-rest s) (mySum s))))

(display-stream-limit (mySum integers) 5)

#|
(define (mySum s sum)
  (if (stream-empty? s)
  empty-stream
  (stream-cons (+ (stream-first s) sum) (mySum (stream-rest s) (+ sum (stream-first s))))))
|#
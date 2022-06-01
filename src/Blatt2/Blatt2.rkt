#lang racket


;A1
(define (powerCloseTo b n e)
  (if (> (expt b e) n)
      e
      (powerCloseTo b n (+ e 1))))

;A2
(require racket/trace)

(define (fibRec n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibRec (- n 1))
                 (fibRec (- n 2))))))

(define (fibIt n)
  (fib-iter 1 0 n))

(define (fib-iter a b n)
  (if (= n 0)
      b
      (fib-iter (+ a b) a (- n 1))))

(trace fibRec)
(trace fib-iter)

;A3
(define (myLoop f n)
  (if (void? f)
      (display "not a function")
      (for ([i n])
        (f))))

;A4
(define (square n) (* n n))
(define (cube n) (* n n n))

(define (min-fx-gx f g x)
  (min (f x)
       (g x)))

(define (combine-fx-gx c f g x)
  (c (f x)
     (g x)))

;A5
(define (f g)
 (g 5))

#|(f square)
Bei Aufruf von f wird square wird anstelle von g eingesetzt.
Dadurch wird die Funktion (square 5) aufgerufen
und als Ergebnis von f zurückgegeben.|#

#|(f (lambda (x) (* x (+ x 2))))
Die Lmada Definition ist (λ kw-formals body ...+)
Lamda erzeugt eine Funktion die als Argumente kw-formals annimmt.
In unserem Fall ist das x, welches von der generierten Lamda-
Funktion zurückgegeben wird, nachdem 2 addiert und mit der
x multipliziert wurde. Wenn also f mit der Lamdafunktion für g aufgerufen
wird, dann wird wie bei (f square) g ersetzt und 5 als Argument übergeben.
Die Lamdafunktion gibt dann (5+2)*5=35 zurück.

Würde unsere lamda Funktion mehre argumente (in kw-formals) definieren,
dann könnte sie nicht von unserem f aufgerufen werden,
weil dieses nur ein argument übergibt.|#

#|(f f)
Unserer Funktion f wird die gleiche Funktion f als Parameter übergeben.
Das stellt noch kein Problem dar, denn f soll sogar eine Funktion
als Argument bekommen. Nach dem Aufruf von f (mit Argument f),
wird g mit f ersetzt und folglich die Prozedur (f 5) aufgerufen.
Nun bekommt f aber als Argument eine Zahl und keine Funktion, so wie es
nötig wäre, damit der Folgebefehl funktionert. Denn hier wird nun wieder
g ersetzt, diesmal mit 5 und die Prozedur (5 5) wird ausgeführt.
Dadurch entsteht ein "application: not a procedure;" error, denn 5 ist
keine Prozedur.|#
#lang racket

; needs to be pretty-printed

(define (map f xs)
  (if
   (null? xs)
   '()
   (cons (f (car xs)) (map f (cdr xs)))))

(define (filter p? xs)
  (if
   (null? xs)
   '()
   (if
    (p? (car xs))
    (cons (car xs) (filter p? (cdr xs)))
      (filter p? (cdr xs)))
))

(define (concat xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (concat (cdr xs) ys))))


(define (quick-sort xs)
  (if
   (null? xs)
   '()
   (concat (quick-sort (filter (lambda (x) (< x (car xs))) (cdr xs))) (cons (car xs) (quick-sort (filter (lambda (x) (>= x (car xs))) (cdr xs)))))))

(define (reverse xs)
  (if (null? xs) '()
      (concat (reverse (cdr xs)) (cons (car xs) '()))))

(define (eq-lists? xs ys)
  (if (null? xs) (null? ys) ; you vs the guy she tells you not to worry about
      (and (eq? (car xs) (car ys)) (eq-lists? (cdr xs) (cdr ys)))))
 
(define (zip xs ys)
  (if (or (null? xs) (null? ys)) '()
     (cons (cons (car xs) (car ys)) (zip (cdr xs) (cdr ys)))))

(define (abacaba num)
  (if (eq? num 0) '(0)
      (concat (abacaba (- num 1)) (cons num (abacaba (- num 1))))))

(define (contains? xs x)
  (and (not (null? xs)) (or (eq? x (car xs)) (contains? (cdr xs) x))))

(define (range min max)
  (if (eq? min max) '()
      (cons min (range (+ min 1) max))))
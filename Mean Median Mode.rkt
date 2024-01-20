#lang racket

(define (sum lst)
  (if (null? lst) 0 (+ (car lst) (sum (cdr lst)))))

(define (mean lst)
  (/ (sum lst) (length lst)))

(define (sort-list lst)
  (if (null? lst) '()
      (insert (car lst) (sort-list (cdr lst)))))

(define (insert x lst)
  (if (null? lst) (list x)
      (if (< x (car lst)) (cons x lst)
          (cons (car lst) (insert x (cdr lst))))))

(define (median lst)
  (let ([s (sort-list lst)] [len (length lst)])
    (if (odd? len) (list-ref s (quotient len 2))
        (/ (+ (list-ref s (quotient len 2))
              (list-ref s (- (quotient len 2) 1))) 2))))

(define (count x lst)
  (if (null? lst) 0
      (if (= x (car lst)) (+ 1 (count x (cdr lst)))
          (count x (cdr lst)))))

(define (mode lst)
  (define (mode-helper lst current max-count max-value)
    (if (null? lst) max-value
        (let ([current-count (count (car lst) lst)])
          (if (> current-count max-count)
              (mode-helper (cdr lst) (car lst) current-count (car lst))
              (mode-helper (cdr lst) current max-count max-value)))))
  (mode-helper lst 0 0 0))

(define (stats lst)
  (list (mean lst) (median lst) (mode lst)))

; Example usage:
(stats '(1 2 3 4 5 5 3 2 2 1 1 1 4 4 5 5 6))
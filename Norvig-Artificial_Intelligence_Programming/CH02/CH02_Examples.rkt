#lang racket

; Chapter 2 context free generative grammars


; ==========================
; Book Work
; ==========================

; Create mappend
(define (mappend f lst)
  (flatten (list (map f lst))))

(define (rule-lhs rule)
  (first rule))

(define (rule-rhs rule)
  (cond
    [(not rule) null]
    [else (rest rule)]))




; ====================================================
; Problems
; ====================================================


; Problem 2.3

(define equation-grammar
  '((equation (variable equality variable operator variable))
    (variable x y z w 5)
    (equality = > < >= <=)
    (operator + - / *)))

(define (rewrites category)
  (rule-rhs (assoc category equation-grammar)))

(define (generate phrase)
  (if (list? phrase)
      (mappend generate phrase)
      (let ((choices (rewrites phrase)))
        (if (null? choices)
            (list phrase)
            (generate (list-ref choices (random (length choices))))))))

;(generate 'equation)

; Problem 2.2
(define (non-terminal phrase)
  (not (null? (rewrites phrase))))

(define (generate2 phrase)
  (if (list? phrase)
      (mappend generate2 phrase)
      (if (non-terminal phrase)
          (let ((choices (rewrites phrase)))
          (generate2 (list-ref choices (random (length choices)))))
          (let ((choices (rewrites phrase)))
            (list phrase)))))
; (generate2 'equation)


; Problem 2.4

(define (cross-product f x y)
  (f x y))

(define (combine-all xlist ylist)
  (apply append (map (lambda (y)
                       (map (lambda (x)
                              (append (list x) (list y))) xlist))
           ylist)))


(combine-all append '(1 2 3) '(a b c))

; Although I can define a cross product, I find it difficult to use different
; operators than append.  I run into the problem that not everything is a list
; so append only applies when I explicityl create lists from x and y.



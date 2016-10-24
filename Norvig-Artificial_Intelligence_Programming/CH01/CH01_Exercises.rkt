#lang racket

; Problem 1.1

; Using a Conditional statement
(define (last-name lst)
  (cond
    [(equal? (last lst) "Jr.")
     (first (rest (reverse lst)))]
    [(equal? (last lst) "MD")
     (first (rest (reverse lst)))]
    [(equal? (last lst) "Esquire")
     (first (rest (reverse lst)))]
    [else (last lst)]))

; Using or form
(define (last-name2 lst)
  (if (or (equal? (last lst) "Jr.")
          (equal? (last lst) "MD")
          (equal? (last lst) "Esquire"))
      (first (rest (reverse lst)))
      (last lst)))

; Using a pattern match
(define (last-name3 lst)
  (match (last lst)
    ["Jr." (first (rest (reverse lst)))]
    ["MD" (first (rest (reverse lst)))]
    ["Esquire" (first (rest (reverse lst)))]
    [_ (last lst)]))


(last-name (list "Dr." "Vivek" "Srikumar"))
(last-name2 (list "Dr." "Vivek" "Srikumar"))
(last-name3 (list "Dr." "Vivek" "Srikumar"))

(last-name (list "Micheal" "Malloney" "Jr."))
(last-name2 (list "Micheal" "Malloney" "Jr."))
(last-name3 (list "Micheal" "Malloney" "Jr."))



; Problem 1.2

(define (power a b)
  (if (equal? b 0)
      1
      (* a (power a (- b 1)))))

(power 3 3)


; Problem 1.3
; Count atoms is the length of all elements in a list including those
; in a sublist.  I will be working with lists though in the book it is
; expression.

(define (atom? x)
  "Define an is atom function"
  (and (not (null? x))
       (not (pair? x))))

(define (count-atoms exp)
  "Count atoms excluding nulls"
  (cond
    [(null? exp) 0]
    [(atom? exp) 1]
    [else (+ (count-atoms (rest exp)) (count-atoms (first exp)))]))

(count-atoms (list 1 2 3 4 5))
(count-atoms `(a (b a) () c))

; Problem 1.4


; Problem 1.5

(define (dot a b)
  (foldl (lambda (e v)
           (+ e v))
         0
         (map (lambda (x y) (* x y))
              a
              b)))

(dot (list 1 2 3) (list 1 2 3))






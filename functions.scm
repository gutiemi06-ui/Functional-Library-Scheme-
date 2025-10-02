;; functions.scm
;; Functional Programming Utilities - Scheme
;; Author: Emiliano Gutierrez

;; Recursive set union
(define (union s1 s2)
  (cond
    [(null? s1) s2]
    [(member (car s1) s2) (union (cdr s1) s2)]
    [else (cons (car s1) (union (cdr s1) s2))]))

;; Recursive set intersection
(define (intersection s1 s2)
  (cond
    [(or (null? s1) (null? s2)) '()]
    [(member (car s1) s2) (cons (car s1) (intersection (cdr s1) s2))]
    [else (intersection (cdr s1) s2)]))

;; Recursive set difference (s1 - s2)
(define (difference s1 s2)
  (cond
    [(null? s1) '()]
    [(member (car s1) s2) (difference (cdr s1) s2)]
    [else (cons (car s1) (difference (cdr s1) s2))]))

;; Recursive map implementation
(define (my-map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst)) (my-map f (cdr lst)))))

;; Recursive filter implementation
(define (my-filter pred lst)
  (cond
    [(null? lst) '()]
    [(pred (car lst)) (cons (car lst) (my-filter pred (cdr lst)))]
    [else (my-filter pred (cdr lst))]))

;; Recursive fold (reduce)
(define (my-fold f init lst)
  (if (null? lst)
      init
      (f (car lst) (my-fold f init (cdr lst)))))

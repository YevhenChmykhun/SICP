#lang racket

;; Exercise 2.53

(list 'a 'b 'c)
;; => (a b c)

(list (list 'george))
;; => ((george))

(cdr '((x1 x2) (y1 y2)))
;; => ((y1 y2))

(cadr '((x1 x2) (y1 y2)))
;; => (y1 y2)

(pair? (car '(a short list)))
;; => #f

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'red '((red shoes) (blue socks)))
;; => #f

(memq 'red '(red shoes blue socks))
;; => (red shoes blue socks)

;; Exercise 2.54
;; reimplementation is needed. maybe flat lists before apply equal?

(define equal?
  (lambda (a b)
   (if (and (pair? a) (pair? b)) 
       (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))) 
       (eq? a b))))

(equal? '(this is a list) '(this is a list))
;; => #t
(equal? '(this is a list) '(this (is a) list))
;; => #f

;; Exercise 2.55

;; we can rewrite (car ''abracadabra) as (car (quote (quote (abracadabra))))
;; the first quote quotes the following expression which becomes a list of two elements

(car ''abracadabra)
;; => quote

(cdr ''abracadabra)
;; => (abracadabra)
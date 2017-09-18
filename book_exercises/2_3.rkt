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

;; Exercise 2.56

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation
                                      (base exp)
                                      (make-sum (exponent exp) -1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp)
       (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (expt b e))
        (else (list '** b e))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(deriv '(* 3 (** x 2)) 'x)
;; => (* 3 (* 2 x))

;; Exercise 2.57

(define (deriv1 exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend1 exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand1 exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand1 exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation
                                      (base exp)
                                      (make-sum (exponent exp) -1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) 
          (accumulate op initial (cdr sequence))))) 

(define (augend1 s)
  (accumulate make-sum 0 (cddr s)))

(define (multiplicand1 p)
  (accumulate make-product 1 (cddr p)))

(deriv1 '(* x y (+ x 3)) 'x)
;; => (+ (* x y) (* y (+ x 3)))
#lang racket

;; Exercise 2.73 a

;; Procedures number? and same-variable? are simple predicates.
;; They do not have a "type tag" therefore they cannot participate
;; in the data-directed dispatch.

;; Exercise 2.73 b c

(define (put)
  '())

(define (get)
  '())

(define (install-deriv-package)

  ;; internal procedures
  (define (=number? exp num)
    (and (number? exp)
         (= exp num)))
  
  (define (addend s) (cadr s))

  (define (augend s) (caddr s))
  
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  
  (define (sum-deriv exp var)
    (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))

  (define (multiplier p) (cadr p))

  (define (multiplicand p) (caddr p))

  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  
  (define (product-derive exp var)
    (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))

  (define (base e) (cadr e))

  (define (exponent e) (caddr e))
  
  (define (make-exponentiation b e)
    (cond ((=number? e 0) 1)
          ((=number? e 1) b)
          ((and (number? b) (number? e)) (expt b e))
          (else (list '** b e))))

  (define (exponentiation-derive exp var)
    (make-product (make-product (exponent exp)
                                (make-exponentiation
                                 (base exp)
                                 (make-sum (exponent exp) -1)))
                  (deriv (base exp) var)))

  ;; interface to the rest of the system
  (put 'deriv '+ sum-deriv)
  (put 'deriv '* product-derive)
  (put 'deriv '** exponentiation-derive)
  'done)

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))

;; Exercise 2.73 d

;; The signature of the put procedure has to be changed accordingly.


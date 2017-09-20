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
         (make-sum (deriv1 (addend exp) var)
                   (deriv1 (augend1 exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv1 (multiplicand1 exp) var))
           (make-product (deriv1 (multiplier exp) var)
                         (multiplicand1 exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation
                                      (base exp)
                                      (make-sum (exponent exp) -1)))
                       (deriv1 (base exp) var)))
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

;; Exercise 2.58 a

(define (deriv2 exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum?2 exp)
         (make-sum2 (deriv2 (addend2 exp) var)
                    (deriv2 (augend exp) var)))
        ((product?2 exp)
         (make-sum2
           (make-product2 (multiplier2 exp)
                          (deriv2 (multiplicand exp) var))
           (make-product2 (deriv2 (multiplier2 exp) var)
                          (multiplicand exp))))
        ((exponentiation?2 exp)
         (make-product2 (make-product2 (exponent exp)
                                       (make-exponentiation2
                                        (base2 exp)
                                        (make-sum2 (exponent exp) -1)))
                       (deriv2 (base2 exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (make-sum2 a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product2 m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (make-exponentiation2 b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (expt b e))
        (else (list b '** e))))

(define (sum?2 x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend2 s) (car s))

(define (product?2 x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier2 p) (car p))

(define (exponentiation?2 x)
  (and (pair? x) (eq? (cadr x) '**)))

(define (base2 e) (car e))

(deriv2 '(x + (3 * (x + (y + 2)))) 'x)
;; => 4

;; Exercise 2.58 b
;; MISSING

;; Exercise 2.59

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((not (element-of-set? (car set1) set2))        
         (cons (car set1)
               (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))

(union-set '(1 3 2) '(4 5 3))
;; => (1 2 4 5 3)

;; Exercise 2.60

(define (adjoin-set2 x set)
  (cons x set))

(define (union-set2 set1 set2)
  (append set1 set2))

(define (intersection-set2 set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set2 (cdr set1) set2)))
        (else (intersection-set2 (cdr set1) set2))))

(intersection-set2 '(2 3 2 1 3 2 2) '(9 4 3 6 3 2))

;; Exercise 2.61

(define (adjoin-set3 x set)
  (if (or (null? set) (> (car set) x))
      (cons x set)
      (cons (car set) (adjoin-set3 x (cdr set)))))

(adjoin-set3 3 '(1 2 4 5))
;; => (1 2 3 4 5)

(adjoin-set3 3 '(1 2))
;; => (1 2 3)

(adjoin-set3 3 '(4 5))
;; => (3 4 5)

(adjoin-set3 3 '())
;; => (3)

;; Exercise 2.62

(define (union-set3 set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((a (car set1))
               (b (car set2)))
           (if (< a b)
               (cons a (union-set3 (cdr set1) set2))
               (cons b (union-set3 set1 (cdr set2))))))))

(union-set3 '() '(3 4 5 6))
;; => (3 4 5 6)

(union-set3 '(1 2 3 4) '())
;; => (1 2 3 4)

(union-set3 '(1 2 3 4) '(3 4 5 6))
;; => ( 1 2 3 3 4 4 5 6)

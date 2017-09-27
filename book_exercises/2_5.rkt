#lang racket

;; Exercise 2.77

;; If we assume that the procedure magnitude for complex numbers
;; is implemented and is added in install-complex-package then
;; apply-generic will be invoked twice. First to get magnitude
;; of complex and second to get magnitude of rectangular.

;; Exercise 2.78

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

;; Exercise 2.79

(define (install-scheme-number-package)
   (put 'equ? '(scheme-number scheme-number) =)
   'done)

(define (install-rational-package)
   (define (equ? x y)
     (and (= (numer x) (numer y))
          (= (denom x) (denom y))))
  
   (put 'equ? '(rational rational) equ?)
   'done)

(define (install-complex-package)
   (define (equ? x y)
     (and (= (real-part x) (real-part y))
          (= (imag-part x) (imag-part y))))
  
   (put 'equ? '(complex complex) equ?)
   'done)

(define (equ? x y) (apply-generic 'equ? x y))

;; Exercise 2.80

(define (install-scheme-number-package)
  (define (=zero? x)
     (= x 0))
  
   (put '=zero? '(scheme-number) =zero?)
   'done)

(define (install-rational-package)
   (define (=zero? x)
     (= (numer x) 0))
  
   (put '=zero? '(rational) =zero?)
   'done)

(define (install-complex-package)
   (define (equ? x)
     (= (real-part x) (imag-part x) 0))
  
   (put '=zero? '(complex) =zero?)
   'done)

(define (=zero? x) (apply-generic '=zero? x))
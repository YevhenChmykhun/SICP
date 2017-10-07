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

;; Exercise 2.81 a

;; If we define a generic exponentiation operation and
;; put a procedure for exponentiation in the Scheme-number package
;; and then call apply-generic with two arguments of type scheme-number
;; it will actually work. But if we call apply-generic with two arguments of
;; type complex it will not be able to find a matching procedure for an
;; operation called with a pair of arguments of type complex. Then it will
;; apply a coercion procedure to one of the args and try to call apply-generic
;; onc again which will lead to an infitite loop;

;; Exercise 2.81 b

;; apply-generic works correctly as is. If an appropriate procedure
;; cant't be found in the coercion table apply-generic will raise an error.

;; Exercise 2.81 c

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (not (eq? type1 type2))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (error "No method for these types"
                                    (list op type-tags)))))
                    (error "No method for these types"
                           (list op type-tags))))
                (error "No method for these types"
                     (list op type-tags)))))))

;; Exercise 2.82

;; MISSING

;; Exercise 2.83

;; MISSING

;; Exercise 2.84

;; MISSING

;; Exercise 2.85

;; MISSING

;; Exercise 2.86

;; MISSING

;; Exercise 2.87

;; MISSING

;; Exercise 2.88

;; MISSING

;; Exercise 2.89

;; MISSING

;; Exercise 2.90

;; MISSING

;; Exercise 2.91

;; MISSING

;; Exercise 2.92

;; MISSING

;; Exercise 2.93

;; MISSING

;; Exercise 2.94

;; MISSING

;; Exercise 2.95

;; MISSING

;; Exercise 2.96

;; MISSING

;; Exercise 2.97

;; MISSING

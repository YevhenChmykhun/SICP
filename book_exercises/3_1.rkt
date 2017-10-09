#lang racket

;; Exercise 3.1

(define (make-accumulator value)
  (lambda (increment)
    (begin (set! value (+ value increment))
           value)))

(define A (make-accumulator 5))

(A 10)
;; => 15

(A 10)
;; => 25

;; Exercise 3.2

(define (make-monitored f)
  (define count 0)
  (lambda (param)
    (cond ((eq? param 'how-many-calls?) count)
          ((eq? param 'reset-count) (set! count 0))
          (else (begin (set! count (+ count 1))
                       (f param))))))

(define s (make-monitored sqrt))

(s 100)
;; => 10

(s 25)
;; => 5

(s 'how-many-calls?)
;; => 2

(s 'reset-count)

(s 'how-many-calls?)
;; => 0

;; Exercise 3.3

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch entered-password m)
    (if (eq? password entered-password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (lambda (amount)
          (displayln "Incorrect password"))))
  dispatch)

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
;; => 60

((acc 'some-other-password 'deposit) 50)
;; => Incorrect password

;; Exercise 3.4

(define (make-secure-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define access-attempts 0)
  
  (define (dispatch entered-password m)
    (if (not (eq? password entered-password))
        (begin (set! access-attempts (+ access-attempts 1))
               (if (> access-attempts 3)
                   (lambda (amount)
                     (displayln "Cops are on their way, you little rascal!"))
                   (lambda (amount)
                     (displayln "Incorrect password"))))
        (begin (set! access-attempts 0)
               (cond ((eq? m 'withdraw) withdraw)
                     ((eq? m 'deposit) deposit)
                     (else (error "Unknown request -- MAKE-ACCOUNT"
                                  m))))))
  
  dispatch)

(define sec-acc (make-secure-account 100 'secret-password))

((sec-acc 'secret-password 'withdraw) 40)
;; => 60

((sec-acc 'some-other-password 'deposit) 50)
;; => Incorrect password
((sec-acc 'some-other-password 'deposit) 50)
;; => Incorrect password
((sec-acc 'some-other-password 'deposit) 50)
;; => Incorrect password
((sec-acc 'some-other-password 'deposit) 50)
;; => Incorrect password
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
;; => Cops are on their way, you little rascal!

;; Exercise 3.5

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high) 
  (let ((range (- high low))) 
    (+ low (* (random) range))))

(define (P x y) 
  (< (+ (expt (- x 5) 2) 
        (expt (- y 7) 2)) 
     (expt 3 2)))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (monte-carlo trials (lambda ()
                        (P (random-in-range x1 x2) 
                           (random-in-range y1 y2)))))

(define (estimate-pi trials)
  (/ (* (estimate-integral P 2.0 8.0 4.0 10.0 trials) 36) 
      9.0))

(estimate-pi 1000000)
;; => ~3.14

;; Exercise 3.6

(define random-init 47)

(define (rand-update x)
  (+ x 13))

(define rand
  (let ((x random-init))
    (define (dispatch message)
      (cond ((eq? message 'generate)
             (set! x (rand-update x))
             x)
            ((eq? message 'reset)
             (lambda (new-value) (set! x new-value)))))
    dispatch))

(rand 'generate)
;; => 60

(rand 'generate)
;; => 73

((rand 'reset) random-init)

(rand 'generate)
;; => 60

;; Exercise 3.7

(define (make-joint account old-password new-password)
  (let ((response ((account old-password 'withdraw) 0)))
    (if (number? response)
        (lambda (entered-password m)
          (if (eq? entered-password new-password)
              (account old-password m)
              (account 'intentionally-wrong-password m)))
        response)))

(define peter-acc (make-secure-account 100 'open-sesame))

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

((peter-acc 'open-sesame 'deposit) 25)
;; => 125
((paul-acc 'rosebud 'withdraw) 15)
;; => 110

((paul-acc 'wrong-pass 'withdraw) 110)
;; => Incorrect password
((paul-acc 'wrong-pass 'withdraw) 110)
;; => Incorrect password
((paul-acc 'wrong-pass 'withdraw) 110)
;; => Incorrect password
((paul-acc 'wrong-pass 'withdraw) 110)
;; => Cops are on their way, you little rascal!

;; Exercise 3.8

(define (proc x)
  (lambda (arg)
      (let ((y x))
        (set! x arg)
        y)))

(define f
  (proc 0))

(define g
  (proc 0))

(+ (f 0) (f 1))
;; => 0

(+ (g 1) (g 0))
;; => 1

#lang racket

;; Exercise 3.9

;;     global
;;      env
;;       |
;;       v
;; ----------------
;; |              |
;; | factorial: --|-->@@--> parameters: n
;; |              |   |     body: (if (= n 1)
;; |              |<--'               1
;; |              |                   (* n (factorial (- n 1))))
;; |              |
;; |              |   -------
;; |              |<--| n:6 | E1 (factorial 6)
;; |              |   -------
;; |              |   -------
;; |              |<--| n:5 | E2 (factorial 5)
;; |              |   -------
;; |              |   -------
;; |              |<--| n:4 | E3 (factorial 4)
;; |              |   -------
;; |              |   -------
;; |              |<--| n:3 | E4 (factorial 3)
;; |              |   -------
;; |              |   -------
;; |              |<--| n:2 | E5 (factorial 2)
;; |              |   -------
;; |              |   -------
;; |              |<--| n:1 | E6 (factorial 1)
;; ----------------   -------

;;     global
;;      env
;;       |
;;       v
;; ----------------
;; |              |
;; | factorial: --|-->@@--> parameters: n
;; |              |   |     body: (fact-iter 1 1 n)
;; |              |<--'
;; |              |
;; |              |
;; | fact-iter: --|-->@@--> parameters: product, counter, max-count
;; |              |   |     body: (if (> counter max-count)
;; |              |<--'               product
;; |              |                   (fact-iter (* counter product)
;; |              |                              (+ counter 1)
;; |              |                              max-count))
;; |              |
;; |              |   -------
;; |              |<--| n:6 | E1 (factorial 6)
;; |              |   -------
;; |              |   ----------------
;; |              |   | product:   1 |
;; |              |<--| counter:   1 | E2 (fact-iter 1 1 6)
;; |              |   | max-count: 6 |
;; |              |   ----------------
;; |              |   ----------------
;; |              |   | product:   1 |
;; |              |<--| counter:   2 | E3 (fact-iter 1 2 6)
;; |              |   | max-count: 6 |
;; |              |   ----------------
;; |              |   ----------------
;; |              |   | product:   2 |
;; |              |<--| counter:   3 | E4 (fact-iter 2 3 6)
;; |              |   | max-count: 6 |
;; |              |   ----------------
;; |              |   ----------------
;; |              |   | product:   6 |
;; |              |<--| counter:   4 | E5 (fact-iter 6 4 6)
;; |              |   | max-count: 6 |
;; |              |   ----------------
;; |              |   ----------------
;; |              |   | product:  24 |
;; |              |<--| counter:   5 | E6 (fact-iter 24 5 6)
;; |              |   | max-count: 6 |
;; |              |   ----------------
;; |              |   ----------------
;; |              |   | product: 120 |
;; |              |<--| counter:   6 | E7 (fact-iter 120 6 6)
;; |              |   | max-count: 6 |
;; |              |   ----------------
;; |              |   ----------------
;; |              |   | product: 720 |
;; |              |<--| counter:   7 | E8 (fact-iter 720 7 6)
;; |              |   | max-count: 6 |
;; ----------------   ----------------

;; Exercise 3.10

;; The environment after evaluating (define W1 (make-withdraw 100))

;;     global
;;      env
;;       |
;;       v
;; --------------------
;; |                  |
;; |  make-withdraw: -|-->@@--> parameters: initial-amount
;; |                  |   |     body: (let ((balance initial-amount))
;; |                  |<--'             (lambda (amount)
;; |                  |                   (if (>= balance amount)
;; |                  |                       (begin (set! balance (- balance amount))
;; |                  |                              balance)
;; |                  |                       "Insufficient funds")))
;; |                  |
;; |                  |
;; |      W1: --------|-->@@----------------------------------------------> parameters: amount
;; |                  |                                                |    body: (if (>= balance amount)
;; |                  |                                                |              (begin (set! balance (- balance amount))
;; |                  |   -----------------------   ----------------   |                     balance)
;; |                  |<--| initial-amount: 100 |<--| balance: 100 |<--'              "Insufficient funds")
;; |                  |   -----------------------   ----------------
;; |                  |             E1                     E2
;; |                  |
;; --------------------

;; The environment after evaluating (W1 50)

;;     global
;;      env
;;       |
;;       v
;; --------------------
;; |                  |
;; |  make-withdraw: -|-->@@--> parameters: initial-amount
;; |                  |   |     body: (let ((balance initial-amount))
;; |                  |<--'             (lambda (amount)
;; |                  |                   (if (>= balance amount)
;; |                  |                       (begin (set! balance (- balance amount))
;; |                  |                              balance)
;; |                  |                       "Insufficient funds")))
;; |                  |
;; |                  |
;; |      W1: --------|-->@@---------------------------------------------> parameters: amount
;; |                  |                                               |    body: (if (>= balance amount)
;; |                  |                                               |              (begin (set! balance (- balance amount))
;; |                  |   -----------------------   ---------------   |                     balance)
;; |                  |<--| initial-amount: 100 |<--| balance: 50 |<--'              "Insufficient funds")
;; |                  |   -----------------------   ---------------
;; |                  |             E1                     E2
;; |                  |
;; --------------------

;; The environment after evaluating (define W2 (make-withdraw 100))

;;     global
;;      env
;;       |
;;       v
;; --------------------
;; |                  |
;; |  make-withdraw: -|-->@@--> parameters: initial-amount
;; |                  |   |     body: (let ((balance initial-amount))
;; |                  |<--'             (lambda (amount)
;; |                  |                   (if (>= balance amount)
;; |                  |                       (begin (set! balance (- balance amount))
;; |                  |                              balance)
;; |                  |                       "Insufficient funds")))
;; |                  |
;; |                  |
;; |      W1: --------|---------------------------------------------->@@--> parameters: amount
;; |                  |                                               | ^   body: (if (>= balance amount)
;; |                  |                                               | |        (begin (set! balance (- balance amount))
;; |                  |   -----------------------   ---------------   | |               balance)
;; |                  |<--| initial-amount: 100 |<--| balance: 50 |<--' |        "Insufficient funds")
;; |                  |   -----------------------   ---------------     |
;; |                  |             E1                     E2           |
;; |                  |                                                 |
;; |      W2: --------|----------------------------------------------->@@
;; |                  |                                                |
;; |                  |                                                |
;; |                  |   -----------------------   ----------------   |
;; |                  |<--| initial-amount: 100 |<--| balance: 100 |<--'
;; |                  |   -----------------------   ----------------
;; |                  |             E3                     E4
;; --------------------

;; Exercise 3.11

;; The environment after evaluating (define acc (make-account 50))

;;     global
;;      env
;;       |
;;       v
;; -------------------
;; |                 |
;; |  make-account: -|-->@@--> parameters: balance
;; |                 |   |     body: (define withdraw ... )
;; |                 |<--'           (define deposit ... )
;; |                 |               (define dispatch  ... )
;; |                 |               dispatch
;; |                 |
;; |                 |
;; |                 |         E1
;; |                 |   ---------------
;; |                 |<--| balance: 50 |
;; |                 |   |             |
;; |                 |   |  withdraw: -|-->@@--> parameters: amount         
;; |                 |   |             |   |     body: (if ... )                                   
;; |                 |   |             |<--'
;; |                 |   |             |
;; |                 |   |   deposit: -|-->@@--> parameters: amount
;; |                 |   |             |   |     body: (set! ... )
;; |                 |   |             |<--'
;; |                 |   |             |
;; |                 |   |  dispatch: -|-->@@--> parameters: m
;; |                 |   |             |   | ^   body: (cond ... )   
;; |                 |   |             |<--' |
;; |                 |   ---------------     |
;; |                 |                       |
;; |      acc: ------|-----------------------'
;; |                 |
;; |                 |
;; -------------------

;; The environment after evaluating ((acc 'deposit) 40)

;;     global
;;      env
;;       |
;;       v
;; -------------------
;; |                 |
;; |  make-account: -|-->@@--> parameters: balance
;; |                 |   |     body: (define withdraw ... )
;; |                 |<--'           (define deposit ... )
;; |                 |               (define dispatch  ... )
;; |                 |               dispatch
;; |                 |
;; |                 |
;; |                 |         E1                                           E2
;; |                 |   ---------------                             ---------------
;; |                 |<--| balance: 50 |<----------------------------| m: 'deposit | call to dispatch
;; |                 |   |             |                             ---------------
;; |                 |   |             |                                    E3
;; |                 |   |             |                             --------------
;; |                 |   |             |<----------------------------| amount: 40 | call to deposit
;; |                 |   |             |                             --------------
;; |                 |   |  withdraw: -|-->@@--> parameters: amount
;; |                 |   |             |   |     body: (if ... )
;; |                 |   |             |<--'
;; |                 |   |             |
;; |                 |   |   deposit: -|-->@@--> parameters: amount
;; |                 |   |             |   |     body: (set! ... )
;; |                 |   |             |<--'
;; |                 |   |             |
;; |                 |   |  dispatch: -|-->@@--> parameters: m
;; |                 |   |             |   | ^   body: (cond ... )
;; |                 |   |             |<--' |
;; |                 |   ---------------     |
;; |                 |                       |
;; |      acc: ------|-----------------------'
;; |                 |
;; |                 |
;; -------------------

;; The environment after evaluating ((acc 'withdraw) 60)

;;     global
;;      env
;;       |
;;       v
;; -------------------
;; |                 |
;; |  make-account: -|-->@@--> parameters: balance
;; |                 |   |     body: (define withdraw ... )
;; |                 |<--'           (define deposit ... )
;; |                 |               (define dispatch  ... )
;; |                 |               dispatch
;; |                 |
;; |                 |
;; |                 |         E1                                           E4
;; |                 |   ---------------                             ----------------
;; |                 |<--| balance: 90 |<----------------------------| m: 'withdraw | call to dispatch
;; |                 |   |             |                             ----------------
;; |                 |   |             |                                    E5
;; |                 |   |             |                             --------------
;; |                 |   |             |<----------------------------| amount: 60 | call to withdraw
;; |                 |   |             |                             --------------
;; |                 |   |  withdraw: -|-->@@--> parameters: amount
;; |                 |   |             |   |     body: (if ... )
;; |                 |   |             |<--'
;; |                 |   |             |
;; |                 |   |   deposit: -|-->@@--> parameters: amount
;; |                 |   |             |   |     body: (set! ... )
;; |                 |   |             |<--'
;; |                 |   |             |
;; |                 |   |  dispatch: -|-->@@--> parameters: m
;; |                 |   |             |   | ^   body: (cond ... )
;; |                 |   |             |<--' |
;; |                 |   ---------------     |
;; |                 |                       |
;; |      acc: ------|-----------------------'
;; |                 |
;; |                 |
;; -------------------

;; The environment after evaluating (define acc2 (make-account 100))

;;     global
;;      env
;;       |
;;       v
;; -------------------
;; |                 |
;; |  make-account: -|-->@@--> parameters: balance
;; |                 |   |     body: (define withdraw ... )
;; |                 |<--'           (define deposit ... )
;; |                 |               (define dispatch  ... )
;; |                 |               dispatch
;; |                 |
;; |                 |
;; |                 |         E1
;; |                 |   ---------------
;; |                 |<--| balance: 30 |
;; |                 |   |             |
;; |                 |   |             |
;; |                 |   |             |
;; |                 |   |             |
;; |                 |   |             |
;; |                 |   |  withdraw: -|-->@@--> parameters: amount
;; |                 |   |             |   |     body: (if ... )
;; |                 |   |             |<--'
;; |                 |   |             |
;; |                 |   |   deposit: -|-->@@--> parameters: amount
;; |                 |   |             |   |     body: (set! ... )
;; |                 |   |             |<--'
;; |                 |   |             |
;; |                 |   |  dispatch: -|-->@@--> parameters: m
;; |                 |   |             |   | ^   body: (cond ... )
;; |                 |   |             |<--' |
;; |                 |   ---------------     |
;; |                 |                       |
;; |      acc: ------|-----------------------'
;; |                 |
;; |                 |
;; |                 |         E6
;; |                 |   ----------------
;; |                 |<--| balance: 100 |
;; |                 |   |              |
;; |                 |   |              |
;; |                 |   |              |
;; |                 |   |              |
;; |                 |   |              |
;; |                 |   |  withdraw: --|-->@@--> parameters: amount
;; |                 |   |              |   |     body: (if ... )
;; |                 |   |              |<--'
;; |                 |   |              |
;; |                 |   |   deposit: --|-->@@--> parameters: amount
;; |                 |   |              |   |     body: (set! ... )
;; |                 |   |              |<--'
;; |                 |   |              |
;; |                 |   |  dispatch: --|-->@@--> parameters: m
;; |                 |   |              |   | ^   body: (cond ... )
;; |                 |   |              |<--' |
;; |                 |   ----------------     |
;; |                 |                        |
;; |      acc2: -----|------------------------'
;; |                 |
;; |                 |
;; -------------------

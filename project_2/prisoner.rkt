#lang racket

;; 
;;  The play-loop procedure takes as its  arguments two prisoner's
;;  dilemma strategies, and plays an iterated game of approximately
;;  one hundred rounds.  A strategy is a procedure that takes
;;  two arguments: a history of the player's previous plays and 
;;  a history of the other player's previous plays.  The procedure
;;  returns either a "c" for cooperate or a "d" for defect.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (play-loop strat0 strat1)
  (define (play-loop-iter strat0 strat1 count history0 history1 limit)
    (cond ((= count limit) (print-out-results history0 history1 limit))
	  (else (let ((result0 (strat0 history0 history1))
		      (result1 (strat1 history1 history0)))
		  (play-loop-iter strat0 strat1 (+ count 1)
				  (extend-history result0 history0)
				  (extend-history result1 history1)
				  limit)))))
  (play-loop-iter strat0 strat1 0 the-empty-history the-empty-history
		  (+ 90 (random 21))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The following procedures are used to compute and print
;;  out the players' scores at the end of an iterated game
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-out-results history0 history1 number-of-games)
  (let ((scores (get-scores history0 history1)))
    (newline)
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)))

(define (get-scores history0 history1)
  (define (get-scores-helper history0 history1 score0 score1)
    (cond ((empty-history? history0)
	   (list score0 score1))
	  (else (let ((game (make-play (most-recent-play history0)
				       (most-recent-play history1))))
		  (get-scores-helper (rest-of-plays history0)
				     (rest-of-plays history1)
				     (+ (get-player-points 0 game) score0)
				     (+ (get-player-points 1 game) score1))))))
  (get-scores-helper history0 history1 0 0))

(define (get-player-points num game)
  (list-ref (get-point-list game) num))

(define *game-association-list*
  ;; format is that first sublist identifies the players' choices 
  ;; with "c" for cooperate and "d" for defect; and that second sublist 
  ;; specifies payout for each player
  '((("c" "c") (3 3))
    (("c" "d") (0 5))
    (("d" "c") (5 0))
    (("d" "d") (1 1))))

(define (get-point-list game)
  (cadr (extract-entry game *game-association-list*)))

(define (extract-entry game game-list)
  (let ((entry (car game-list)))
    (if (equal? game (car entry))
        entry
        (extract-entry game (cdr game-list)))))

(define make-play list)

(define the-empty-history '())

(define extend-history cons)
(define empty-history? null?)

(define most-recent-play car)
(define rest-of-plays cdr)

;; A sampler of strategies

(define (NASTY my-history other-history)
  "d")

(define (PATSY my-history other-history)
  "c")

(define (SPASTIC my-history other-history)
  (if (= (random 2) 0)
      "c"
      "d"))

(define (EGALITARIAN  my-history other-history)
  (define (count-instances-of test hist)
    (cond ((empty-history? hist) 0)
	  ((string=? (most-recent-play hist) test)
	   (+ (count-instances-of test (rest-of-plays hist)) 1))
	  (else (count-instances-of test (rest-of-plays hist)))))
  (let ((ds (count-instances-of "d" other-history))
	(cs (count-instances-of "c" other-history)))
    (if (> ds cs) "d" "c")))

(define (EYE-FOR-EYE my-history other-history)
  (if (empty-history? my-history)
      "c"
      (most-recent-play other-history)))

(newline)
(displayln "NASTY")
(play-loop NASTY NASTY)
(play-loop NASTY PATSY)
(play-loop NASTY SPASTIC)
(play-loop NASTY EGALITARIAN)
(play-loop NASTY EYE-FOR-EYE)

;;           ------------------------------------------------------------------------
;;           |    NASTY   |    PATSY   |   SPASTIC  |  EGALITARIAN  |  EYE-FOR-EYE  |
;;-----------------------------------------------------------------------------------
;;   NASTY   |     tie    |     win    |     win    |      win      |      win      | 
;;           | 1.0 points | 5.0 points | 2.9 points |  1.04 points  |  1.04 points  | 
;;-----------------------------------------------------------------------------------

(newline)
(displayln "PATSY")
(play-loop PATSY NASTY)
(play-loop PATSY PATSY)
(play-loop PATSY SPASTIC)
(play-loop PATSY EGALITARIAN)
(play-loop PATSY EYE-FOR-EYE)

;;           -------------------------------------------------------------------------
;;           |    NASTY   |    PATSY   |   SPASTIC   |  EGALITARIAN  |  EYE-FOR-EYE  |
;;------------------------------------------------------------------------------------
;;   PATSY   |   defeat   |     tie    |   defeat    |      tie      |      tie      | 
;;           | 0.0 points | 3.0 points | 1.58 points |  3.0 points   |  3.0 points   | 
;;------------------------------------------------------------------------------------

(newline)
(displayln "SPASTIC")
(play-loop SPASTIC NASTY)
(play-loop SPASTIC PATSY)
(play-loop SPASTIC SPASTIC)
(play-loop SPASTIC EGALITARIAN)
(play-loop SPASTIC EYE-FOR-EYE)

;;           ----------------------------------------------------------------------------
;;           |    NASTY    |    PATSY    |   SPASTIC   |  EGALITARIAN  |  EYE-FOR-EYE   |
;;---------------------------------------------------------------------------------------
;;  SPASTIC  |    defeat   |     win     |   defeat    |    defeat     |      win       | 
;;           | 0.49 points | 3.95 points | 2.03 points |  1.3 points   |  2.19 points   | 
;;---------------------------------------------------------------------------------------

(newline)
(displayln "EGALITARIAN")
(play-loop EGALITARIAN NASTY)
(play-loop EGALITARIAN PATSY)
(play-loop EGALITARIAN SPASTIC)
(play-loop EGALITARIAN EGALITARIAN)
(play-loop EGALITARIAN EYE-FOR-EYE)

;;               ----------------------------------------------------------------------------
;;               |    NASTY    |    PATSY    |   SPASTIC   |  EGALITARIAN  |  EYE-FOR-EYE   |
;;-------------------------------------------------------------------------------------------
;;  EGALITARIAN  |    defeat   |     tie     |   defeat    |      tie      |      tie       | 
;;               | 0.99 points | 3.0 points  | 1.55 points |  3.0 points   |   3.0 points   | 
;;-------------------------------------------------------------------------------------------

(newline)
(displayln "EYE-FOR-EYE")
(play-loop EYE-FOR-EYE NASTY)
(play-loop EYE-FOR-EYE PATSY)
(play-loop EYE-FOR-EYE SPASTIC)
(play-loop EYE-FOR-EYE EGALITARIAN)
(play-loop EYE-FOR-EYE EYE-FOR-EYE)

;;               ----------------------------------------------------------------------------
;;               |    NASTY    |    PATSY    |   SPASTIC   |  EGALITARIAN  |  EYE-FOR-EYE   |
;;-------------------------------------------------------------------------------------------
;;  EYE-FOR-EYE  |    defeat   |     tie     |     tie     |      tie      |      tie       | 
;;               | 0.98 points | 3.0 points  | 2.24 points |  3.0 points   |   3.0 points   | 
;;-------------------------------------------------------------------------------------------

;; The first implementation of the Egalitarian strategy has an order of growth of O(2n),
;; but Alyssa's implementation has an order of growth of O(n) which are basically the same orders
;; of growth even though Alyssa's implementation will be executed roughly twice as fast as the first
;; implementation.

(define (Egalitarian my-history other-history)
  (define (majority-loop cs ds hist)
    (cond ((empty-history? hist) (if (> ds cs) "d" "c"))
          ((string=? (most-recent-play hist) "c")
           (majority-loop (+ 1 cs) ds (rest-of-plays hist)))
          (else
           (majority-loop cs (+ 1 ds) (rest-of-plays hist)))))
  (majority-loop 0 0 other-history))

(define (EYE-FOR-TWO-EYES my-history other-history)
  (if (and (> (length other-history) 1)
           (string=? "d" (most-recent-play other-history) (most-recent-play (rest-of-plays other-history))))
      "d"  
      "c"))

(EYE-FOR-TWO-EYES '() '())
;; => c

(EYE-FOR-TWO-EYES '() (list "c"))
;; => "c"
(EYE-FOR-TWO-EYES '() (list "d"))
;; => "c"

(EYE-FOR-TWO-EYES '() (list "c" "c"))
;; => "c"
(EYE-FOR-TWO-EYES '() (list "c" "d"))
;; => "c"
(EYE-FOR-TWO-EYES '() (list "d" "c"))
;; => "c"
(EYE-FOR-TWO-EYES '() (list "d" "d"))
;; => "d"

(EYE-FOR-TWO-EYES '() (list "c" "c" "c"))
;; => "c"
(EYE-FOR-TWO-EYES '() (list "c" "d" "c"))
;; => "c"
(EYE-FOR-TWO-EYES '() (list "d" "c" "c"))
;; => "c"
(EYE-FOR-TWO-EYES '() (list "d" "d" "c"))
;; => "d" 

(newline)
(displayln "EYE-FOR-TWO-EYES")
(play-loop EYE-FOR-TWO-EYES NASTY)
(play-loop EYE-FOR-TWO-EYES PATSY)
(play-loop EYE-FOR-TWO-EYES SPASTIC)
(play-loop EYE-FOR-TWO-EYES EGALITARIAN)
(play-loop EYE-FOR-TWO-EYES EYE-FOR-EYE)
(play-loop EYE-FOR-TWO-EYES EYE-FOR-TWO-EYES)

;;                    ----------------------------------------------------------------------------------------------
;;                    |    NASTY    |    PATSY    |   SPASTIC   |  EGALITARIAN  |  EYE-FOR-EYE   | EYE-FOR-TWO-EYES
;;------------------------------------------------------------------------------------------------------------------
;;  EYE-FOR-EYE       |    defeat   |     tie     |     tie     |      tie      |      tie       |       tie       | 
;;                    | 0.98 points | 3.0 points  | 2.24 points |  3.0 points   |   3.0 points   |    3.0 points   | 
;;------------------------------------------------------------------------------------------------------------------
;;  EYE-FOR-TWO-EYES  |    defeat   |     tie     |   defeat    |      tie      |      tie       |       tie       |
;;                    | 0.97 points | 3.0 points  | 1.9 points  |  3.0 points   |   3.0 points   |    3.0 points   | 
;;------------------------------------------------------------------------------------------------------------------

(define (EYE-FOR-N-EYES n)
  (define (check-last-n-plays n other-history)
    (cond ((= n 0) "d")
          ((string=? "c" (most-recent-play other-history)) "c")
          (else (check-last-n-plays (- n 1) (rest-of-plays other-history)))))
        
  (lambda (my-history other-history)
    (if (< (length other-history) n)
         "c"
        (check-last-n-plays n other-history))))


(define EYE-FOR-THREE-EYES (EYE-FOR-N-EYES 3))

(EYE-FOR-THREE-EYES '() (list "c" "c" "c" "c"))
;; => "c"
(EYE-FOR-THREE-EYES '() (list "c" "d" "c" "d"))
;; => "c"
(EYE-FOR-THREE-EYES '() (list "d" "c" "d" "c"))
;; => "c"
(EYE-FOR-THREE-EYES '() (list "d" "d" "d" "c"))
;; => "d"
(EYE-FOR-THREE-EYES '() (list "d" "d" "d" "d"))
;; => "d"

(newline)
(displayln "EYE-FOR-THREE-EYES")
(play-loop EYE-FOR-THREE-EYES NASTY)
(play-loop EYE-FOR-THREE-EYES PATSY)
(play-loop EYE-FOR-THREE-EYES SPASTIC)
(play-loop EYE-FOR-THREE-EYES EGALITARIAN)
(play-loop EYE-FOR-THREE-EYES EYE-FOR-EYE)
(play-loop EYE-FOR-THREE-EYES EYE-FOR-TWO-EYES)
(play-loop EYE-FOR-THREE-EYES EYE-FOR-THREE-EYES)

;;                    ------------------------------------------------------------------------------------------------------------------------
;;                      |    NASTY    |    PATSY    |   SPASTIC   |  EGALITARIAN  |  EYE-FOR-EYE   | EYE-FOR-TWO-EYES |  EYE-FOR-THREE-EYES  |
;;--------------------------------------------------------------------------------------------------------------------------------------------
;;  EYE-FOR-EYE         |    defeat   |     tie     |     tie     |      tie      |      tie       |       tie        |         tie          | 
;;                      | 0.98 points | 3.0 points  | 2.24 points |  3.0 points   |   3.0 points   |    3.0 points    |      3.0 points      | 
;;--------------------------------------------------------------------------------------------------------------------------------------------
;;  EYE-FOR-TWO-EYES    |    defeat   |     tie     |   defeat    |      tie      |      tie       |       tie        |         tie          |
;;                      | 0.97 points | 3.0 points  | 1.9 points  |  3.0 points   |   3.0 points   |    3.0 points    |      3.0 points      | 
;;--------------------------------------------------------------------------------------------------------------------------------------------
;;  EYE-FOR-THREE-EYES  |    defeat   |     tie     |   defeat    |      tie      |      tie       |       tie        |         tie          |
;;                      | 0.96 points | 3.0 points  | 1.6 points  |  3.0 points   |   3.0 points   |    3.0 points    |      3.0 points      | 
;;--------------------------------------------------------------------------------------------------------------------------------------------

(define (make-rotating-strategy strat0 strat1 freq0 freq1)
  (define (pick-strategy strat0 strat1 freq0 freq1 count)
    (let ((rem (remainder count (+ freq0 freq1))))
      (if (< rem freq0)
          strat0
          strat1)))

  (lambda (my-history other-history)
    (let ((strategy (pick-strategy strat0 strat1 freq0 freq1 (length other-history))))
      (strategy my-history other-history))))
      
(define ROTATING-STRATEGY (make-rotating-strategy NASTY EYE-FOR-EYE 1 3))

(newline)
(displayln "ROTATING-STRATEGY")
(play-loop ROTATING-STRATEGY NASTY)
(play-loop ROTATING-STRATEGY PATSY)
(play-loop ROTATING-STRATEGY SPASTIC)
(play-loop ROTATING-STRATEGY EGALITARIAN)
(play-loop ROTATING-STRATEGY EYE-FOR-EYE)
(play-loop ROTATING-STRATEGY EYE-FOR-TWO-EYES)
(play-loop ROTATING-STRATEGY EYE-FOR-THREE-EYES)
(play-loop ROTATING-STRATEGY ROTATING-STRATEGY)

;;                      ---------------------------------------------------------------------------------------------------------------------------------
;;                      |    NASTY   |    PATSY   |   SPASTIC   | EGALITARIAN | EYE-FOR-EYE | EYE-FOR-TWO-EYES | EYE-FOR-THREE-EYES | ROTATING-STRATEGY |
;;-------------------------------------------------------------------------------------------------------------------------------------------------------
;;  ROTATING-STRATEGY   |    tie     |     win    |     win     |     win     |     win     |       win        |        win         |       tie         | 
;;                      | 1.0 points | 3.5 points | 2.41 points | 2.5 points  | 2.52 points |    3.5 points    |    3.51 points     |     1.0 points    | 
;;-------------------------------------------------------------------------------------------------------------------------------------------------------

(define (make-higher-order-spastic strategies)
  (define (pick-strategy strategies count)
    (let ((rem (remainder count (length strategies))))
      (list-ref strategies rem)))

  (lambda (my-history other-history)
    (let ((strategy (pick-strategy strategies (length other-history))))
      (strategy my-history other-history))))
      
(define HIGHER-ORDER-SPASTIC (make-higher-order-spastic (list NASTY
                                                              PATSY
                                                              SPASTIC
                                                              EGALITARIAN
                                                              EYE-FOR-EYE
                                                              EYE-FOR-TWO-EYES
                                                              EYE-FOR-THREE-EYES
                                                              ROTATING-STRATEGY)))

(newline)
(displayln "HIGHER-ORDER-SPASTIC")
(play-loop HIGHER-ORDER-SPASTIC NASTY)
(play-loop HIGHER-ORDER-SPASTIC PATSY)
(play-loop HIGHER-ORDER-SPASTIC SPASTIC)
(play-loop HIGHER-ORDER-SPASTIC EGALITARIAN)
(play-loop HIGHER-ORDER-SPASTIC EYE-FOR-EYE)
(play-loop HIGHER-ORDER-SPASTIC EYE-FOR-TWO-EYES)
(play-loop HIGHER-ORDER-SPASTIC EYE-FOR-THREE-EYES)
(play-loop HIGHER-ORDER-SPASTIC ROTATING-STRATEGY)
(play-loop HIGHER-ORDER-SPASTIC HIGHER-ORDER-SPASTIC)

;; the HIGHER-ORDER-SPASTIC strategy gives a different result every time

(define (gentle strat gentleness-factor)
  (lambda (my-history other-history)
    (let ((choice (strat my-history other-history)))
      (if (string=? "d" choice)
          (if (< (random) gentleness-factor)
              "c"
              choice)
          choice))))
      
(define  SLIGHTLY-GENTLE-NASTY (gentle NASTY 0.1))

(newline)
(displayln "SLIGHTLY-GENTLE-NASTY")
(play-loop SLIGHTLY-GENTLE-NASTY NASTY)
(play-loop SLIGHTLY-GENTLE-NASTY PATSY)
(play-loop SLIGHTLY-GENTLE-NASTY SPASTIC)
(play-loop SLIGHTLY-GENTLE-NASTY EGALITARIAN)
(play-loop SLIGHTLY-GENTLE-NASTY EYE-FOR-EYE)
(play-loop SLIGHTLY-GENTLE-NASTY ROTATING-STRATEGY)
(play-loop SLIGHTLY-GENTLE-NASTY HIGHER-ORDER-SPASTIC)
(play-loop SLIGHTLY-GENTLE-NASTY SLIGHTLY-GENTLE-NASTY)

;; the SLIGHTLY-GENTLE-NASTY strategy gives a different result every time

(define  SLIGHTLY-GENTLE-EYE-FOR-EYE (gentle EYE-FOR-EYE 0.1))

(newline)
(displayln "SLIGHTLY-GENTLE-EYE-FOR-EYE")
(play-loop SLIGHTLY-GENTLE-EYE-FOR-EYE NASTY)
(play-loop SLIGHTLY-GENTLE-EYE-FOR-EYE PATSY)
(play-loop SLIGHTLY-GENTLE-EYE-FOR-EYE SPASTIC)
(play-loop SLIGHTLY-GENTLE-EYE-FOR-EYE EGALITARIAN)
(play-loop SLIGHTLY-GENTLE-EYE-FOR-EYE EYE-FOR-EYE)
(play-loop SLIGHTLY-GENTLE-EYE-FOR-EYE ROTATING-STRATEGY)
(play-loop SLIGHTLY-GENTLE-EYE-FOR-EYE HIGHER-ORDER-SPASTIC)
(play-loop SLIGHTLY-GENTLE-EYE-FOR-EYE SLIGHTLY-GENTLE-NASTY)
(play-loop SLIGHTLY-GENTLE-EYE-FOR-EYE SLIGHTLY-GENTLE-EYE-FOR-EYE)

;; the SLIGHTLY-GENTLE-EYE-FOR-EYE strategy gives a different result every time

;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; code to use in 3 player game
;;

(define (tp-play-loop strat0 strat1 strat2)
  (define (play-loop-iter strat0 strat1 strat2 count history0 history1 history2 limit)
    (cond ((= count limit) (tp-print-out-results history0 history1 history2 limit))
	  (else (let ((result0 (strat0 history0 history1 history2))
		      (result1 (strat1 history1 history0 history2))
                      (result2 (strat2 history2 history0 history1)))
		  (play-loop-iter strat0 strat1 strat2 (+ count 1)
				  (extend-history result0 history0)
				  (extend-history result1 history1)
                                  (extend-history result2 history2)
				  limit)))))
  (play-loop-iter strat0 strat1 strat2 0 the-empty-history the-empty-history the-empty-history
		  (+ 90 (random 21))))

(define (tp-print-out-results history0 history1 history2 number-of-games)
  (let ((scores (tp-get-scores history0 history1 history2)))
    (newline)
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)
    (display "Player 3 Score:  ")
    (display (* 1.0 (/ (caddr scores) number-of-games)))
    (newline)))

(define (tp-get-scores history0 history1 history2)
  (define (get-scores-helper history0 history1 history2 score0 score1 score2)
    (cond ((empty-history? history0)
	   (list score0 score1 score2))
	  (else (let ((game (make-play (most-recent-play history0)
				       (most-recent-play history1)
                                       (most-recent-play history2))))
		  (get-scores-helper (rest-of-plays history0)
				     (rest-of-plays history1)
                                     (rest-of-plays history2)
				     (+ (tp-get-player-points 0 game) score0)
				     (+ (tp-get-player-points 1 game) score1)
                                     (+ (tp-get-player-points 2 game) score2))))))
  (get-scores-helper history0 history1 history2 0 0 0))

(define (tp-get-player-points num game)
  (list-ref (tp-get-point-list game) num))

(define (tp-get-point-list game)
  (cadr (extract-entry game *tp-game-association-list*)))

(define *tp-game-association-list*
  (list (list (list "c" "c" "c") (list 4 4 4))
        (list (list "c" "c" "d") (list 2 2 5))
        (list (list "c" "d" "c") (list 2 5 2))
        (list (list "d" "c" "c") (list 5 2 2))
        (list (list "c" "d" "d") (list 0 3 3))
        (list (list "d" "c" "d") (list 3 0 3))
        (list (list "d" "d" "c") (list 3 3 0))
        (list (list "d" "d" "d") (list 1 1 1))))

(define (NASTY-3 my-history history1 history2)
  "d")

(define (PATSY-3 my-history history1 history2)
  "c")

(define (SPASTIC-3 my-history history1 history2)
  (if (= (random 2) 0)
      "c"
      "d"))

(newline)
(displayln "THREE PLAYERS")
(tp-play-loop SPASTIC-3 PATSY-3 NASTY-3)
(tp-play-loop PATSY-3 SPASTIC-3 PATSY-3)
(tp-play-loop NASTY-3 NASTY-3 SPASTIC-3)

(define (TOUGH-EYE-FOR-EYE my-history history1 history2)
  (cond ((empty-history? my-history) "c")
        ((or (string=? "d" (most-recent-play history1))
             (string=? "d" (most-recent-play history2))) "d")
        (else "c")))

(define (SOFT-EYE-FOR-EYE my-history history1 history2)
  (cond ((empty-history? my-history) "c")
        ((and (string=? "d" (most-recent-play history1))
              (string=? "d" (most-recent-play history2))) "d")
        (else "c")))

(newline)
(displayln "TOUGH-EYE-FOR-EYE")
(tp-play-loop TOUGH-EYE-FOR-EYE PATSY-3 NASTY-3)
(tp-play-loop TOUGH-EYE-FOR-EYE SPASTIC-3 PATSY-3)
(tp-play-loop TOUGH-EYE-FOR-EYE NASTY-3 SPASTIC-3)

(newline)
(displayln "SOFT-EYE-FOR-EYE")
(tp-play-loop TOUGH-EYE-FOR-EYE PATSY-3 NASTY-3)
(tp-play-loop TOUGH-EYE-FOR-EYE SPASTIC-3 PATSY-3)
(tp-play-loop TOUGH-EYE-FOR-EYE NASTY-3 SPASTIC-3)

;; in expected-values: #f = don't care 
;;                      X = actual-value needs to be #f or X 
;(define (test-entry expected-values actual-values) 
;   (cond ((null? expected-values) (null? actual-values)) 
;         ((null? actual-values) #f) 
;         ((or (not (car expected-values)) 
;              (not (car actual-values)) 
;              (= (car expected-values) (car actual-values))) 
;          (test-entry (cdr expected-values) (cdr actual-values))) 
;         (else #f))) 
;
;(define (is-he-a-fool? hist0 hist1 hist2) 
;   (test-entry (list 1 1 1) 
;               (get-probability-of-c 
;                (make-history-summary hist0 hist1 hist2))))
;
;(define (could-he-be-a-fool? hist0 hist1 hist2)
;  (test-entry (list 1 1 1)
;              (map (lambda (elt) 
;                      (cond ((null? elt) 1)
;                            ((= elt 1) 1)  
;                            (else 0)))
;                   (get-probability-of-c (make-history-summary hist0 
;                                                               hist1
;                                                               hist2)))))

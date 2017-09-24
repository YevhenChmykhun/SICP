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

(define (deriv-1 exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv-1 (addend exp) var)
                   (deriv-1 (augend-1 exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv-1 (multiplicand-1 exp) var))
           (make-product (deriv-1 (multiplier exp) var)
                         (multiplicand-1 exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation
                                      (base exp)
                                      (make-sum (exponent exp) -1)))
                       (deriv-1 (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) 
          (accumulate op initial (cdr sequence))))) 

(define (augend-1 s)
  (accumulate make-sum 0 (cddr s)))

(define (multiplicand-1 p)
  (accumulate make-product 1 (cddr p)))

(deriv-1 '(* x y (+ x 3)) 'x)
;; => (+ (* x y) (* y (+ x 3)))

;; Exercise 2.58 a

(define (deriv-2 exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum?-2 exp)
         (make-sum-2 (deriv-2 (addend-2 exp) var)
                    (deriv-2 (augend exp) var)))
        ((product?-2 exp)
         (make-sum-2
           (make-product-2 (multiplier-2 exp)
                          (deriv-2 (multiplicand exp) var))
           (make-product-2 (deriv-2 (multiplier-2 exp) var)
                          (multiplicand exp))))
        ((exponentiation?-2 exp)
         (make-product-2 (make-product-2 (exponent exp)
                                       (make-exponentiation-2
                                        (base-2 exp)
                                        (make-sum-2 (exponent exp) -1)))
                       (deriv-2 (base-2 exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (make-sum-2 a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product-2 m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (make-exponentiation-2 b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (expt b e))
        (else (list b '** e))))

(define (sum?-2 x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend-2 s) (car s))

(define (product?-2 x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier-2 p) (car p))

(define (exponentiation?-2 x)
  (and (pair? x) (eq? (cadr x) '**)))

(define (base-2 e) (car e))

(deriv-2 '(x + (3 * (x + (y + 2)))) 'x)
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

(define (adjoin-set-2 x set)
  (cons x set))

(define (union-set-2 set1 set2)
  (append set1 set2))

(define (intersection-set-2 set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set-2 (cdr set1) set2)))
        (else (intersection-set-2 (cdr set1) set2))))

(intersection-set-2 '(2 3 2 1 3 2 2) '(9 4 3 6 3 2))

;; Exercise 2.61

(define (adjoin-set-3 x set)
  (if (or (null? set) (> (car set) x))
      (cons x set)
      (cons (car set) (adjoin-set-3 x (cdr set)))))

(adjoin-set-3 3 '(1 2 4 5))
;; => (1 2 3 4 5)

(adjoin-set-3 3 '(1 2))
;; => (1 2 3)

(adjoin-set-3 3 '(4 5))
;; => (3 4 5)

(adjoin-set-3 3 '())
;; => (3)

;; Exercise 2.62

(define (union-set-3 set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((a (car set1))
               (b (car set2)))
           (if (< a b)
               (cons a (union-set-3 (cdr set1) set2))
               (cons b (union-set-3 set1 (cdr set2))))))))

(union-set-3 '() '(3 4 5 6))
;; => (3 4 5 6)

(union-set-3 '(1 2 3 4) '())
;; => (1 2 3 4)

(union-set-3 '(1 2 3 4) '(3 4 5 6))
;; => ( 1 2 3 3 4 4 5 6)

;; Exercise 2.63 a

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define tree1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define tree2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define tree3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

(tree->list-1 tree1)
;; => (1 3 5 7 9 11)

(tree->list-2 tree1)
;; => (1 3 5 7 9 11)

(tree->list-1 tree2)
;; => (1 3 5 7 9 11)

(tree->list-2 tree2)
;; => (1 3 5 7 9 11)

(tree->list-1 tree3)
;; => (1 3 5 7 9 11)

(tree->list-2 tree3)
;; => (1 3 5 7 9 11)

;; Exercise 2.63 b

;; Both functions are basically the same. The only difference is
;; what function is used to put two parts of a tree together.

;; In the first case, it is append function which has a time complexity of O(n).
;; But in each recursive call we need to traverse only first list
;; (left branch) in order to combine two lists together.
;; So overall the time complexity becomes O(nlogn)

;; In the second case cons function is used with a time complexity of O(1)
;; In the result, we have O(n) here.

;; Exercise 2.64 a

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;; 1. Calculate the length of the left tree. Simply substruct 1 which represent
;;    current node from the length of the list and devide the result by 2.
;; 2. Use partial-tree function to build the left tree.
;; 3. Take the first element from the remaining list and use it as a node value.
;; 4. Calculate the length of the right tree. Substract the length of the left
;;    tree plus 1 (the current node) from the length of the element list.
;; 5. Use partial-tree function to build the right tree.
;; 6. Make tree by combining the left tree, the current node and the right tree.
;;    (and add the remaining elements)

(list->tree '(1 3 5 7 9 11))
;;    5
;;  /   \
;; 1     9
;;  \   / \
;;   3 7  11

;; Exercise 2.64 b

;; the order of growth is O(n)

;; Exercise 2.65

(define (union-set-4 tree1 tree2) 
   (list->tree (union-set-3 (tree->list-1 tree1) 
                          (tree->list-1 tree2))))

;; transform the tree to a list for a better view
(tree->list-1 (union-set-4 tree1 tree1))
;; => (1 1 3 3 5 5 7 7 9 9 11 11) 
  
(define (intersection-set-4 tree1 tree2) 
   (list->tree (intersection-set-2 (tree->list-1 tree1) 
                                 (tree->list-1 tree2)))) 

;; transform the tree to a list for a better view
(tree->list-1 (intersection-set-4 tree1 '(9 (7 () ()) ())))
;; => (7 9)

;; Exercise 2.66

(define (key employee) (car employee))

(define (value employee) (cdr employee))

(define (lookup given-key tree-of-records)
  (cond ((null? tree-of-records) false)
        ((equal? given-key (key (entry tree-of-records)))
         (value (entry tree-of-records)))
        ((< given-key (key (entry tree-of-records)))
         (lookup given-key (left-branch tree-of-records)))
        ((> given-key (key (entry tree-of-records)))
         (lookup given-key (right-branch tree-of-records)))))

(define employee-list '((1 Jhon) (2 Bruce) (3 Craig) (4 Morgan) (5 Roger) (6 Scott) (7 Daniel) ))

(list->tree employee-list)

(lookup 5 (list->tree employee-list))
;; => (Roger)

;; Exercise 2.67

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch-huffman tree) (car tree))

(define (right-branch-huffman tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch-huffman branch))
        ((= bit 1) (right-branch-huffman branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)
;; => (A D A B B C A)

;; Exercise 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((element-of-set? symbol (symbols (left-branch-huffman tree)))
         (cons 0 (encode-symbol symbol (left-branch-huffman tree))))
        ((element-of-set? symbol (symbols (right-branch-huffman tree)))
         (cons 1 (encode-symbol symbol (right-branch-huffman tree))))
        (else (error "bad symbol -- ENCODE-SYMBOL" symbol))))

(encode '(A D A B B C A) sample-tree)
;; => (0 1 1 0 0 1 0 1 0 1 1 1 0)

;; Exercise 2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (let ((new-branch (make-code-tree (car leaf-set) (cadr leaf-set))))
        (successive-merge (adjoin-set new-branch (cddr leaf-set))))))

(define generated-tree (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))

(decode sample-message generated-tree)
;; => (A D A B B C A)

(encode '(A D A B B C A) generated-tree)
;; => (0 1 1 0 0 1 0 1 0 1 1 1 0)

;; Exercise 2.70

(define song-alphabet '((a 2) (boom 1) (get 2) (job 2) (na 16) (sha 3) (yip 9) (wah 1)))

(define song-tree (generate-huffman-tree song-alphabet))

(define song-message '(get a job sha na na na na na na na na get a job sha na na na na na na na na wah yip yip yip yip yip yip yip yip yip sha boom))

(define encoded-song (encode song-message song-tree))

encoded-song

;; variable-length codes
(length encoded-song)
;; => 84

;; fixed-length codes
(* 3 (length song-message))
;; => 108

;; Exercise 2.71

;; the tree for n=5
;;
;;   /\
;;  /  \ 15 (8 + 7)
;; 16  /\  
;;    /  \ 7 (4 + 3)
;;   8   /\
;;      /  \ 3 (2 + 1)
;;     4   /\
;;        /  \
;;       2    1

;; the tree for n=10 look the same

;; to encode the most frequent symbol we need 1 bit (depth of the symbol)
;; to encode the least frequent symbol we need n-1 bit (once again depth of the symbol)

;; Exercise 2.71

;; the order of growth in case of encoding the most frequent symbol is O(n)
;; the order of growth in case of encoding the least frequent symbol is O(n^2)
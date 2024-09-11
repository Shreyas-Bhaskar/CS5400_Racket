#lang racket
(require rackunit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1: all-greater

; all-greater? : int list -> int -> bool
(define (all-greater? l k)
  (cond
    [(empty? l) true] ; An empty list vacuously satisfies the condition
    [(<= (car l) k) false] ; If any element is not greater than k, return false
    [else (all-greater? (cdr l) k)])) ; Recursively check the rest of the list

(check-equal? (all-greater? '(2 3 4) 2) false)
(check-equal? (all-greater? '(2 3 4) 1) true)
(check-equal? (all-greater? '(2 3 4) 3) false)
(check-equal? (all-greater? '() 5) true) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2: sorted?
; key concepts: helper functions

; returns true if the list is sorted and false otherwise
; sorted?: number list -> bool
(define (sorted? l)
  (cond
    [(empty? l) true] ; An empty list is sorted
    [(empty? (cdr l)) true] ; A list with one element is sorted
    [(> (car l) (cadr l)) false] ; If the first element is greater than the second, it's not sorted
    [else (sorted? (cdr l))])) ; Recursively check the rest of the list

(check-equal? (sorted? '(1 2 3)) true)
(check-equal? (sorted? '(1 3 2)) false)
(check-equal? (sorted? '(3 2 1)) false)
(check-equal? (sorted? '()) true)
(check-equal? (sorted? '(1)) true) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 3: sum-tree

; type tree =
;   | node of tree * tree
;   | leaf of number
(struct node (l r) #:transparent)
(struct leaf (x) #:transparent)

; sum-all elements of a tree t
; sum-all: tree -> number
(define (sum-tree t)
  (cond
    [(leaf? t) (leaf-x t)] ; If it's a leaf, return its value
    [(node? t) (+ (sum-tree (node-l t)) (sum-tree (node-r t)))])) ; If it's a node, sum the left and right subtrees

(check-equal? (sum-tree (leaf 5)) 5)
(check-equal? (sum-tree (node (leaf 1) (leaf 2))) 3)
(check-equal? (sum-tree (node (node (leaf 1) (leaf 2)) (leaf 3))) 6) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 4: right-linear?

(define (right-linear? t)
  (cond
    [(leaf? t) true] ; A leaf is right-linear
    [(node? t) 
     (and (leaf? (node-l t)) (right-linear? (node-r t)))])) ; A node is right-linear if its left child is a leaf and its right subtree is right-linear

(check-equal? (right-linear? (leaf 10)) true)
(check-equal? (right-linear? (node (leaf 1) (leaf 2))) true)
(check-equal? (right-linear? (node (node (leaf 3) (leaf 4)) (leaf 5))) false)
(check-equal? (right-linear? (node (leaf 6) (node (leaf 7) (leaf 8)))) true) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 5: dfs-list
; convert a tree to a depth-first left-first list
; example:
;     /\
;    / 3
;   /\
;   1 2
; --> '(1 2 3)


; dfs-list: tree -> number list
(define (dfs-list t)
  (cond
    [(leaf? t) (list (leaf-x t))] ; If it's a leaf, return a list containing its value
    [(node? t) (append (dfs-list (node-l t)) (dfs-list (node-r t)))])) ; If it's a node, append the DFS lists of its left and right subtrees

(check-equal? (dfs-list (node (node (leaf 1) (leaf 2)) (leaf 3))) '(1 2 3))
(check-equal? (dfs-list (leaf 5)) '(5))
(check-equal? (dfs-list (node (leaf 4) (node (leaf 6) (leaf 7)))) '(4 6 7)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 6: subst

; subst: tree -> int -> int -> tree
(define (subst t old n)
  (cond
    [(leaf? t) (if (= (leaf-x t) old) (leaf n) t)] ; If it's a leaf, replace its value if it matches 'old'
    [(node? t) (node (subst (node-l t) old n) (subst (node-r t) old n))])) ; If it's a node, recursively substitute in its subtrees

; your tests here
(check-equal? (subst (leaf 5) 5 10) (leaf 10))
(check-equal? (subst (leaf 5) 3 10) (leaf 5))
(check-equal? (subst (node (leaf 1) (leaf 2)) 1 3) (node (leaf 3) (leaf 2))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 7: multi-subst

(define (multi-subst t tbl)
  (cond
    [(leaf? t) (let ([new-val (hash-ref tbl (leaf-x t) (leaf-x t))]) ; Get new value, defaulting to original
                 (if (leaf? new-val) ; If the new value is already a leaf, use it directly
                     new-val
                     (leaf new-val)))] ; Otherwise, wrap it in a leaf
    [(node? t) (node (multi-subst (node-l t) tbl) (multi-subst (node-r t) tbl))]))
(define test-tbl (hash 1 3 2 4))

(check-equal?
 (multi-subst (node (leaf 1) (leaf 20)) test-tbl)
 (node (leaf 3) (leaf 20))
 )
; your tests here
(check-equal? (multi-subst (leaf 1) test-tbl) (leaf 3))
(check-equal? (multi-subst (leaf 20) test-tbl) (leaf 20))
(check-equal? (multi-subst (node (leaf 1) (leaf 2)) test-tbl) (node (leaf 3) (leaf 4))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 8: my-filter

(define (my-filter l f)
  (cond
    [(empty? l) '()] ; If the list is empty, return an empty list
    [(f (car l)) (cons (car l) (my-filter (cdr l) f))] ; If the predicate is true for the first element, include it and recurse
    [else (my-filter (cdr l) f)])) ; Otherwise, skip the first element and recurse

(check-equal? (my-filter '(1 2 3) (lambda (x) (< x 3))) '(1 2))
; your tests here
(check-equal? (my-filter '(1 2 3 4 5) even?) '(2 4))
(check-equal? (my-filter '("apple" "banana" "cherry") (lambda (x) (> (string-length x) 5))) '("banana" "cherry"))


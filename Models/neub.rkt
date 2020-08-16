#lang racket
(require redex)

(define-language nat
  [N ::= Zero (Plus1 N)])

(redex-match? nat N (term Zero))
(redex-match? nat (Plus1 N_a) (term Zero))
(redex-match? nat (Plus1 N_1) (term Zero))
(redex-match? nat (Plus1 N_1) (term (Plus1 Zero)))
(redex-match? nat (Plus1 N_0) (term (Plus1 (Plus1 Zero))))

(redex-match nat N (term Zero))
(redex-match nat (Plus1 N_a) (term Zero))
(redex-match nat (Plus1 N_1) (term Zero))
(redex-match nat (Plus1 N_1) (term (Plus1 Zero)))
(redex-match nat (Plus1 N_0) (term (Plus1 (Plus1 Zero))))

;;non-terminals
(define-language trees
  [binary-tree ::= Leaf
               (Node binary-tree binary-tree)])

(redex-match trees
             (Node binary-tree_left binary-tree_right)
             (term (Node Leaf (Node Leaf Leaf))))





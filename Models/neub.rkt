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



;;judgement forms for the natural language
(define-judgment-form nat
  #:mode (N= I I)
  #:contract (N= N N)
  [----- "zero"
         (N= Zero Zero)]
  [
   (where (Plus1 N_0) N_0)
   (where (Plus1 N_1) N_1)
   (N= N_0 N_1)
   ------- "Plus1"
   (N= N_0 N_1)])

(judgment-holds (N= Zero Zero))
(judgment-holds (N= Zero Zero))



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
   (where (Plus1 N_0--) N_0)
   (where (Plus1 N_1--) N_1)
   (N= N_0-- N_1--)
   ------- "Plus1"
   (N= N_0 N_1)])

(judgment-holds (N= Zero Zero))
(judgment-holds (N= Zero (Plus1 Zero)))
(judgment-holds (N= (Plus1 Zero) (Plus1 Zero)))
(judgment-holds (N= (Plus1 (Plus1 Zero)) (Plus1 (Plus1 Zero))))
(judgment-holds (N= (Plus1 Zero) (Plus1 (Plus1 Zero))))
(judgment-holds (N= (Plus1 (Plus1 Zero)) (Plus1 (Plus1 Zero))))

;;Type inference
(define-language Arith
  (e ::= integer (e + e))
  (τ ::= Int))

(define-judgment-form Arith
  #:mode (infer-type I O)
  #:contract (infer-type e τ)
  [------- "T-Int"
           (infer-type e_0 Int)])

;;(judgment-holds (infer-type (e + e) _))


(define-language SomeTypes
  (τ ::= (-> τ τ) Integer))


(define-judgment-form SomeTypes
  #:mode (<: I I)
  #:contract (<: τ τ)

  ;;this rule will not work
  ;;this is because there is some guessing
  ;;game going on
  ;;[(<: τ_0 τ_1)
  ;; (<: τ_1 τ_2)
  ;; ----------- "S-Trans"
   ;;(<: τ_0 τ_2)]

  [ ----------- "S-Reflx"
                (<: τ_0 τ_0)]

  [(<: τ_dom-1 τ_dom-0)
   (<: τ_cod-0 τ_cod-1)
   --------------------- "S-Arrow"
   (<: (-> τ_dom-0 τ_cod-0) (-> τ_dom-1 τ_cod-1))])



(judgment-holds  (<: Integer Integer))
(judgment-holds  (<: Integer Integer))

;;Metafunctions
(define-metafunction nat
  N=? :  N N -> boolean
  [(N=? Zero Zero)
  #true]
  [(N=? N_0 N_1)
   (N=? N_0-- N_1--)
   (where (Plus1 N_0--) N_0)
   (where (Plus1 N_1--) N_1)]
   [(N=? N_0 N_1)
    #false])

;;Reduction relations
(define-language Bool
  (bexp ::= #true #false (bexp ∨ bexp) (bexp ∧ bexp))
  (val ::= #true #false)
  (E ::= hole (E ∧ bexp) (val ∧ E) (E ∨ bexp) (val ∨ E)))


(define step
  (reduction-relation Bool
                      #:domain bexp
                      [--> (in-hole E (val_lhs ∧ val_rhs))
                           (in-hole E val_new)
                           ∧-step
                           (where val_new ,(and (term val_lhs) (term val_rhs)))]
                      [--> (in-hole E (val_lhs ∨ val_rhs))
                           (in-hole E val_new)
                           ∨-step
                           (where val_new ,(or (term val_lhs) (term val_rhs)))]
                      ))

  ;;Applying reduction relations
(apply-reduction-relation step (term #true))
(apply-reduction-relation step (term (#true ∧ #true)))
(apply-reduction-relation step (term ((#true ∧ #false) ∧ (#false ∨ #true))))


                          
                         
                                    
                                            

        


  







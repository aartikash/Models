#lang racket/base
(require redex/reduction-semantics)

(define-language BoxyL
  (e ::= x natural (+ e e) (cons e e) (car e) (cdr e) (\lambda (x : A) e) (e e)
     (box e) (let ((box y) e) e))
  (A B ::= Nat (A \times B) (A \rightarrow B) ( \square A))
  (x y ::= variable-not-otherwise-mentioned)
  (v ::= natural (box e) (\lambda (x : A) e) (cons v v))
  

#:binding-forms
(\lambda (x : A) e #:refers-to x)
(let ((box y) e_1) e_2 #:refers-to y))

;;Pattern matching
(define eg1 (term (box 5)))
(define eg2 (term (cdr 5)))

(define-extended-language BoxyBoolL BoxyL
  (e ::= ....boolean))

(define-extended-language BoxyEvalL BoxyL
   (E ::= hole (E e) (v E) (cons v ... E e ...) (+ v ... E e ...) (car E) (cdr E)
          (let ((box x) E) e)))

;;(define-extended-language BoxyEvalL BoxyL
;;    (E ::= hole (E e) (v E) (cons v ... E e ...) (+ v ... E e ...) (car E) (cdr E)
;;           (let ((box x) E) e)))
;;(define eg3 (term (car (cons (+ 2 4) 2)))) 
;;(redex-match BoxyEvalL (in-hole E v) (term (car (cons (+ 1 2) 2)))) 
(redex-match BoxyEvalL (in-hole E v) (term (car (cons (+ 1 2) 2))))



;;(define b (box "apple"))
;; b

(default-language BoxyL)
 (alpha-equivalent? (term (λ (x : Nat) e))  (term (λ (x : Nat) e)))
 (alpha-equivalent? (term (λ (x : Nat) e))  (term (λ (y : Nat) e_2)))

(define an-e1 (generate-term BoxyL e 1))
an-e1

(default-language BoxyL)

(test-predicate (redex-match? BoxyL x) (term kar))
(test-predicate (redex-match? BoxyL x) (term car))
(test-results)


(define ->
  (reduction-relation
   BoxyL
   #:domain e
   #:codomain e
   (--> (( \lambda (x : A) e_1) e_2) (substitue e_1 x e_2)
        "Rule1")
   (--> (car (cons e_1 e_2)) e_1
        "Rule2")
   (--> (cdr (cons e_1 e_2)) e_1
        "Rule3")
   (--> (+ v_1 v_2) ,(+ (term v_1) (term v_2))
        "Rule4")
  ;; (--> (cons e_1 e_2) ,(list e_1 e_2)
  ;;      "Rule5")
   ))

(apply-reduction-relation -> (term (+ 3 4)))
(apply-reduction-relation -> (term (car (cons 1 2))))
(apply-reduction-relation -> (term (car (cons (+ 1 2) 2))))
(apply-reduction-relation* -> (term (car (cons (+ 1 2) 2))))

(define ->* (compatible-closure -> BoxyL e))
(apply-reduction-relation* -> (term (car (cons (+ 1 2) 2))))
(apply-reduction-relation* -> (term (\lambda (x : Nat) (car (cons (+ 1 2) 2)))))
(apply-reduction-relation* ->* (term (\lambda (x : Nat) (car (cons (+ 1 2) 2)))))
;;(apply-reduction-relation* ->* (term (\lambda (x : Nat) (+ x 1) 3)))

(define ->cbv (context-closure -> BoxyEvalL E))
(apply-reduction-relation ->cbv (term (\lambda (x : Nat) (car (cons (+ 1 2) 2)))))
(apply-reduction-relation* ->cbv (term (\lambda (x : Nat) (car (cons (+ 1 2) 2)))))

(test--> -> (term (+ 1 2)) (term 3))
(test--> -> #:equiv alpha-equivalent?
           (term ((λ (x : Nat) (λ (y : Nat) y)) 1))
           (term (λ (z : Nat) z)))

(test-->> ->* #:equiv alpha-equivalent?
            (term (λ (x : Nat) (car (cons (+ 1 2) 2))))
            (term (λ (z : Nat) 3)))
(test-results)

   
   
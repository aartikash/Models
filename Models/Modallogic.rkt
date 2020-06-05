#lang racket/base
(require redex/reduction-semantics)

(define-language BoxyL
  (e ::= x natural (+ e e) (cons e e) (car e) (cdr e) (\lambda  (x : A) e) (e e)
     (box e) (let ((box y) e) e))
  (A B ::= Nat (A × B) (A → B) (□ A))
  (x y ::= variable-not-otherwise-mentioned)
  (v ::= natural (box e) (\lambda (x : A) e) (cons v v))


  

#:binding-forms
(\lambda (x : A) e #:refers-to x)
(let ((box y) e_1) e_2 #:refers-to y))

;;Pattern matching
(define eg1 (term (box 5)))
(define eg2 (term (cdr 5)))

(define-extended-language BoxyBoolL BoxyL
  (e ::= .... boolean))

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
;; Alpha equivalence does  not  check if they are valid terms
(default-language BoxyL)
 (alpha-equivalent? (term (\lambda (x : Nat) e))  (term (\lambda (x : Nat) e)))
 (alpha-equivalent? (term (\lambda (x : Nat) e))  (term (\lambda (y : Nat) e_2)))

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
   (--> (( \lambda (x : A) e_1) e_2) (substitute e_1 x e_2)
        "Rule1")
   (--> (car (cons e_1 e_2)) e_1
        "Rule2")
   (--> (cdr (cons e_1 e_2)) e_1
        "Rule3")
   (--> (+ v_1 v_2) ,(+ (term v_1) (term v_2))
        "Rule4")
  ;; (--> (cons e_1 e_2) ,(list e_1 e_2)
  ;;      "Rule5")
   (--> (let ((box x) (box e_1)) e_2) (substitute e_1 x e_2)
        "Rule 6"
   )))

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



;;Metafunctions
(define-metafunction BoxyL
  boxy-eval : e -> v
  [(boxy-eval e)
   ,(car (apply-reduction-relation* ->cbv (term e)))])

(term (boxy-eval (\lambda (x : Nat) (car (cons (+ 1 2) 2)))))
(term (boxy-eval (\lambda (x : Nat) (cdr (cons (+ 1 2) 2)))))
(term (boxy-eval (\lambda (x : Nat) (cons (+ 1 2) 2))))
(term (boxy-eval (\lambda (x : Nat) (cons (+ 1 2) 2))))

(define-metafunction BoxyL
  eval2 : e -> e
  [(eval2 e)
   ,(car (apply-reduction-relation* ->cbv (term e)))])
(term (eval2 (let ((box y) 1) 2)))
(term (eval2 (\lambda (x : Nat) (cons (+ 1 2) 2))))


(define-metafunction BoxyL
  eval3 : e -> e
  [(eval3 e)
   ,(car (apply-reduction-relation* ->* (term e)))])
(term (eval3 (let ((box y) 1) 2)))
(term (eval3 (\lambda (x : Nat) (cons (+ 1 2) 2))))

;; Fail tests check
;;(define-metafunction BoxyL
;;  eval4 : e -> v
;;  [(eval4 e)
;;     ,(car (apply-reduction-relation* ->* (term e)))])
;;(term (eval4 (let ((box y) 1) 2)))
;; (term (eval4 (\lambda (x : Nat) (cons (+ 1 2) 2))))

(define-metafunction BoxyL
    normalize : e -> e
    [(normalize e)
     ,(car (apply-reduction-relation* ->* (term e)))])

;;Testing reduction relations

(test--> -> (term (+ 1 2)) (term 3))
(test--> -> #:equiv alpha-equivalent?
           (term ((\lambda (x : Nat) (\lambda (y : Nat) y)) 1))
           (term (\lambda (z : Nat) z)))

(test-->> ->* #:equiv alpha-equivalent?
            (term (\lambda (x : Nat) (car (cons (+ 1 2) 2))))
            (term (\lambda (z : Nat) 3)))
(test-results)


;;To  test reduction of one term to another
(test-->>E ->*
            (term (\lambda (x : Nat) (car (cons (+ 1 2) 2))))
            (term (\lambda (x : Nat) (car (cons (+ 1 2) 2)))))


(test-->>∃ ->*
            (term (\lambda (x : Nat) (car (cons (+ 1 2) 2))))
            (term (\lambda (x : Nat) (car (cons 3 2)))))

(test-->>∃ #:steps 1 ->*
             (term (\lambda (x : Nat) (car (cons (+ 1 2) 2))))
             (term (\lambda (x : Nat) 3)))

(test-->>∃ #:steps 4 ->*
             (term (\lambda (x : Nat) (car (cons (+ 1 2) 2))))
             (term (\lambda (x : Nat) 3)))
(test-results)
            
(test-->>∃ #:steps 0 ->*
             (term (\lambda (x : Nat) (car (cons (+ 1 2) 2))))
             (term (\lambda (x : Nat) 3)))
;;(test-results)


;;Judgements

(define-judgment-form BoxyL
    #:contract (≡ e e)
    #:mode (≡ I I)
  
    [(where (e e) ((normalize e_1) (normalize e_2)))
     ----------- "β"
     (≡ e_1 e_2)]
  
    [(≡ e_1 (e_2 x))
     --------------- "η1"
     (≡ (\lambda (x : A) e_1) e_2)]
  
    [(≡ (e_2 x) e_1)
     ------------------------ "η2"
     (≡ e_2 (\lambda (x : A) e_1))])

;;Testing judgements
(judgment-holds
   (≡ (\lambda (x : Nat) ((\lambda (x : Nat) x) 5))
      (\lambda (x : Nat) 5)))
(judgment-holds
   (≡ (f 5)
      (\lambda (x : Nat) ((f 5) x))))
(judgment-holds
(≡ (\lambda (x : Nat) ((\lambda (x : Nat) e) 5))
      (\lambda (x : Nat) 5)))
;;Failed  judgements
;;(judgment-holds
;;   (≡ (\lambda (x : Nat) ((\lambda (x : Nat) e) (6 5))
;;      (\lambda (x : Nat) 4 5))))

;;Typing extension
(define-extended-language BoxyTypingL BoxyL
    (Γ Δ ::= · (Γ (x : A))))

(define-metafunction BoxyTypingL
    different : x x -> boolean
    [(different x x) #f]
    [(different x y) #t])

(define-metafunction BoxyTypingL
    double : x x -> boolean
    [(double x y) #f]
    [(double x 2x) #t])

;;Type system

(define-judgment-form BoxyTypingL
    #:contract (type-infer Δ Γ e A)
    #:mode (type-infer I I I O)
  
    [------------------------------- "T-VarLocal"
     (type-infer Δ (Γ (x : A)) x A)]
  
    [(type-infer Δ Γ x_1 A)
     (where #t (different x_1 x_2))
     ------------------------------- "T-VarLocalWeak"
     (type-infer Δ (Γ (x_2 : B)) x_1 A)]
  
    [------------------------------- "T-VarGlobal"
     (type-infer (Δ (x : A)) · x A)]
  
    [(type-infer Δ · x_1 A)
     ----------------------------------- "T-VarGlobalWeak"
     (type-infer (Δ (x_2 : B)) · x_1 A)]
  
    [-------------------------- "T-Nat"
     (type-infer Δ Γ natural Nat)]
  
    [(type-infer Δ Γ e_1 Nat)
     (type-infer Δ Γ e_2 Nat)
     -------------------------- "T-Plus"
     (type-infer Δ Γ (+ e_1 e_2) Nat)]
  
    [(type-infer Δ Γ e_1 A)
     (type-infer Δ Γ e_2 B)
     -------------------------- "T-Cons"
     (type-infer Δ Γ (cons e_1 e_2) (A × B))]
  
    [(type-infer Δ Γ e (A × B))
     -------------------------- "T-Car"
     (type-infer Δ Γ (car e) A)]
  
    [(type-infer Δ Γ e (A × B))
     -------------------------- "T-Cdr"
     (type-infer Δ Γ (cdr e) B)]
  
    [(type-infer Δ (Γ (x : A)) e B)
     -------------------------- "T-Fun"
     (type-infer Δ Γ (\lambda (x : A) e) (A → B))]
  
    [(type-infer Δ Γ e_1 (A → B))
     (type-infer Δ Γ e_2 A)
     -------------------------- "T-App"
     (type-infer Δ Γ (e_1 e_2) B)]
  
    [(type-infer Δ · e A)
     -------------------------- "T-Box"
     (type-infer Δ Γ (box e) (□ A))]
  
    [(type-infer Δ Γ e_1 (□ A))
     (type-infer (Δ (x : A)) Γ e_2 B)
     -------------------------- "T-LetBox"
     (type-infer Δ Γ (let ((box x) e_1) e_2) B)])

;;Tests for typing judgements

(judgment-holds (type-infer · · (box 1) A) (term A))
(judgment-holds (type-infer · · (box 5) A) (term B))
(judgment-holds (type-infer · · (box 5) B) (term B))
(judgment-holds (type-infer · · (box (+ 4 5)) B) (term B))
(judgment-holds (type-infer · · (box (car (4 5))) B) (term B))
;;(judgment-holds (type-infer · · · (+ 4 5)) B) (term B)
(judgment-holds (type-infer · · (\lambda (x : Nat) (box 1)) A) (term A))
(judgment-holds (type-infer · · (\lambda (x : Nat) (box x)) A) (term A))

;;Building derivations
(build-derivations (type-infer · · (box 1) A))
(build-derivations (type-infer · · (box (car (4 5))) B))
(build-derivations (type-infer · · (box (+ 4 5)) B))
(build-derivations (type-infer · · (\lambda (x : Nat) (box 1)) A))

(define-metafunction BoxyL
    type-equal? : A B -> boolean
    [(type-equal? A A) #t]
    [(type-equal? A B) #f])
(define-judgment-form BoxyTypingL
    #:contract (type-check Δ Γ e A)
    #:mode (type-check I I I I)
  
    [(type-infer Δ Γ e A)
     (where #t (type-equal? A B))
     --------------------
     (type-check Δ Γ e B)])

;;Testing typing judgements
(judgment-holds (type-check · · (box 5) (□ Nat)))
(test-judgment-holds (type-check · · (box 5) (□ Nat)))















   
   
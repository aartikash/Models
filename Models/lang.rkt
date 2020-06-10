#lang racket

(require redex)
(provide λ λv red typeof)

(define-language λ
  (e (e e)
     x
     (λ (x τ) e)
     (if0 e e e)
     (+ e e)
     number)
  (τ (τ -> τ) num)
  (x variable-not-otherwise-mentioned))


(define-extended-language λv λ
  (v (λ (x τ) e) number)
  (E hole
     (v E) (E e)
     (if0 E e e)
     (+ E e) (+ v E))
  (Γ · (x τ Γ)))



(define red
  (reduction-relation
   λv
   (--> (in-hole E (+ number_1 number_2))
        (in-hole E (Σ number_1 number_2))
        "+")
   (--> (in-hole E (if0 0 e₁ e₂))
        (in-hole E e_1)
        "if0t")
   (--> (in-hole E (if0 number e₁ e₂))
        (in-hole E e_2)
        "if0f"
        (side-condition
         (not (= 0 (term number)))))
   (--> (in-hole E ((λ (x τ) e) v))
        (in-hole E (subst x v e))
        "βv")))
   

(define-metafunction λv
  Σ : number number -> number
  [(Σ number₁ number₂)
   ,(+ (term number₁) (term number₂))])


;;Substitution
(define-metafunction λv
  subst : x any any -> any
  ;; 1. x_1 bound, so don't continue in λ body
  [(subst x_1 any_1 (λ (x_1 τ_1) any_2))
   (λ (x_1 τ_1) any_2)]
  ;; 2. general purpose capture avoiding case
  [(subst x_1 any_1 (λ (x_2 τ_2) any_2))
   (λ (x_new τ_2)
     (subst x_1 any_1
            (subst-var x_2 x_new
                       any_2)))
   (where (x_new)
          ,(variables-not-in
            (term (x_1 any_1 any_2)) 
            (term (x_2))))]
  ;; 3. replace x_1 with e_1
  [(subst x_1 any_1 x_1) any_1]
  ;; 4. x_1 and x_2 are different, so don't replace
  [(subst x_1 any_1 x_2) x_2]
  ;; the last cases cover all other expressions
  [(subst x_1 any_1 (any_2 ...))
   ((subst x_1 any_1 any_2) ...)]
  [(subst x_1 any_1 any_2) any_2])

(define-metafunction λv
  subst-var : x any any -> any
  [(subst-var x_1 any_1 x_1) any_1]
  [(subst-var x_1 any_1 (any_2 ...))
   ((subst-var x_1 any_1 any_2) ...)]
  [(subst-var x_1 any_1 any_2) any_2])










                               

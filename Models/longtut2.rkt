#lang racket
(require redex)

(define-language L
  (e (e e)
     (λ (x t) e)
     x
     (amb e ...)
     number
     (+ e ...)
     (if0 e e e)
     (fix e))
  (t (-> t t) num)
  (x variable-not-otherwise-mentioned))

(redex-match L e (term (λ (x t) e)))
(redex-match L e (term (λ (x x) e)))
(redex-match L e (term (λ (x num) e)))
(redex-match L e (term (e e)))
(redex-match L e (term (x)))
(redex-match L e (term (4)))
(redex-match L e (term (amb e)))
(redex-match L e (term (e amb x 1)))
(redex-match L e (term (amb x 1 1)))
(redex-match L e (term (amb e e x)))

;;Type environments

(define-extended-language L+Γ L
  [Γ · (x : t Γ)])


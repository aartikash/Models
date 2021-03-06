#lang racket/base
(require redex)

(define-language iswim
  ((M  N L K) X (λ X M) (M M) b (o2 M M) (o1 M))
  (o o1 o2)
  (o1 add1 sub1 iszero)
  (o2 + - * \uparrow)
  (b number)
  ((V U W) b X (λ X M))
  (E hole (V E) (E M) (o V ... E M ...))
  ((X Y Z) variable-not-otherwise-mentioned))

;;Test
(redex-match  iswim (in-hole E number) (term (+  1 2)))
(redex-match  iswim (in-hole E E) (term (+  1 2)))


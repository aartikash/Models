#lang racket
(require redex)

(define-language lc-lang
  (e ::= (e e ...)
     x
     (λ (x ...) e))
  (v ::= (λ (x ...) e))
  (E ::= (v ... E e ...)
     hole)
  (x y ::= variable-not-otherwise-mentioned)
  #:binding-forms
  (λ (x) e #: refers-to x))




  
(define-metafunction lc-lang
  biggest : natural natural -> natural
  [(biggest natural_1 natural_2)
   natural_2
   (side-condition (< (term natural_1) (term natural_2)))]
  [(biggest natural_1 natural_2)
   natural_1])

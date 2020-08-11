#lang racket
(require redex)

(define-language Λ
 ( e ::= x
  (λ (x_!_ ...) e)
  (e e ...))
  (x ::= variable-not-otherwise-mentioned))

(define e1 (term y))
(define e2 (term (λ (y) y)))
(define e3 (term (λ (x y) y)))
(define e4 (term (,e2 ,e3)))
(define e5 (term (λ (x x) ,e2)))
(define e6 (term m))
e6
(define e7 (term (x y z)))
e7
;;(define e8 (term x y z))
;;e8

(redex-match? Λ e e1)

;;Defining predicate for tests
(define lambda? (redex-match? Λ e))
;;Tests
(test-equal (lambda? e1) #true)
(test-equal (lambda? e2) #true)
(test-equal (lambda? e3) #true)
(test-equal (lambda? e4) #true)
(test-equal (lambda? e5) #true)

(define eb1 (term (λ ( x x) y)))
(define eb2 (term (λ (x y) 3)))

(test-equal (lambda? eb1) #true
            )
(test-equal (lambda? eb2) #false)

(test-results)


;;Tests - what the test is basically telling us is that 
(module+ test
  (test-equal (term (unique-vars x y)) #true)
  (test-equal (term (unique-vars x y x)) #false))

;;Metafunctions
(define-metafunction Λ
  unique-vars : x ... -> boolean
  [(unique-vars) #true]
  [(unique-vars x x₁ ... x x₂ ...) #false]
  )



(module+ test
  (test-results))




  







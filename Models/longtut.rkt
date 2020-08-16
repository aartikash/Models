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
;;(test-equal (lambda? e1) #true)
;;(test-equal (lambda? e2) #true)
;;(test-equal (lambda? e3) #true)
(test-equal (lambda? e4) #true)
;;(test-equal (lambda? e5) #true)

(define eb1 (term (λ ( x x) y)))
(define eb2 (term (λ (x y) 3)))

;;(test-equal (lambda? eb1) #true)
(test-equal (lambda? eb2) #false)

;;(test-results)


;;Tests - what the test is basically telling us is that 
;;(module+ test
  ;;(test-equal (term (unique-vars x y)) #true)
  ;;(test-equal (term (unique-vars x y x)) #false)
  ;;(test-equal (term (unique-vars x y x x x)) #false))






(module+ test
  (test-results))


(module+ test
  (test-equal (term (subtract (x y z x) x z)) (term (y))))

(define-metafunction Λ
  unique-vars : x ... -> boolean
  [(unique-vars) #true]
  [(unique-vars x x_1 ... x x_2 ...) #false]
  [(unique-vars x x_1 ...) (unique-vars x_1 ...)])

(module+ test
  (test-results))


(module+ test
  (test-equal (term (subtract (x y z x) x)) (term (y z))))

(define-metafunction Λ
  subtract1 : (x ...) x -> (x ...)
  [(subtract1 (x_1 ... x x_2 ...) x)
   (x_1 ... x_2new ...)
   (where (x_2new ...) (subtract1 (x_2 ...) x))
   (where #false (in x (x_1 ...)))]
  [(subtract1 (x ...) x_1) (x ...)])

(define-metafunction Λ
  in : x (x ...) -> boolean
  [(in x (x_1 ... x x_2 ...)) #true]
  [(in x (x_1 ...)) #false])

;;Language 2

(module+ test
  (module+ test
    (test-equal (term (fv x)) (term (x)))
    (test-equal (term (fv (λ (x) x))) (term ()))
    (test-equal (term (fv (λ (x) (y z x)))) (term (y z))))
    (test-equal (term (fv (λ (x) (y z x m n)))) (term (y z m n)))
    (test-equal (term (fv (λ (x y) (y z x)))) (term (z))))

(module+ test
  (test-results))


(define-metafunction Λ
  fv : e -> (x ...)
  [(fv x) (x)]
  [(fv (λ (x ...) e))
   (subtract (x_e ...) x ...)
   (where (x_e ...) (fv e))]
  [(fv (e_f e_a ...))
   (x_f ... x_a ... ...)
   (where (x_f ...) (fv e_f))
   (where ((x_a ...) ...) ((fv e_a) ...))])

;;Accumulator functions in redex

(define-extended-language SD Λ
  (e ::= .... (K n ))
  (n ::= natural))

(define sd1 (term (K 1)))
(define sd2 (term 1))

(define SD? (redex-match? SD e))

(module+ test
  (test-equal (SD? sd1) #true))

(define-metafunction SD
  sd : e -> e
  [(sd e_1) (sd/a e_1())])


(test-results)

(module+ test
  (test-equal (term (sd/a x ())) (term x))
  (test-equal (term (sd/a x ((y) (z) (x)))) (term (K 2 0)))
  (test-equal (term (sd/a ((lambda (x) x) (lambda (y) y)) ()))
              (term ((lambda () (K 0 0)) (lambda () (K 0 0)))))
  (test-equal (term (sd/a (lambda (x) (x (lambda (y) y))) ()))
              (term (lambda () ((K 0 0) (lambda () (K 0 0))))))
  (test-equal (term (sd/a (lambda (z x) (x (lambda (y) z))) ()))
              (term (lambda () ((K 0 1) (lambda () (K 1 0)))))))

(module+ test
  (test-results))
 
(define-metafunction SD
  sd/a : e ((x ...) ...) -> e
  [(sd/a x ((x_1 ...) ... (x_0 ... x x_2 ...) (x_3 ...) ...))
   ; bound variable 
   (K n_rib n_pos)
   (where n_rib ,(length (term ((x_1 ...) ...))))
   (where n_pos ,(length (term (x_0 ...))))
   (where #false (in x (x_1 ... ...)))]
  [(sd/a (lambda (x ...) e_1) (e_rest ...))
   (lambda () (sd/a e_1 ((x ...) e_rest ...)))]
  [(sd/a (e_fun e_arg ...) (e_rib ...))
   ((sd/a e_fun (e_rib ...)) (sd/a e_arg (e_rib ...)) ...)]
  [(sd/a e_1 e)
   ; a free variable is left alone 
   e_1])

(define-extended-language Lambda/n Λ
  (e ::= .... n)
  (n ::= natural))

(define in-Lambda/n? (redex-match? Lambda/n e))

(module+ test
  (test-equal (term (=α (λ (x) x) (λ (y) y))) #true)
  (test-equal (term (=α (λ (x) (x 1)) (λ (y) (y 1)))) #true)
  (test-equal (term (=α (λ (x) x) (λ (y) z))) #false))


(define-metafunction SD
  =α : e e -> boolean
  [(=α e_1 e_2) ,(equal? (term (sd e_1)) (term (sd e_2)))])

(define (=α/racket x y) (term (=α ,x ,y)))



(define-extended-language SD Λ
  (e ::= ....
     true
     
     false
     (if e e e)))


(module+ test
  (test-equal (SD? sd1) #true))

(define-metafunction SD
  sd : any -> any
  [(sd any_1) (sd/a any_1 ())])

(module+ test
  (test-equal (term (sd/a x ())) (term x))
  (test-equal (term (sd/a x ((y) (z) (x)))) (term (K 2 0)))
  (test-equal (term (sd/a ((lambda (x) x) (lambda (y) y)) ()))
              (term ((lambda () (K 0 0)) (lambda () (K 0 0)))))
  (test-equal (term (sd/a (lambda (x) (x (lambda (y) y))) ()))
              (term (lambda () ((K 0 0) (lambda () (K 0 0))))))
  (test-equal (term (sd/a (lambda (z x) (x (lambda (y) z))) ()))
              (term (lambda () ((K 0 1) (lambda () (K 1 0)))))))









  
  



  







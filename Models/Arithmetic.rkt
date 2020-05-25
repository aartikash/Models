#lang racket/base
(require redex)

(define-language arith
  (e (binop e e)
     (sqrt e)
     number)
  (binop +
         -
         )
  (e-ctxt (binop e e-ctxt)
          (binop e-ctxt e)
          (sqrtu e-ctxt)
          hole)
  (v number))

(define eg1 (term (+ 3 2)))
(define eg3 (term (+ 3 (+ 2 5))))
(define eg2 (term (df)))


(redex-match arith any eg1)
(redex-match? arith any eg1)
(redex-match arith any eg3)
(redex-match? arith any eg3)
(redex-match arith e eg2)

;;reduction-relations

(define ->
  (reduction-relation
   arith

   (--> (+ number_1 number_2)
         ,(+ (term number_1) (term number_2))
         "add")
   (--> (- number_1 number_2)
         ,(- (term number_1) (term number_2))
         "sub")
  ;; with
   ;;[(--> (in-hole  e-ctxt_1 a) (in-hole e-ctxt_1 b))
    ;;(c--> a b)
   ;; ]
   
   )
  )

   ;;Generate terms
   (define e1 (generate-term arith e 10))
   e1
;;Test reductions

(apply-reduction-relation -> (term (+ 3 4)))
(apply-reduction-relation -> (term (car (cons (+ 1 2) 2))))
(apply-reduction-relation* -> (term (car (cons (+ 1 2) 2))))
(apply-reduction-relation* -> (term (+ 2 (+ 1 2) )))
(apply-reduction-relation* -> (term (djvf)))




 
   


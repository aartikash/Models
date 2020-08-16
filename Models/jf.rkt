#lang racket
(require redex)

(define-language nats
  (n ::= z (s n)))

(define-judgment-form nats
  #:mode (sum I I O)
  #:contract (sum n n n)

  [------------ "zero"
                (sum z n n)]

  [(sum n_1 n_2 n_3)
   --------------------- "add1"
   (sum (s n_1) n_2 (s n_3))])


(judgment-holds (sum (s (s z)) (s z) (s (s (s z)))))
(judgment-holds (sum (s (s z)) (s z) (s (s (s n)))))
(judgment-holds (sum (s (s z)) (s z) (s (s (s (s n))))))

;;Different type of judgment form
;;all pairs with given sums

(define-judgment-form nats
  #:mode (sumr O O I)
  #:contract (sumr n n n)
  [--------- "z"
             (sumr z n n)]
  
  [(sumr n_1 n_2 n_3)
   -------------------- "s"
   (sumr (s n_1) n_2 (s n_3))])


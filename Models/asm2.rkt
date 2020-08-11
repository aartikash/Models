#lang racket/base

(require redex)

(define-language asmL
  ;;registers
  (r ::= (variable-prefix r))
  ;;memory
  (m ::= (variable-prefix m))
  ;;naturals
  (N ::= (variable-prefix n) pc i j k)
  ;;States - includes memory, registers, flags and pc 
  (s ::= (M R f pc))
  ;;Instructions
  (i ::= ( add r_i r_j) (const k r_i) (jmp r_i)
     (jz r_i) (load r_i r_j) (store r_i r_j) (cmp r_i r_j)
     (set r_i r_j))
  ;;Register file
  (R ::= ((r₀ n₀) (r₁ n₀) ... ))
  ;;Booleans
  (b ::= true false)
  ;;Flags
  (f ::= zf = b)
  ;;Memory
  (M ::= ((n₀ m₀) (n₁ m₁) ... ))
  )

;;Tests
(redex-match? asmL r
              (term (r₀)))
(redex-match? asmL r (term r₀))
(redex-match? asmL any (term r₀))

(redex-match? asmL R (term r₀))

(redex-match? asmL (r 2) (term r₀))
(redex-match? asmL R (term (r₀ 2)))
(redex-match? asmL R (term (r₀ 2)))
(redex-match? asmL any (term (r₀ 2)))
(redex-match? asmL any (term (r₀ 2 r)))
(redex-match? asmL b (term true))
(redex-match? asmL R (term (r₀ n₀)))




(module+ test
  (require redex-chk)
  (redex-chk
   #:lang asmL
   #:m r r₀
   )
  )


(define-language L
  (e ::= ( λ (x) e) (e e) x)
  (v ::= ( λ (x) e))
  (C ::= ( λ (x) C) (C e) (e C) hole)
  (V ::= (λ (x) C))
  (x ::= variable-not-otherwise-mentioned))
  
;;(redex-match? L e (term (λ (x) e)))
;;(redex-match? L x (term x))
;;(redex-match? L e (term (λ (x) 4)))
;;(redex-match? L v (term (λ (x) e)))
;;(redex-match? L C (term (λ (x) e)))
;;(redex-match? L C (term (λ (x) C)))
;;(redex-match? L C (term (C e)))
;;(redex-match? L any (term (C e)))








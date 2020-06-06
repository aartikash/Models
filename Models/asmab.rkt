#lang racket/base

(require
  redex/reduction-semantics
  (only-in racket/contract/base integer-in))

;;Defining ranges for integers..hmm
(define largest-int32 (sub1 (expt 2 31)))
(define smallest-int32 (* -1 (expt 2 31)))
(define int32? (integer-in smallest-int32 largest-int32))

(define-language isaL
  ;;registers - I think the number matters
  ;;if they didn't then why even include them in the language?
  (r ::= (variable-prefix r))
  ;;memory - I think the number matters here alo
  (m ::= (variable-prefix m))
  ;;Labels - here it doesn't
  (l ::= (variable-prefix l))
  ;;machine integers - why can't 
  (int ::= (side-condition integer_0 (int32? (term integer_0))))
  ;;word values
  (w ::= l int)
  ;;values on which instructions operate
  (v ::= r w)
  ;;Heap values
  (h ::= (w ...) (code () I))
  ;;Heaps
  (H ::= l_0 h)
  ;;Register Files
  (R ::= r_0 w)
  ;;instructions
  (i ::= (add r_d r_s v)
        (bnz r v)
        (ld r_d (r_s int)))
  ;;instruction sequence
  (I ::=  (i I)
         (jmp v)
          halt)
  ;;Program
  (P ::= (H R I))
        
  )

                    
  
  
  
  
  
  
         

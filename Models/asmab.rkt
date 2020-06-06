#lang racket/base

(require
  redex
  racket/dict
  (only-in racket/contract/base integer-in)
   (only-in racket/list check-duplicates))

;;Defining ranges for integers..hmm
(define largest-int32 (sub1 (expt 2 31)))
(define smallest-int32 (* -1 (expt 2 31)))
(define int32? (integer-in smallest-int32 largest-int32))

(define-language asmL
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
  (H ::= (side-condition ((l_0 h) ...)
                         (not (check-duplicates (term (l_0 ...))))))
  ;;Register Files
 (R ::= (side-condition ((r_0 w) ...)
                         (not (check-duplicates (term (r_0 ...))))))
  ;;instructions (only looking at add for now)
  (i ::= (add r_d r_s v)
         (mov r_d v)
        (bnz r v)
        (ld r_d (r_s int)))
  ;;instruction sequence
  (I ::=  (i I)
         (jmp v)
          halt)
  ;;Program
  (P ::= (H R I))
        
  )

;;Tests
(redex-match? asmL r (term ( r_0)))
(redex-match? asmL r (term r_0))
(redex-match? asmL R (term (r_0 5)))
(redex-match? asmL v (term (5)))
(redex-match? asmL v 5)



;;Redex-chk for better usability
(module+  test
  (require redex-chk)
;;Check again
(redex-chk
 #:lang asmL
 #:m r r0
 #:m v 5)
)
 


;;Defining metafuctions
;;Handling overflows - not much imp I think for the moment.
(define (int32-overflow x)
  (cond
    [(< x smallest-int32)
     (int32-overflow (- largest-int32 (- smallest-int32 x 1)))]
    [(> x largest-int32)
     (int32-overflow (+ smallest-int32 (- x largest-int32 1)))]
    [else x]))
;;currently since only modeling one instruction
(define-metafunction asmL
  int32-add : int int -> int
  [(int32-add int_0 int_1)
   ,(int32-overflow (+ (term int_0) (term int_1)))])
;;update the dictionary with key and value
(define-metafunction asmL
  [(update any_dict any_decl)
   ,(dict-set (term any_dict) (car (term any_decl)) (cdr (term any_decl)))])
;;Picking up the values from dictionary
(define-metafunction asmL
  [(ref any_dict any_key)
   ,(cond
      [(dict-ref (term any_dict) (term any_key) (lambda _ #f)) =>
       (lambda (x) (cons (term any_key) x))]
      [else #f])])

(define-metafunction asmL
  [(build any_dict (any_decl ...))
   ,(for/fold ([dict (term any_dict)])
              ([d (term (any_decl ...))])
      (term (update ,dict ,d)))])

;;ref-val, mapping the register values from the register file
(define-metafunction asmL
  ref-val : R v -> w
  [(ref-val R w) w]
  [(ref-val R r) w
   (where (_ w) (ref R r))])


;;Reduction-relations
(define ->
  (reduction-relation
   asmL
   ;;Updates the register file by replacing r_d with the final output
   (--> (H R ((add r_d r_s v) I))
        (H (update R (r_d (int32-add (ref-val R r_s) (ref-val R v))) I)) "add")
   (--> (H R ((mov r_d v) I))
        (H (update R (r_d (ref-val R v))) I) "mov")
   ))

;;Testing reductions
;;nonsense test
(apply-reduction-relation -> (term (+ 3 4)))
(apply-reduction-relation -> (term (add 3 4 4)))
(apply-reduction-relation -> (term (add 3 4 4)))


(define (and-print e)
  (displayln e)
  e)

;;Eval
(define-metafunction asmL
  eval : r H I -> v
  [(eval r H I)
   (ref-val R_o r)
   (where (H_o R_o halt) ,(car (apply-reduction-relation* -> (term (H () I)) #:cache-all? #t)))])

(define-metafunction asmL
  build-I : i ... any -> I
  [(build-I i)
   (i halt)]
  [(build-I I)
   I]
  [(build-I i_0 i ... any)
   (i_0 (build-I i ... any))])

(define-metafunction asmL
  code* : (any ...) i ... any -> (code () I)
  [(code* any i ... any_0)
   (code any (build-I i ... any_0))])


(module+ test
  (require redex-chk)

  
   (int32-overflow 1) 1
   (int32-overflow (add1 largest-int32)) smallest-int32
   (int32-overflow (sub1 smallest-int32)) largest-int32

  (redex-chk
   #:lang asmL
   #:m r r0
   #:m v 5
   #:m w 5
   #:m R ((r0 5))
   #:m R ((rx 0) (rpc 0) (r0 5))
   #:m H ((l1 (code () ((add rx rx 1) halt))))
   #:m H ((l1 (code () ((add rx r0 1) halt))))
   #:m H ((l1 (code () ((mov rx 5) halt))))

   ;;not running two instructions at a time rn - error
   #:m H ((l1 (code () ((mov rx 5) (add rx r0 1) halt))))
   #:m H ((l1 (code () ((mov rx 5) (mov ry 5) halt))))
   #:m H ((l1 (code () ((add rx r0 1)  (add rx r0 1) halt))))


   
   
   ))















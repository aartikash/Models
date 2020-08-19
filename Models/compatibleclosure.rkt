#lang racket
(require redex)

(define-language grammar
  (B t
     f
     (B * B)))

(define r
  (reduction-relation
   grammar
   (--> (f * B_1) (B_1) "false")
   (--> (t * B_1) (t) "true")

   ))

(define ->r (compatible-closure r grammar B))


;;traces
(traces ->r '((f * f) * (t * f)))


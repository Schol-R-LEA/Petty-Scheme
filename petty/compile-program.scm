(define-module (petty compile-program)
  #:export (compile-program))

(use-modules (ice-9 format)     ; string formatting
             (srfi srfi-1)      ; enhanced lists
             (srfi srfi-13)     ; enhanced strings
             (srfi srfi-60)     ; bitwise operations on integers
             (petty emit)
             (petty env)
             (petty eval))


(define (compile-program expr)
  (emit-eval expr global-env))


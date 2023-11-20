(define-module (petty compile-program)
  #:export (compile-program))

(use-modules (petty reconciliations)
             (petty system)
             (petty emit)
             (petty env)
             (petty eval))


(define (compile-program exprs)
  (emit-preamble)
  (let loop ((current-clause (car exprs))
             (remaining-clauses (cdr exprs)))
    (emit-eval expr global-env)
    (if (not (null? remaining-clauses))
        (loop (car remaining-clauses) (cdr remaining-clauses))
        (begin
          (emit-lambda-epilogue global-env)
          (emit-program-exit))))
  (emit-epilogue))


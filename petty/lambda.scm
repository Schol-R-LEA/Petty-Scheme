(define-module (petty lambda)
  #:export (emit-lambda emit-lambda-preamble emit-lambda-epilogue))


(use-modules (ice-9 format)     ; string formatting
             (srfi srfi-1)      ; enhanced lists
             (srfi srfi-13)     ; enhanced strings
             (srfi srfi-60)     ; bitwise operations on integers
             (petty emit)
             (petty env)
             (petty eval)
             (petty system))



(define (emit-lambda-preamble env)
  (emit-push frame-pointer)
  (emit-move stack-pointer frame-pointer))

(define (emit-lambda-epilogue env)
  (emit-move frame-pointer stack-pointer)
  (emit-pop frame-pointer))


(define (emit-lambda name params body env)
  (let ((lambda-start (gen-label (if (null? name)
                                     "lambda"
                                     (symbol->string name))))
        (lambda-end (gen-label "lambda_bypass"))
        (current-env (new-env env)))
   (emit-jump lambda-end)
   (emit-label lambda-start)
   (emit-lambda-preamble env)
   (let gather-params ((next-param (car params))
                       (remaining-params (cdr params))
                       (count 0))
        (if (>= count (length passing-seq))
            (bind next-param current-env))
        (if (not (null? remaining-params))
            (gather-params (car remaining-params)
                           (cdr remaining-params)
                           (+ 1 count))))
   (let eval-clauses ((next-clause ((car body)))
                      (remaining (cdr body)))
     (emit-eval next-clause current-env)
     (if (not (null? remaining))
         (eval-clauses (car remaining) (cdr remaining))))
   (emit-lambda-epilogue env)
   (emit-label lambda-end)
   lambda-start))
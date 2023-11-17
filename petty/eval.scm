(define-module (petty eval)
  #:export (emit-eval))

(use-modules (ice-9 format)     ; string formatting
             (srfi srfi-1)      ; enhanced lists
             (srfi srfi-13)     ; enhanced strings
             (srfi srfi-60)     ; bitwise operations on integers
             (petty emit)
             (petty env)
             (petty types)
             (petty system)
             (petty primitives))


(define (emit-eval expr env)
  (cond ((immediate? expr)
         (emit-load-imm (immediate-rep expr) acc))
        ((list? expr)
         (let ((arity (- (length expr) 1))
               (fn (car expr)))
            (cond
                ((equal? 'quote fn) (emit "nop"))
                ((equal? 'lambda fn) (emit "nop"))
                ((equal? 'if fn) (emit "nop"))
                ((equal? 'cond fn) (emit "nop"))
                ((equal? 'define fn)
                (emit-bind-local (cdr expr) env))
                (else
                (cond
                    ((> 0 arity) (emit-error "arity mismatch"))
                    ((zero? arity)
                    (if (primitive? fn)
                        (apply-prim fn '() env)
                        (emit-apply fn '() env)))
                    (else
                    (let* ((arg-list (cdr expr))
                            (evaluated (evlist arg-list env)))
                        (if (primitive? fn)
                            (apply-prim fn evaluated env)
                            (emit-apply fn evaluated env)))))))))
        ((symbol? expr)
        (emit-load-local (lookup expr env) acc))
        (else (emit-error "cannot evaluate"))))


(define (apply-prim fn args env)
(let ((op (hash-ref primitives fn)))
  (if op
      (case (prim-arity op)
        ((0) ((prim-proc op)))
        ((1) ((prim-proc op) (list-ref passing-seq 0)))
        ((2)
             ((prim-proc op) (list-ref passing-seq 0)
                             (list-ref passing-seq 1)))
        (else (emit-error "invalid arity in primitive")))
      (emit-error "primitive not found"))))


(define (emit-apply fn params env)
(emit-error "procedure application not supported yet"))


(define (evlist args env)
  (let ((list-end (length args))
        (reg-top (- (length passing-seq) 1)))
    (let iter ((current-param 0))
        (if (< current-param list-end)
            (let ((current-arg (list-ref args current-param))
                (current-reg (list-ref passing-seq (min reg-top current-param))))
            (emit-eval current-arg env)
            (if (> current-param reg-top)
                (emit-push current-arg)
                (emit-move acc current-reg))
            (iter (+ 1 current-param)))))))
(define-module (petty compile-program)
  #:export (compile-program))

(use-modules (ice-9 format)     ; string formatting
             (srfi srfi-1)      ; enhanced lists
             (srfi srfi-13)     ; enhanced strings
             (srfi srfi-60)     ; bitwise operations on integers
             (petty emit)
             (petty primitives))




(define (gen-label label-base)
  (gensym label-base))







(define passing-seq '(rdi rsi rdx rcx r8 r9))

(define (compile-program in-path out-path filename)
  )




          (define (emit-primitive-procedures)
            #f)

          (define (emit-move src dest)
            (emit "movq %~a, %~a" src dest))

          (define (emit-move-imm imm dest)
            (emit "movq $~d, %~a" imm dest))

          (define (emit-preamble)
            (format out ".text~%")
            (format out ".p2align 8,,15~%")
            (format out ".globl scheme_entry~%")
            (format out ".type scheme_entry, @function~%~%")
            (emit-primitive-procedures)
            (format out "~%scheme_entry:~%")
            (emit-lambda-preamble global-env))

          (define (emit-epilogue)
            (format out "~%~%.data~%"))

          (define (emit-predicate-result comparison)
            (let ((cmp (hash-ref comparisons comparison)))
              (emit-move-imm 0 acc)
              (emit "set~a %al" cmp)
              (emit-tagged 'bool acc)))

          (define (emit-comparison-predicate value-0 value-1 comparison)
            (emit "cmpq %~a, %~a" value-0 value-1)
            (emit-predicate-result comparison))

          (define (emit-type-predicate type reg)
            (let ((mask (find-mask (hash-ref tags type)))
                  (tag (find-tag (hash-ref tags type))))
              (emit-move-imm tag acc)
              (emit "andq $~d, %~a" mask reg)
              (emit-comparison-predicate acc reg 'equal)))

          (define (emit-freed type reg)
            (let ((shift (find-shift (hash-ref tags type))))
              (emit "shrq $~d, %~a" shift reg)))

          (define (emit-tagged type reg)
              (let ((shift (find-shift (hash-ref tags type)))
                    (tag (find-tag (hash-ref tags type))))
                (emit "salq $~d, %~a" shift reg)
                (emit "orq $~d, %~a" tag reg)))

          (define (emit-arith op factor-0 factor-1)
            (emit "~aq %~a, %~a" op factor-0 factor-1))

          (define (emit-arith-imm op factor-0 factor-1)
            (emit "~aq $~d, %~a" op factor-0 factor-1))

          (define (emit-arith-operation op factor-0 factor-1)
            (emit-freed 'sys-int factor-0)
            (emit-freed 'sys-int factor-1)
            (emit-move factor-0 acc)
            (emit-arith op factor-1 acc)
            (emit-tagged 'sys-int acc))

          (define (evlist args env)
            (let ((list-end (length args))
                  (reg-top (- (length passing-seq) 1)))
              (let iter ((current-param 0))
                (if (< current-param list-end)
                    (let ((current-arg (list-ref args current-param))
                          (current-reg (list-ref passing-seq (min reg-top current-param))))
                      (emit-eval current-arg env)
                      (if (> current-param reg-top)
                          (emit "pushq ~a" current-arg)
                          (emit-move acc current-reg))
                      (iter (+ 1 current-param)))))))


          (define (emit-jump label)
            (emit "jmp ~a" label))

          (define (emit-push reg)
            (emit "pushq %~a" reg))

          (define (emit-push-mem address)
            (emit "pushq $~d" address))

          (define (emit-pop reg)
            (emit "popq %~a" reg))

          (define (emit-pop-mem address)
            (emit "popq $~d" address))

          (define (emit-load-local offset dest)
            (if offset
                (emit "movq -~a(%rbp), %~a" offset dest)
                (emit-error "emit-load-local: Symbol lookup failed")))

          (define (emit-store-local src offset)
            (if offset
                (emit "movq %~a, -~a(%rbp)" src offset)
                (emit-error "emit-store-local: Symbol lookup failed")))

          (define (emit-load-arg offset dest)
            (if offset
                (emit "movq ~a(%rbp), %~a" offset dest)
                (emit-error "emit-load-arg: Symbol lookup failed")))

          (define (emit-store-arg src offset)
            (if offset
                (emit "movq %~a, ~a(%rbp)" src offset)
                (emit-error "emit-store-arg: Symbol lookup failed")))

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
                      )
                  (if (not (null? remaining))
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


          (define (emit-load-address label reg)
            (emit "lea ~a, %~a" label reg))


          (define (emit-bind-local expr env)
            (let ((name (car expr))
                  (value (cadr expr)))
              (cond
                ((list? name)
                 (let ((offset (bind (car name) env))
                       (label (emit-lambda (car name) (cdr name) value env)))
                  (emit-load-address label acc)
                  (emit-store-local acc offset)))
                ((symbol? name)
                 (let ((offset (bind name env)))
                   (if offset
                       (begin
                         (emit-eval value env)
                         (emit-store-local acc offset))
                       (emit-error "symbol not found"))))
                (else (emit-error "Dynamic symbols not supported")))))


          (define (emit-eval expr env)
            (cond ((immediate? expr)
                   (emit-move-imm (immediate-rep expr) acc))
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



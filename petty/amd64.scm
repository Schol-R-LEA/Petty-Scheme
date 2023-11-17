(define-module (petty amd64)
  #:export (amd64-word-size amd64-passing-seq
            amd64-acc amd64-stack-pointer amd64-frame-pointer
            amd64-emit-preamble amd64-emit-epilogue amd64-emit-program-exit
            amd64-emit-return
            amd64-emit-move
            amd64-emit-load amd64-emit-load-imm amd64-emit-load-address
            amd64-emit-store amd64-emit-store-imm
            amd64-emit-arith amd64-emit-arith-imm amd64-emit-arith-operation
            amd64-emit-jump amd64-emit-conditional
            amd64-emit-comparison
            amd64-emit-push amd64-emit-push-mem amd64-emit-pop amd64-emit-pop-mem
            amd64-emit-and amd64-emit-and-imm amd64-emit-or amd64-emit-or-imm
            amd64-emit-xor amd64-emit-xor-imm
            amd64-emit-not
            amd64-emit-shift-left amd64-emit-shift-right
            amd64-emit-load-local amd64-emit-store-local
            amd64-emit-load-arg amd64-emit-store-arg
            amd64-emit-predicate-result amd64-emit-comparison-predicate
            amd64-emit-bind-local))


(use-modules (ice-9 format)     ; string formatting
             (srfi srfi-1)      ; enhanced lists
             (srfi srfi-13)     ; enhanced strings
             (srfi srfi-60)     ; bitwise operations on integers
             (petty emit)
             (petty env)
             (petty eval)
             (petty types)
             (petty lambda)
             (petty comparisons))


(define amd64-word-size 8)

(define amd64-acc "rax")
(define amd64-stack-pointer "rsp")
(define amd64-frame-pointer "rbp")

(define amd64-passing-seq '(rdi rsi rdx rcx r8 r9))

(define (amd64-emit-preamble)
  (format out ".text~%")
  (format out ".p2align 8,,15~%")
  (format out ".globl scheme_entry~%")
  (format out ".type scheme_entry, @function~%~%")
  (amd64-emit-primitive-procedures)
  (format out "~%scheme_entry:~%")
  (emit-lambda-preamble global-env))


(define (amd64-emit-program-exit)
  (emit "ret"))


(define (amd64-emit-epilogue)
  (format out "~%~%.data~%"))

(define (amd64-emit-return)
  (emit "ret"))

(define (amd64-emit-primitive-procedures)
  #f)

(define (amd64-emit-move src dest)
  (emit "movq %~a, %~a" src dest))

(define (amd64-emit-load-imm imm dest)
  (emit "movq $~d, %~a" imm dest))


(define (emit-load-address label reg)
  (emit "lea ~a, %~a" label reg))


(define (amd64-emit-arith op factor-0 factor-1)
  (emit "~aq %~a, %~a" op factor-0 factor-1))

(define (amd64-emit-arith-imm op factor-0 factor-1)
  (emit "~aq $~d, %~a" op factor-0 factor-1))


(define (amd64-emit-arith-operation op factor-0 factor-1)
  (emit-freed 'sys-int factor-0)
  (emit-freed 'sys-int factor-1)
  (amd64-emit-move factor-0 amd64-acc)
  (amd64-emit-arith op factor-1 amd64-acc)
  (emit-tagged 'sys-int amd64-acc))


(define (amd64-emit-jump label)
  (emit "jmp ~a" label))

(define (amd64-emit-conditional condition label)
  (emit "j~a ~a" condition label))

(define (amd64-emit-push reg)
  (emit "pushq %~a" reg))

(define (amd64-emit-push-mem address)
  (emit "pushq $~d" address))

(define (amd64-emit-pop reg)
  (emit "popq %~a" reg))

(define (amd64-emit-pop-mem address)
  (emit "popq $~d" address))

(define (amd64-emit-and reg-0 reg-1)
  (emit "andq %~a, %~a" reg-0 reg-1))

(define (amd64-emit-and-imm imm reg)
  (emit "andq $~d, %~a" imm reg))


(define (amd64-emit-or reg-0 reg-1)
  (emit "orq %~a, %~a" reg-0 reg-1))

(define (amd64-emit-or-imm imm reg)
  (emit "orq $~d, %~a" imm reg))

(define (amd64-emit-xor reg-0 reg-1)
  (emit "xorq %~a, %~a" reg-0 reg-1))

(define (amd64-emit-xor-imm imm reg)
  (emit "xorq $~d, %~a" imm reg))

(define (amd64-emit-not reg)
  (emit "notq %~a" reg)

(define (amd64-emit-shift-left shift reg)
  (emit "shlq $~d, %~a" shift reg))

(define (amd64-emit-shift-right shift reg)
  (emit "shrq $~d, %~a" shift reg))


(define (amd64-emit-load-local offset dest)
  (if offset
      (emit "movq -~a(%rbp), %~a" offset dest)
      (emit-error "emit-load-local: Symbol lookup failed")))

(define (amd64-emit-store-local src offset)
  (if offset
      (emit "movq %~a, -~a(%rbp)" src offset)
      (emit-error "emit-store-local: Symbol lookup failed")))

(define (amd64-emit-load-arg offset dest)
  (if offset
      (emit "movq ~a(%rbp), %~a" offset dest)
      (emit-error "emit-load-arg: Symbol lookup failed")))

(define (amd64-emit-store-arg src offset)
  (if offset
      (emit "movq %~a, ~a(%rbp)" src offset)
      (emit-error "emit-store-arg: Symbol lookup failed")))


(define (amd64-emit-predicate-result comparison)
  (let ((cmp (hash-ref comparisons comparison)))
    (amd64-emit-load-imm 0 amd64-acc)
    (emit "set~a %al" cmp)
    (emit-tagged 'bool amd64-acc)))

(define (amd64-emit-comparison-predicate value-0 value-1 comparison)
  (amd64-emit-comparison value-0 value-1)
  (amd64-emit-predicate-result comparison))


(define (amd64-emit-bind-local expr env)
  (let ((name (car expr))
        (value (cadr expr)))
    (cond
      ((list? name)
       (let ((offset (bind (car name) env))
             (label (emit-lambda (car name) (cdr name) value env)))
        (emit-load-address label amd64-acc)
        (amd64-emit-store-local amd64-acc offset)))
      ((symbol? name)
       (let ((offset (bind name env)))
         (if offset
             (begin
               (emit-eval value env)
               (amd64-emit-store-local amd64-acc offset))
             (emit-error "symbol not found"))))
      (else (emit-error "Dynamic symbols not supported")))))
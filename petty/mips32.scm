(define-module (petty mips32)
  #:export
    (mips32-word-size mips32-passing-seq
    mips32-acc mips32-stack-pointer mips32-frame-pointer
    mips32-emit-preamble mips32-emit-epilogue
    mips32-emit-program-exit
    mips32-emit-return
    mips32-emit-move
    mips32-emit-load mips32-emit-load-imm mips32-emit-load-address
    mips32-emit-store
    mips32-emit-arith mips32-emit-arith-imm mips32-emit-arith-operation
    mips32-emit-jump mips32-emit-conditional
    mips32-emit-push mips32-emit-push-mem mips32-emit-pop mips32-emit-pop-mem
    mips32-emit-and mips32-emit-and-imm
    mips32-emit-or mips32-emit-or-imm
    mips32-emit-xor mips32-emit-xor-imm
    mips32-emit-not
    mips32-emit-shift-left mips32-emit-shift-right
    mips32-emit-load-local mips32-emit-store-local
    mips32-emit-load-arg mips32-emit-store-arg
    mips32-emit-predicate-result mips32-emit-comparison-predicate))

(use-modules (petty reconciliations)
             (petty system)
             (ice-9 format)     ; string formatting
             (srfi srfi-1)      ; enhanced lists
             (srfi srfi-13)     ; enhanced strings
             (srfi srfi-60)     ; bitwise operations on integers
             (petty emit)
             (petty env)
             (petty eval)
             (petty types)
             (petty lambda)
             (petty comparisons))


(define mips32-word-size 4)

(define mips32-acc "v0")
(define mips32-stack-pointer "sp")
(define mips32-frame-pointer "fp")

(define mips32-passing-seq '(a0 a1 a2 a3))

(define (mips32-emit-preamble)
  (format out ".text~%")
  (format out ".p2align 8,,15~%")
  (format out ".globl scheme_entry~%")
  (format out ".type scheme_entry, @function~%~%")
  (mips32-emit-primitive-procedures)
  (format out "~%scheme_entry:~%")
  (emit-lambda-preamble global-env))


(define (mips32-emit-program-exit)
  (emit "jr $ra"))


(define (mips32-emit-epilogue)
  (format out "~%~%.data~%"))

(define (mips32-emit-return)
  (emit "jr $ra"))

(define (mips32-emit-primitive-procedures)
  #f)

(define (mips32-emit-move src dest)
  (emit "move %~a, %~a" src dest))

(define (mips32-emit-load mem dest)
  (emit "lw $~a, 0($~a)" dest src))

(define (mips32-emit-load-imm imm dest)
  (emit "li $~a, ~d" dest imm))


(define (mips32-emit-store src mem)
  (emit "sw $~a, 0($~a)" src mem))


(define (mips32-emit-load-address label reg)
  (emit "lea ~a, %~a" label reg))


(define (mips32-emit-arith op factor-0 factor-1)
  (emit "~aq %~a, %~a" op factor-0 factor-1))

(define (mips32-emit-arith-imm op factor-0 factor-1)
  (emit "~aq $~d, %~a" op factor-0 factor-1))


(define (mips32-emit-arith-operation op factor-0 factor-1)
  (emit-freed 'sys-int factor-0)
  (emit-freed 'sys-int factor-1)
  (mips32-emit-move factor-0 mips32-acc)
  (mips32-emit-arith op factor-1 mips32-acc)
  (emit-tagged 'sys-int mips32-acc))


(define (mips32-emit-jump label)
  (emit "jmp ~a" label))

(define (mips32-emit-conditional condition label)
  (emit "j~a ~a" condition label))

(define (mips32-emit-push reg)
  (emit "pushq %~a" reg))

(define (mips32-emit-push-mem address)
  (emit "pushq $~d" address))

(define (mips32-emit-pop reg)
  (emit "popq %~a" reg))

(define (mips32-emit-pop-mem address)
  (emit "popq $~d" address))

(define (mips32-emit-and reg-0 reg-1)
  (emit "andq %~a, %~a" reg-0 reg-1))

(define (mips32-emit-and-imm imm reg)
  (emit "andq $~d, %~a" imm reg))


(define (mips32-emit-or reg-0 reg-1)
  (emit "orq %~a, %~a" reg-0 reg-1))

(define (mips32-emit-or-imm imm reg)
  (emit "orq $~d, %~a" imm reg))

(define (mips32-emit-xor reg-0 reg-1)
  (emit "xorq %~a, %~a" reg-0 reg-1))

(define (mips32-emit-xor-imm imm reg)
  (emit "xorq $~d, %~a" imm reg))

(define (mips32-emit-not reg)
  (emit "notq %~a" reg))

(define (mips32-emit-shift-left shift reg)
  (emit "shlq $~d, %~a" shift reg))

(define (mips32-emit-shift-right shift reg)
  (emit "shrq $~d, %~a" shift reg))


(define (mips32-emit-load-local offset dest)
  (if offset
      (emit "movq -~a(%rbp), %~a" offset dest)
      (emit-error "emit-load-local: Symbol lookup failed")))

(define (mips32-emit-store-local src offset)
  (if offset
      (emit "movq %~a, -~a(%rbp)" src offset)
      (emit-error "emit-store-local: Symbol lookup failed")))

(define (mips32-emit-load-arg offset dest)
  (if offset
      (emit "movq ~a(%rbp), %~a" offset dest)
      (emit-error "emit-load-arg: Symbol lookup failed")))

(define (mips32-emit-store-arg src offset)
  (if offset
      (emit "movq %~a, ~a(%rbp)" src offset)
      (emit-error "emit-store-arg: Symbol lookup failed")))


(define (mips32-emit-predicate-result comparison)
  (let ((cmp (hash-ref comparisons comparison)))
    (mips32-emit-load-imm 0 mips32-acc)
    (emit "set~a %al" cmp)
    (emit-tagged 'bool mips32-acc)))


(define (mips32-emit-comparison value-0 value-1)
    (emit "cmpq %~a, %~a" value-0 value-1))


(define (mips32-emit-comparison-predicate value-0 value-1 comparison)
  (mips32-emit-comparison value-0 value-1)
  (mips32-emit-predicate-result comparison))



(define (mips32-emit-bind-local expr env)
  (let ((name (car expr))
        (value (cadr expr)))
    (cond
      ((list? name)
       (let ((offset (bind (car name) env))
             (label (emit-lambda (car name) (cdr name) value env)))
        (mips32-emit-load-address label mips32-acc)
        (mips32-emit-store-local mips32-acc offset)))
      ((symbol? name)
       (let ((offset (bind name env)))
         (if offset
             (begin
               (emit-eval value env)
               (mips32-emit-store-local mips32-acc offset))
             (emit-error "symbol not found"))))
      (else (emit-error "Dynamic symbols not supported")))))

(use-modules (ice-9 format)
             (srfi srfi-1)
             (srfi srfi-13)
             (srfi srfi-60))

(define word-size 8)

;; an environment consists of a hash table containing
;; symbols and stack offsets for the variables the symbols
;; reference. Each environment save the global one
;; also has a special symbol, env-parent-marker, which
;; holds a pointer to its parent environment.
(define global-env (make-hash-table))

(define env-parent-marker '_%%$_PARENT)


(define (get-env-size env)
  (hash-count (lambda (entry value) (not (hash-table? value))) env))


(define (bind sym env)
  (hash-set! env sym (* word-size (+ 1 (get-env-size env))))
  (hash-ref env sym))   ; return the assigned offset


(define (new-env parent)
  (let ((child ((make-hash-table))))
    (hash-set! child env-parent-marker parent)
    child))


(define (lookup sym env)
  (let ((offset (hash-ref env sym)))
    (if offset
        offset
        (let ((parent (hash-ref env env-parent-marker)))
          (if parent
              (+ (lookup sym parent) (get-env-size parent))
              #f)))))

(define acc "rax")
(define stack-pointer "rsp")
(define frame-pointer "rbp")

(define (gen-label label-base)
  (gensym label-base))

(define comparisons (make-hash-table))

(hash-set! comparisons 'equal "e")
(hash-set! comparisons 'not-equal "ne")
(hash-set! comparisons 'zero "z")
(hash-set! comparisons 'not-zero "nz")
(hash-set! comparisons 'greater "g")
(hash-set! comparisons 'not-greater "ng")
(hash-set! comparisons 'greater-equal "ge")
(hash-set! comparisons 'not-greater-equal "nge")
(hash-set! comparisons 'less "l")
(hash-set! comparisons 'not-less "nl")
(hash-set! comparisons 'less-equal "le")
(hash-set! comparisons 'not-less-equal "nle")
(hash-set! comparisons 'carry "c")
(hash-set! comparisons 'not-carry "nc")
(hash-set! comparisons 'overflow "o")
(hash-set! comparisons 'not-overflow "no")


(define tags (make-hash-table))

(hash-set! tags 'sys-int   '(2 #x03 #b00000000))
(hash-set! tags 'char      '(8 #xff #b00001111))
(hash-set! tags 'bool      '(7 #x7f #b00011111))
(hash-set! tags 'null-list '(8 #xff #b00101111))

(define find-shift car)
(define find-mask cadr)
(define find-tag caddr)

(define (get-shift x)
  (find-shift
    (hash-ref tags
      (cond
        ((null? x) 'null-list)
        ((integer? x) 'sys-int)
        ((char? x) 'char)
        ((boolean? x) 'bool)))))

(define (get-mask x)
  (find-mask
    (hash-ref tags
      (cond
        ((null? x) 'null-list)
        ((integer? x) 'sys-int)
        ((char? x) 'char)
        ((boolean? x) 'bool)))))


(define (get-tag x)
  (find-tag
    (hash-ref tags
      (cond
        ((null? x) 'null-list)
        ((integer? x) 'sys-int)
        ((char? x) 'char)
        ((boolean? x) 'bool)))))


(define (any->integer x)
  (cond
    ((null? x)    0)
    ((integer? x) x)
    ((char? x)    (char->integer x))
    ((boolean? x) (if x 1 0))))


(define (immediate-rep x)
  (let ((shift-amnt (get-shift x)))
    (logior
      (ash (any->integer x) shift-amnt)
      (get-tag x))))

(define (immediate? x)
  (or (null? x) (integer? x) (char? x) (boolean? x)))


(define primitives (make-hash-table))


(define (primitive? x)
  (or (hash-ref primitives x)))

(define (prim-arity fn)
  (car fn))

(define (prim-proc fn)
  (cdr fn))

(define passing-seq '(rdi rsi rdx rcx r8 r9))

(define (compile-program in-path out-path filename)
  (call-with-input-file (format #f "~a/~a.scm" in-path filename)
    (lambda (in)
      (call-with-output-file (format #f "~a/~a.s" out-path filename)
        (lambda (out)
          (define (emit fmt . args)
            (format out "~/")
            (apply format out fmt args)
            (format out "~%"))

          (define (emit-error descr)
            (format #t "error - ~a~%" descr)
            (format out ".error ~a~a~a~%" #\" descr #\"))

          (define (emit-label label)
            (format out "~a:~%" label))


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

          (define (emit-lambda params body env)
            (let ((lambda-start (gen-label "lambda"))
                  (lambda-end (gen-label "lambda_bypass")))
             (emit-jump lambda-end)
             (emit-label lambda-start)
             (emit-lambda-preamble env)
             ;(emit-error "lambda expressions not supported yet")
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
                       (label (emit-lambda (cdr name) value env)))
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

          (hash-set! primitives
            'inc
            (cons 1 (lambda (reg)
                       (emit-move reg acc)
                       (emit-arith-imm "add" (immediate-rep 1) acc))))

          (hash-set! primitives
            'dec
            (cons 1 (lambda (reg)
                      (emit-move reg acc)
                      (emit-arith-imm "sub" (immediate-rep 1) acc))))

          (hash-set! primitives
            'integer->char
            (cons 1 (lambda (reg)
                      (let ((shift (- (find-shift (hash-ref tags 'char))
                                      (find-shift (hash-ref tags 'sys-int))))
                            (tag (find-tag (hash-ref tags 'char))))
                      (emit-move reg acc)
                      (emit "shlq $~d, %rax" shift)
                      (emit "orq $~d, %rax" tag)))))

          (hash-set! primitives
            'char->integer
            (cons 1 (lambda (reg)
                      (let ((shift (- (find-shift (hash-ref tags 'char))
                                     (find-shift (hash-ref tags 'sys-int)))))
                        (emit-move reg acc)
                        (emit "shrq $~d, %rax" shift)))))

          (hash-set! primitives
            'not
            (cons 1 (lambda (reg)
                      (emit-move reg acc)
                      (emit-freed 'sys-int acc)
                      (emit "notq %rax")
                      (emit-tagged 'sys-int acc))))

          (hash-set! primitives
            'null?
            (cons 1 (lambda (reg)
                      (emit-type-predicate 'null-list reg))))

          (hash-set! primitives
            'zero?
            (cons 1 (lambda (reg)
                        (emit-move-imm 0 acc)
                        (emit-comparison-predicate acc reg 'equal))))

          (hash-set! primitives
            'integer?
            (cons 1 (lambda (reg)
                      (emit-type-predicate 'sys-int reg))))


          (hash-set! primitives
            'char?
            (cons 1 (lambda (reg)
                      (emit-type-predicate 'char reg))))


          (hash-set! primitives
            'boolean?
            (cons 1 (lambda (reg)
                      (emit-type-predicate 'bool reg))))

          (hash-set! primitives
            '+
            (cons 2 (lambda (augend addend)
                      (emit-arith-operation "add" augend addend))))

          (hash-set! primitives
            '-
            (cons 2 (lambda (minuend subtrahend)
                      (emit-arith-operation "sub" minuend subtrahend))))

          (hash-set! primitives
            '*
            (cons 2 (lambda (multiplier multiplicand)
                      (emit-arith-operation "imul" multiplier multiplicand))))

          (hash-set! primitives
            '/
            (cons 2 (lambda (dividend divisor)
                      (emit-move-imm 0 "rdx")
                      (emit-arith-operation "idiv" dividend divisor))))

          (hash-set! primitives
            'mod
            (cons 2 (lambda (dividend divisor)
                      (emit-move-imm 0 "rdx")
                      (emit-arith-operation "idiv" dividend divisor)
                      (emit-tagged 'sys-int "rdx")
                      (emit-move "rdx" acc))))

          (hash-set! primitives
            '=
            (cons 2 (lambda (value-0 value-1)
                      (emit-comparison-predicate value-0 value-1 'equal))))

          (hash-set! primitives
            '!=
            (cons 2 (lambda (value-0 value-1)
                      (emit-comparison-predicate value-0 value-1 'not-equal))))

          (hash-set! primitives
            '<
            (cons 2 (lambda (value-0 value-1)
                      (emit-comparison-predicate value-0 value-1 'less))))

          (hash-set! primitives
            '<=
            (cons 2 (lambda (value-0 value-1)
                      (emit-comparison-predicate value-0 value-1 'less-equal))))

          (hash-set! primitives
            '>
            (cons 2 (lambda (value-0 value-1)
                      (emit-comparison-predicate value-0 value-1 'greater))))

          (hash-set! primitives
            '>=
            (cons 2 (lambda (value-0 value-1)
                      (emit-comparison-predicate value-0 value-1 'greater-equal))))


          ;; body of the main compilation function
          (emit-preamble)
          (let loop ((current-clause (read in)))
            (if (not (eof-object? current-clause))
                (begin
                  (emit-eval current-clause global-env)
                  (loop (read in)))
                (begin
                  (emit-lambda-epilogue global-env)
                  (emit "ret"))))
          (emit-epilogue))))))


(define ipath (cadr (program-arguments)))
(define opath (caddr (program-arguments)))
(define source (cadddr (program-arguments)))

(compile-program ipath opath source)
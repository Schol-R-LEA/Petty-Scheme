(define-module (petty inline-primitives)
  #:export (inline-primitives primitive? prim-arity prim-proc))

(use-module (petty emit))

(define inline-primitives (make-hash-table))

(define (primitive? x)
  (hash-ref inline-primitives x))

(define (prim-arity fn)
(car fn))

(define (prim-proc fn)
(cdr fn))

  (hash-set! inline-primitives
    'inc
    (cons 1 (lambda (reg)
            (emit-move reg acc)
            (emit-arith-imm "add" (immediate-rep 1) acc))))

  (hash-set! inline-primitives
    'dec
    (cons 1 (lambda (reg)
            (emit-move reg acc)
            (emit-arith-imm "sub" (immediate-rep 1) acc))))

  (hash-set! inline-primitives
    'integer->char
    (cons 1 (lambda (reg)
            (let ((shift (- (find-shift (hash-ref tags 'char))
                            (find-shift (hash-ref tags 'sys-int))))
                    (tag (find-tag (hash-ref tags 'char))))
            (emit-move reg acc)
            (emit "shlq $~d, %rax" shift)
            (emit "orq $~d, %rax" tag)))))

  (hash-set! inline-primitives
    'char->integer
    (cons 1 (lambda (reg)
            (let ((shift (- (find-shift (hash-ref tags 'char))
                            (find-shift (hash-ref tags 'sys-int)))))
                (emit-move reg acc)
                (emit "shrq $~d, %rax" shift)))))

  (hash-set! inline-primitives
    'not
    (cons 1 (lambda (reg)
            (emit-move reg acc)
            (emit-freed 'sys-int acc)
            (emit "notq %rax")
            (emit-tagged 'sys-int acc))))

  (hash-set! inline-primitives
    'null?
    (cons 1 (lambda (reg)
            (emit-type-predicate 'null-list reg))))

  (hash-set! inline-primitives
    'zero?
    (cons 1 (lambda (reg)
                (emit-move-imm 0 acc)
                (emit-comparison-predicate acc reg 'equal))))

  (hash-set! inline-primitives
    'integer?
    (cons 1 (lambda (reg)
            (emit-type-predicate 'sys-int reg))))


  (hash-set! inline-primitives
    'char?
    (cons 1 (lambda (reg)
            (emit-type-predicate 'char reg))))


  (hash-set! inline-primitives
    'boolean?
    (cons 1 (lambda (reg)
            (emit-type-predicate 'bool reg))))

  (hash-set! inline-primitives
    '+
    (cons 2 (lambda (augend addend)
            (emit-arith-operation "add" augend addend))))

  (hash-set! inline-primitives
    '-
    (cons 2 (lambda (minuend subtrahend)
            (emit-arith-operation "sub" minuend subtrahend))))

  (hash-set! inline-primitives
    '*
    (cons 2 (lambda (multiplier multiplicand)
            (emit-arith-operation "imul" multiplier multiplicand))))

  (hash-set! inline-primitives
    '/
    (cons 2 (lambda (dividend divisor)
            (emit-move-imm 0 "rdx")
            (emit-arith-operation "idiv" dividend divisor))))

  (hash-set! inline-primitives
    'mod
    (cons 2 (lambda (dividend divisor)
            (emit-move-imm 0 "rdx")
            (emit-arith-operation "idiv" dividend divisor)
            (emit-tagged 'sys-int "rdx")
            (emit-move "rdx" acc))))

  (hash-set! inline-primitives
    '=
    (cons 2 (lambda (value-0 value-1)
            (emit-comparison-predicate value-0 value-1 'equal))))

  (hash-set! inline-primitives
    '!=
    (cons 2 (lambda (value-0 value-1)
            (emit-comparison-predicate value-0 value-1 'not-equal))))

  (hash-set! inline-primitives
    '<
    (cons 2 (lambda (value-0 value-1)
            (emit-comparison-predicate value-0 value-1 'less))))

  (hash-set! inline-primitives
    '<=
    (cons 2 (lambda (value-0 value-1)
            (emit-comparison-predicate value-0 value-1 'less-equal))))

  (hash-set! inline-primitives
    '>
    (cons 2 (lambda (value-0 value-1)
            (emit-comparison-predicate value-0 value-1 'greater))))

  (hash-set! inline-primitives
    '>=
    (cons 2 (lambda (value-0 value-1)
            (emit-comparison-predicate value-0 value-1 'greater-equal)))))
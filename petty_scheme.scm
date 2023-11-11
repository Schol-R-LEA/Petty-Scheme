(use-modules (ice-9 format)
             (srfi srfi-1)
             (srfi srfi-13)
             (srfi srfi-60))


(define (gen-label label-base)
  (gensym label-base))


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
          (define (emit-preamble)
            (format out ".text~%")
            (format out ".p2align 8,,15~%")
            (format out ".globl scheme_entry~%")
            (format out ".type scheme_entry, @function~%~%")
            (format out "scheme_entry:~%"))

          (define (evlist args env)
            (let ((list-end (length args))
                  (reg-top (- (length passing-seq) 1)))
              (let iter ((current-param 0))
                (if (< current-param list-end)
                    (let ((current-arg (list-ref args current-param))
                          (current-reg (list-ref passing-seq (min reg-top current-param))))
                      (emit-eval current-arg env)
                      (if (> current-param reg-top)
                          (emit "pushq ~a" current-arg))
                          (emit "movq %rax, %~a" current-reg))
                      (iter (+ 1 current-param))))))
          (define (emit-eval expr env)
            (cond ((immediate? expr)
                   (emit "movq $~a, %rax" (immediate-rep expr)))
                  ((symbol? expr)
                   (emit-error "symbols not supported yet"))
                  ((list? expr)
                   (let ((arity (- (length expr) 1))
                         (fn (car expr)))
                     (cond ((equal? 'quote fn)
                            (emit "nop"))
                           ((> 0 arity) (emit-error "arity mismatch"))
                            ((zero? arity)
                             (if (primitive? fn)
                                 (apply-prim fn '() env)
                                 (emit-apply fn '() env)))
                            (else
                              (let ((arg-list (cdr expr)))
                                (let ((evaluated (evlist arg-list env)))
                                  (if (primitive? fn)
                                      (apply-prim fn evaluated env)
                                      (emit-apply fn evaluated env))))))))
                  (else (emit-error "cannot evaluate"))))
          (define (apply-prim fn args env)
            (let ((op (hash-ref primitives fn)))
              (if op
                  (case (prim-arity op)
                    ((0) ((prim-proc op)))
                    ((1) ((prim-proc op) (list-ref passing-seq 0)))
                    ((2) ((prim-proc op) (list-ref passing-seq 0)
                                         (list-ref passing-seq 1)))
                    (else (emit-error "invalid arity in primitive")))
                  (emit-error "primitive not found"))))
          (define (emit-apply fn params env)
            (emit-error "procedure application not supported yet"))

          (emit-preamble)

          (hash-set! primitives
            'inc
            (cons 1 (lambda (reg)
                       (emit "movq %~a, %rax" reg)
                       (emit "addq $~d, %rax" (immediate-rep 1)))))

          (hash-set! primitives
            'dec
            (cons 1 (lambda (reg)
                      (emit "movq %~a, %rax" reg)
                      (emit "subq $~d, %rax" (immediate-rep 1)))))

          (hash-set! primitives
            'integer->char
            (cons 1 (lambda (reg)
                      (emit "movq %~a, %rax" reg)
                      (emit "shlq $~d, %rax" (- (find-shift (hash-ref tags 'char))
                                                (find-shift (hash-ref tags 'sys-int))))
                      (emit "orq $~d, %rax" (find-tag (hash-ref tags 'char))))))

          (hash-set! primitives
            'char->integer
            (cons 1 (lambda (reg)
                      (emit "movq %~a, %rax" reg)
                      (emit "shrq $~d, %rax" (- (find-shift (hash-ref tags 'char))
                                                (find-shift (hash-ref tags 'sys-int)))))))

          (hash-set! primitives
            'not
            (cons 1 (lambda (reg)
                      (let ((shift-amt (find-shift (hash-ref tags 'sys-int))))
                        (emit "movq %~a, %rax" reg)
                        (emit "shrq $~d, %rax" shift-amt)
                        (emit "notq %rax")
                        (emit "shlq $~d, %rax" shift-amt)))))

          (hash-set! primitives
            'null?
            (cons 1 (lambda (reg)
                      (let ((null-mask (find-mask (hash-ref tags 'null-list)))
                            (true-label (gen-label "null_true"))
                            (exit-label (gen-label "null_exit")))
                        (emit "movq %~a, %rax" reg)
                        (emit "andq $~d, %rax" null-mask)
                        (emit "cmpq $0, %rax")
                        (emit "jz ~a" true-label)
                        (emit "movq $~a, %rax" (immediate-rep #f))
                        (emit "jmp ~a" exit-label)
                        (emit-label true-label)
                        (emit "movq $~a, %rax" (immediate-rep #t))
                        (emit-label exit-label)))))

          (hash-set! primitives
            'zero?
            (cons 1 (lambda (reg)
                      (let ((bool-shift (find-shift (hash-ref tags 'bool)))
                            (bool-tag (find-tag (hash-ref tags 'bool))))
                        (emit "movq $0, %rax")
                        (emit "cmpq $0, %~a" reg)
                        (emit "sete %al")
                        (emit "salq $~d, %rax" bool-shift)
                        (emit "orq $~a, %rax" bool-tag)))))

          (hash-set! primitives
            'integer?
            (cons 1 (lambda (reg)
                      (let ((int-tag (find-tag (hash-ref tags 'sys-int)))
                            (int-shift (find-shift (hash-ref tags 'sys-int)))
                            (bool-tag (find-tag (hash-ref tags 'bool)))
                            (bool-shift (find-shift (hash-ref tags 'bool))))
                        (emit "pushq %rbx")
                        (emit "movq $0, %rax")
                        (emit "movq %~a, %rbx" reg)
                        (emit "sarq $~d, %rbx" int-shift)
                        (emit "salq $~d, %rbx" int-shift)
                        (emit "cmpq %~a, %rbx" reg)
                        (emit "sete %al")
                        (emit "salq $~d, %rax" bool-shift)
                        (emit "orq $~a, %rax" bool-tag)
                        (emit "popq %rbx")))))


          (hash-set! primitives
            'char?
            (cons 1 (lambda (reg)
                      (let ((char-mask (find-mask (hash-ref tags 'char)))
                            (char-tag (find-tag (hash-ref tags 'char)))
                            (bool-tag (find-tag (hash-ref tags 'bool)))
                            (bool-shift (find-shift (hash-ref tags 'bool))))
                        (emit "movq $0, %rax")
                        (emit "andq $~d, %~a" char-mask reg)
                        (emit "cmpq $~d, %~a" char-tag reg)
                        (emit "sete %al")
                        (emit "salq $~d, %rax" bool-shift)
                        (emit "orq $~a, %rax" bool-tag)))))


          (hash-set! primitives
            'boolean?
            (cons 1 (lambda (reg)
                      (let ((bool-mask (find-mask (hash-ref tags 'bool)))
                            (bool-tag (find-tag (hash-ref tags 'bool)))
                            (bool-shift (find-shift (hash-ref tags 'bool))))
                        (emit "movq $0, %rax")
                        (emit "andq $~d, %~a" bool-mask reg)
                        (emit "cmpq $~d, %~a" bool-tag reg)
                        (emit "sete %al")
                        (emit "salq $~d, %rax" bool-shift)
                        (emit "orq $~a, %rax" bool-tag)))))


          (let loop ((current-clause (read in)))
            (if (not (eof-object? current-clause))
                (begin
                  ;(format #t "~A~%" current-clause)
                  (emit-eval current-clause '())
                  (loop (read in)))
                (emit "ret"))))))))


(define ipath (cadr (program-arguments)))
(define opath (caddr (program-arguments)))
(define source (cadddr (program-arguments)))

(compile-program ipath opath source)
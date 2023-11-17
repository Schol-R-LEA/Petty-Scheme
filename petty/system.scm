(define-module (petty system)
  #:export (isa-type word-size passing-seq
  acc stack-pointer frame-pointer
  emit-preamble emit-epilogue
  emit-program-exit
  emit-return
  emit-move
  emit-load emit-load-imm emit-load-address
  emit-store emit-store-imm
  emit-arith emit-arith-imm emit-arith-operation
  emit-jump emit-conditional
  emit-push emit-push-mem emit-pop emit-pop-mem
  emit-and emit-and-imm emit-or emit-or-imm emit-xor emit-xor-imm
  emit-not
  emit-shift-left emit-shift-right
  emit-load-local emit-store-local
  emit-load-arg emit-store-arg
  emit-predicate-result emit-comparison-predicate))

(use-modules (petty amd64))


(define isa-type 'amd64)
(define os-type 'linux)

(define (bind-isa-detail name)
  (string->symbol
    (string-append (cond ((equal? isa-type 'amd64) "amd64-")
                         ((equal? isa-type 'mips32) "mips32-")
                         ((equal? isa-type 'mips64) "mips64-")
                         ((equal? isa-type 'riscv64) "riscv64-")
                         ((equal? isa-type 'aarch64) "aarch64-")
                         (else (error "Unknown or unsupported ISA")))
                    name)))

(define word-size (bind-isa-detail "word-size"))
(define passing-seq (bind-isa-detail "passing-seq"))

(define acc (bind-isa-detail "acc"))
(define stack-pointer (bind-isa-detail "stack-pointer"))
(define frame-pointer (bind-isa-detail "frame-pointer"))

(define emit-preamble (bind-isa-detail "emit-preamble"))
(define emit-epilogue (bind-isa-detail "emit-epilogue"))

(define emit-program-exit (bind-isa-detail "emit-program-exit"))
(define emit-return (bind-isa-detail "emit-return"))

(define emit-move (bind-isa-detail "emit-move"))
(define emit-load (bind-isa-detail "emit-load"))
(define emit-load-imm (bind-isa-detail "emit-load-imm"))
(define emit-load-address (bind-isa-detail "emit-load-address"))
(define emit-store (bind-isa-detail "emit-store"))
(define emit-store-imm (bind-isa-detail "emit-store-imm"))

(define emit-arith (bind-isa-detail "emit-arith"))
(define emit-arith-imm (bind-isa-detail "emit-arith-imm"))
(define emit-arith-operation (bind-isa-detail "emit-arith-operation"))

(define emit-jump (bind-isa-detail "emit-jump"))
(define emit-conditional (bind-isa-detail "emit-conditional"))

(define emit-push (bind-isa-detail "emit-push"))
(define emit-push-mem (bind-isa-detail "emit-push-mem"))
(define emit-pop (bind-isa-detail "emit-pop"))
(define emit-pop-mem (bind-isa-detail "emit-pop-mem"))

(define emit-and (bind-isa-detail "emit-and"))
(define emit-and-imm (bind-isa-detail "emit-and-imm"))
(define emit-or (bind-isa-detail "emit-or"))
(define emit-or-imm (bind-isa-detail "emit-or-imm"))
(define emit-xor (bind-isa-detail "emit-xor"))
(define emit-xor-imm (bind-isa-detail "emit-xor-imm"))

(define emit-not (bind-isa-detail "emit-not"))

(define emit-shift-left (bind-isa-detail "emit-shift-left"))
(define emit-shift-right (bind-isa-detail "emit-shift-right"))

(define emit-load-local (bind-isa-detail "emit-load-local"))
(define emit-store-local (bind-isa-detail "emit-store-local"))
(define emit-load-arg (bind-isa-detail "emit-load-arg"))
(define emit-store-arg (bind-isa-detail "emit-store-arg"))
(define emit-predicate-result (bind-isa-detail "emit-predicate-result"))
(define emit-comparison-predicate (bind-isa-detail "emit-comparison-predicate"))

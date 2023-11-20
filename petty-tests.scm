(add-to-load-path ".")

(use-modules )

; (bind-reconciliations isa-type
;                       acc stack-pointer frame-pointer
;                       emit-preamble emit-epilogue
;                       emit-program-exit
;                       emit-return
;                       emit-move
;                       emit-load emit-load-imm emit-load-address
;                       emit-store
;                       emit-arith emit-arith-imm emit-arith-operation
;                       emit-jump emit-conditional
;                       emit-push emit-push-mem emit-pop emit-pop-mem
;                       emit-and emit-and-imm emit-or emit-or-imm emit-xor emit-xor-imm
;                       emit-not
;                       emit-shift-left emit-shift-right
;                       emit-load-local emit-store-local
;                       emit-load-arg emit-store-arg
;                       emit-predicate-result emit-comparison-predicate)

(use-modules (ice-9 format)     ; string formatting
             (ice-9 rdelim)     ; line-oriented input
             (ice-9 popen)      ; pipes and shell commands
             (srfi srfi-1)      ; lists
             (srfi srfi-13)     ; enhanced strings
             (srfi srfi-60)     ; bitwise operations on integers
             (srfi srfi-64)     ; unit tests
             (petty reconciliations)
             (petty amd64)
             (petty system))

(set! isa-type (supported-isa amd64))
(set! os-type (supported-os linux-amd64))
(bind-reconciliations isa-type os-type)

(test-begin "loading")

(test-equal word-size amd64-word-size)

(test-end "loading")
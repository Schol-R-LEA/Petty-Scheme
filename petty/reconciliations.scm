(define-module (petty reconciliations)
#:export (isa-reconciliations os-reconciliations
          isa-type supported-isa isas
          os-type supported-os oses
          map-specific map-general))

(use-modules (rnrs enums))


(define-enumeration supported-isa (amd64 mips32 mips64 aarch64 riscv64) isas)
(define-enumeration supported-os (linux-amd64) oses)

(define isa-type (supported-isa amd64))
(define os-type (supported-os linux-amd64))


(define isa-reconciliations
        '(word-size passing-seq
          acc stack-pointer frame-pointer
          emit-preamble emit-epilogue
          emit-program-exit
          emit-return
          emit-move
          emit-load emit-load-imm emit-load-address
          emit-store
          emit-arith emit-arith-imm emit-arith-operation
          emit-jump emit-conditional
          emit-push emit-push-mem emit-pop emit-pop-mem
          emit-and emit-and-imm emit-or emit-or-imm emit-xor emit-xor-imm
          emit-not
          emit-shift-left emit-shift-right
          emit-load-local emit-store-local
          emit-load-arg emit-store-arg
          emit-predicate-result emit-comparison-predicate))

(define os-reconciliations '(emit-program-exit))


(define (append-specific patch item)
  (string->symbol
    (string-append
    (symbol->string patch)
    (string-append "-"
    (symbol->string item)))))


(define-syntax map-specific
  (syntax-rules ()
    ((_ patch reconciliations)
     (let* ((expanded-patch (make-list (length reconciliations) patch))
            (patched (map append-specific expanded-patch reconciliations)))
       (eval `(export ,@patched) (interaction-environment))))))



(define-syntax map-general
  (syntax-rules ()
    ((_ patch reconciliations)
      (begin
        `(export ,@reconciliations)
        (let* ((expanded-patch (make-list (length reconciliations) patch))
               (patched (map append-specific expanded-patch reconciliations)))
          (for-each (lambda (def val)
                      (eval `(define ,def ,val) (interaction-environment)))
                    reconciliations patched))))))

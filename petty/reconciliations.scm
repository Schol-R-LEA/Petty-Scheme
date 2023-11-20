(define-module (petty reconciliations)
#:export (isa-type supported-isa
          os-type supported-os))

(use-modules (rnrs enums))

(define-enumeration supported-isa (amd64 mips32 mips64 aarch64 riscv64) isas)
(define-enumeration supported-os (linux-amd64) oses)

(define isa-type (supported-isa amd64))
(define os-type (supported-os linux-amd64))

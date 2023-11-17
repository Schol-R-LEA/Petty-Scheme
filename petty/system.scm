(define-module (petty system))

(use-modules (petty reconciliations)
             (petty amd64)
             (rnrs enums))

(map-general isa-type isa-reconciliations)

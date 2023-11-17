(add-to-load-path ".")

(use-modules (petty reconciliations))


(set! isa-type (supported-isa amd64))
(set! os-type (supported-os linux-amd64))

(use-modules (ice-9 format)     ; string formatting
             (ice-9 rdelim)     ; line-oriented input
             (ice-9 popen)      ; pipes and shell commands
             (srfi srfi-1)      ; lists
             (srfi srfi-13)     ; enhanced strings
             (srfi srfi-60)     ; bitwise operations on integers
             (srfi srfi-64)    ; unit tests
             (petty compile-program)
             (petty system)
             (petty emit)
             (petty env)
             (petty eval))

(test-begin "loading")

(test-end "loading")
(define-module (petty emit)
  #:export (out emit emit-error emit-label))

(use-modules (ice-9 format)     ; string formatting
             (srfi srfi-1)      ; enhanced lists
             (srfi srfi-13)     ; enhanced strings
             (srfi srfi-60))     ; bitwise operations on integers

; stubs for the global output port
(define out '())

(define (emit fmt . args)
    (format out "~/")
    (apply format out fmt args)
    (format out "~%"))

(define (emit-error descr)
    (format #t "error - ~a~%" descr)
    (format out ".error ~a~a~a~%" #\" descr #\"))

(define (emit-label label)
    (format out "~a:~%" label))

(use-modules (ice-9 format)     ; string formatting
             (srfi srfi-1)      ; enhanced lists
             (srfi srfi-13)     ; enhanced strings
             (srfi srfi-60)     ; bitwise operations on integers
             (petty compile-program)
             (petty emit))

(define ipath  (list-ref (program-arguments) 1))
(define opath  (list-ref (program-arguments) 2))
(define source (list-ref (program-arguments) 3))

(define (compile-source-file in-path out-path filename)
  (call-with-input-file (format #f "~a/~a.scm" in-path filename)
    (lambda (in-file)
      (call-with-output-file (format #f "~a/~a.s" out-path filename)
        (lambda (out-file)
          (set! out out-file)
          ;; body of the main compilation function
          (emit-preamble)
          (let loop ((current-clause (read in)))
            (if (not (eof-object? current-clause))
                (begin
                  (compile-program current-clause)
                  (loop (read in)))
                (begin
                  (emit-lambda-epilogue global-env)
                  (emit "ret"))))
          (emit-epilogue))))))

(compile-source-file ipath opath source)
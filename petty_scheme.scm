(use-modules (ice-9 format)
             (srfi srfi-1)
             (srfi srfi-13)
             (srfi srfi-60))


(define tags (make-hash-table))

(hash-set! tags 'sys-int   '(2 #x3 #b00000000))
(hash-set! tags 'char      '(8 #xff #b00001111))
(hash-set! tags 'bool      '(7 #x7f #b00011111))
(hash-set! tags 'null-list '(8 #xff #b00101111))


(define (get-shift x)
  (car
    (hash-ref tags
      (cond
        ((null? x) 'null-list)
        ((integer? x) 'sys-int)
        ((char? x) 'char)
        ((boolean? x) 'bool)))))

(define (get-mask x)
  (cadr
    (hash-ref tags
      (cond
        ((null? x) 'null-list)
        ((integer? x) 'sys-int)
        ((char? x) 'char)
        ((boolean? x) 'bool)))))


(define (get-tag x)
  (caddr
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


(define (compile-program in-path out-path filename)
  (call-with-input-file (format #f "~a/~a.scm" in-path filename)
    (lambda (in)
      (call-with-output-file (format #f "~a/~a.s" out-path filename)
        (lambda (out)
          (define (emit fmt . args)
            (format out "~/")
            (apply format out fmt args)
            (format out "~%"))
          (define (emit-preamble)
            (format out ".text~%")
            (format out ".p2align 8,,15~%")
            (format out ".globl scheme_entry~%")
            (format out ".type scheme_entry, @function~%~%")
            (format out "scheme_entry:~%"))

          (emit-preamble)
          (let loop ((current-clause (read in)))
            (if (not (eof-object? current-clause))
                (begin
                  (emit "movq $~a, %rax" (immediate-rep current-clause))
                  (loop (read in)))
                (emit "ret"))))))))

(define ipath (cadr (program-arguments)))
(define opath (caddr (program-arguments)))
(define source (cadddr (program-arguments)))

(compile-program ipath opath source)
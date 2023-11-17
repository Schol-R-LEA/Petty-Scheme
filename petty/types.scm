(define-module (petty types)
  #:export (tags
            find-shift find-mask find-tag
            get-shift get-mask get-tag
            any->integer
            immediate? immediate-rep
            emit-freed emit-tagged emit-type-predicate))

(use-modules (srfi srfi-60)    ; bitwise operations on integers
             (petty system))


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


(define (emit-freed type reg)
  (let ((shift (find-shift (hash-ref tags type))))
    (emit-shift-left shift reg)))

(define (emit-tagged type reg)
    (let ((shift (find-shift (hash-ref tags type)))
          (tag (find-tag (hash-ref tags type))))
      (emit-shift-right shift reg)
      (emit-or-imm tag reg)))


(define (emit-type-predicate type reg)
  (let ((mask (find-mask (hash-ref tags type)))
        (tag (find-tag (hash-ref tags type))))
    (emit-load-imm tag acc)
    (emit-and-imm mask reg)
    (emit-comparison-predicate acc reg 'equal)))

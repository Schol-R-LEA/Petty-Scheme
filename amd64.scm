(define-module (petty amd64)
  #:export (word-size acc stack-pointer frame-pointer))


(define word-size 8)

(define acc "rax")
(define stack-pointer "rsp")
(define frame-pointer "rbp")
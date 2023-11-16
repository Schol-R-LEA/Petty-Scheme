(define-module (petty env)
  #:export (global-env bind lookup))

(use-module (petty amd64))


;; an environment consists of a hash table containing
;; symbols and stack offsets for the variables the symbols
;; reference. Each environment save the global one
;; also has a special symbol, env-parent-marker, which
;; holds a pointer to its parent environment.



(define global-env (make-hash-table))

(define env-parent-marker '_%%$_PARENT)


(define (get-env-size env)
  (hash-count (lambda (entry value) (not (hash-table? value))) env))


(define (bind sym env)
  (hash-set! env sym (* word-size (+ 1 (get-env-size env))))
  (hash-ref env sym))   ; return the assigned offset


(define (new-env parent)
  (let ((child (make-hash-table)))
    (hash-set! child env-parent-marker parent)
    child))


(define (lookup sym env)
  (let ((offset (hash-ref env sym)))
    (if offset
        offset
        (let ((parent (hash-ref env env-parent-marker)))
          (if parent
              (+ (lookup sym parent) (get-env-size parent))
              #f)))))
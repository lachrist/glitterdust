
(begin
  ;; Call
  (define $call (lambda (f xs i) (apply f xs)))
  ;; Creation
  (define $lambda (lambda (x l) x))
  (define $constant (lambda (x l) x))
  ;; Control flow
  (define $branch (lambda (x l) x))
  (define $eval (lambda (x l) x))
  ;; Environment
  (define $def (lambda (v x l) (cons x x)))
  (define $set (lambda (v x l) (cons x x)))
  (define $get (lambda (v x l) x)))
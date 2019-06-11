(begin
  (define $trace "")
  (define $log (lambda (xs)
    (set! $trace (string-join (list3 $trace (string-join xs) "\n")))))
  (define $show (lambda (x)
    (define name (nameof x))
    (if (eq? name #n)
      (address x)
      name)))
  ;; Call
  (define $call (lambda (f xs l)
    (begin
      ($log (list4 "before-call" ($show f) (map $show xs) l))
      (define r (apply f xs))
      ($log (list2 "after-call" ($show r)))
      r)))
  ;; Creation
  (define $lambda (lambda (x l)
    (begin
      ($log (list3 "lambda" ($show x) l))
      x)))
  (define $constant (lambda (x l)
    (begin
      ($log (list3 "constant" ($show x) l))
      x)))
  ;; Control flow
  (define $branch (lambda (x l)
    (begin
      ($log (list3 "if" ($show x) l))
      x)))
  (define $eval (lambda (x l)
    (begin
      ($log (list3 "eval" ($show x) l))
      x)))
  ;; Environment
  (define $define (lambda (v x l)
    (begin
      ($log (list4 "define" v ($show x) l))
      (cons x x))))
  (define $set (lambda (v x l)
    (begin
      ($log (list4 "set" v ($show x) l))
      (cons x x))))
  (define $get (lambda (v x l)
    (begin
      ($log (list4 "get" v ($show x) l))
      x))))

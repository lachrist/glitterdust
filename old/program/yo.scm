(begin
  (define x 3)
  (set! x (+ x 1))
  (define id (lambda (x) x))
  (id x))
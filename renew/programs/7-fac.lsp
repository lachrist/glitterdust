(let fac
  (lambda (f n)
    (if
      n
      (* n (f f (- n 1)))
      1))
  (fac fac 6))
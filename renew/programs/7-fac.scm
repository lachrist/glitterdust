(let fac
  (lambda (f n)
    (if
      n
      (* n (f f (- n 1)))
      1))
  (write
    (number->string
      (fac
        fac
        (string->number
          (read-line))))))
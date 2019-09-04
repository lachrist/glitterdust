(let fac
  (lambda (fac n)
    (if (= n 0) (* n (fac fac (- n 1))) 1))
  (print (fac fac (string->number (read-line)))))
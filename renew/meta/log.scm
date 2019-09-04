(let meta-literal
  (lambda (x p) (begin
    (print (inspect x) " << LITERAL@" p)
    x))
  (let meta-abstraction
    (lambda (vs f p) (begin
      (print (inspect f) " << ABSTRACTION@" p)
      f))
    (let meta-condition
      (lambda (x p) (begin
        (print (inspect x) " >> TEST@" p)
        x))
      (let meta-application
        (lambda (f xs p)
          (let r (apply f xs)
            (begin
              (print
                (inspect r) " << APPLY("
                (inspect f) " | "
                (apply inspect xs) ")@" p)
              r)))
        <INSTRUMENTED CODE>))))
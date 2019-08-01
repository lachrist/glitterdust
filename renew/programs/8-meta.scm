(let meta-literal
  (lambda (x)
    (let _      
      (write
        (string-append
          (inspect x)
          (string-append
            " << LITERAL["
            (string-append
              (any->string x)
              "]\n"))))
      x))
  (let meta-test
    (lambda (x)
      (write
        (string-append
          (inspect x)
          " >> TEST\n")))
    (let meta-apply-1
      (lambda (f x)
        (let r (f x)
          (let _
            (write
              (string-append
                (inspect r)
                (string-append
                  " << ("
                  (string-append
                    (inspect f)
                    (string-append
                      " "
                      (string-append
                        (inspect x)
                        ")\n"))))))
            r)))
      (let meta-apply-2
        (lambda (f x y)
          (let r (f x y)
            (let _
              (write
                (string-append
                  (inspect r)
                  (string-append
                    " << ("
                    (string-append
                      (inspect f)
                      (string-append
                        " "
                        (string-append
                          (inspect x)
                          (string-append
                            " "
                            (string-append
                              (inspect y)
                              ")\n"))))))))
              r)))
        (meta-apply-2
          +
          (meta-literal 1)
          (meta-literal 2))))))
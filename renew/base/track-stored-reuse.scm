(let x 123
  (print
    (inspect x)                      ; &37[123]
    (inspect 123)                    ; &37[123]
    (inspect (let y x y))            ; &37[123]
    (inspect ((lambda (x) x) x))     ; &37[123]
    (inspect (car (cons x "foo"))))) ; &37[123]
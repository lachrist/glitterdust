(let values (cons #n #n)
  (let identity (lambda (self x xs i)
    (if (eq? x (car xs))
      i
      (if (null? (cdr xs))
        (begin
          (set-cdr! xs (cons x #n))
          (+ i 1))
        (self self x (cdr xs) (+ i 1)))))
    (let inspect (lambda (x)
      (string-append
        "&"
        (number->string
          (identity identity x values 0))
        "["
        (any->string x)
        "]"))
      (let inspectAll (lambda (self xs)
        (if (null? xs)
          ""
          (string-append
            (inspect (car xs))
            (if (null? (cdr xs)) "" ", ")
            (self self (cdr xs)))))
        (let log (lambda (f xs p)
          (let r (apply f xs)
            (begin
              (print (inspect r) " = " f "(" (inspectAll inspectAll xs) ") // " p)
              r)))
          (let solve (lambda (a b c)
            (let d
              (log sqrt (list
                (log - (list
                  (log expt (list b 2) "d.1")
                  (log * (list 4 a c) "d.2"))
                  "d.3"))
                "d")
              (cons
                (log / (list
                  (log - (list (log - (list b) "s1.1") d) "s1.2")
                  (log * (list 2 a) "s1.3"))
                  "s1")
                (log / (list
                  (log + (list (log - (list b) "s2.1") d) "s2.2")
                  (log * (list 2 a) "s2.3"))
                  "s2"))))
            (let prompt (lambda (n)
              (begin
                (print "Enter a number for '" n "'...")
                (log string->number (list (log read-line (list) n)) n)))
              (let a (prompt "a")
                (let b (prompt "b")
                  (let c (prompt "c")
                    (let p (solve a b c)
                      (log print (list "Sol1 = " (car p) ", Sol2 = " (cdr p)) ""))))))))))))

; (let log (lambda (f xs p)
;   (let r (apply f xs) (begin
;     (print (inspect r) " = " f "(" (apply inspect xs) ") // " p)
;     r)))
;   (let solve (lambda (a b c)
;     (let d
;       (log sqrt (list
;         (log - (list
;           (log expt (list b 2) "d.1")
;           (log * (list 4 a c) "d.2"))
;           "d.3"))
;         "d")
;       (cons
;         (log / (list
;           (log - (list (log - (list b) "s1.1") d) "s1.2")
;           (log * (list 2 a) "s1.3"))
;           "s1")
;         (log / (list
;           (log + (list (log - (list b) "s2.1") d) "s2.2")
;           (log * (list 2 a) "s2.3"))
;           "s2"))))
;     (let prompt (lambda (n)
;       (begin
;         (print "Enter a number for '" n "'...")
;         (log string->number (list (log read-line (list) n)) n)))
;       (let a (prompt "a")
;         (let b (prompt "b")
;           (let c (prompt "c")
;             (let p (solve a b c)
;               (log print (list "Sol1 = " (car p) ", Sol2 = " (cdr p)) ""))))))))
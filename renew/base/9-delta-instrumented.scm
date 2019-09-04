(let meta-literal
  (lambda (x l) (begin
    (print (inspect x) " << LITERAL[" (any->string x) "]@" l)
    x))
  (let meta-condition
    (lambda (x l) (begin
      (print (inspect x) " >> TEST@" l)
      x))
    (let meta-application
      (lambda (f xs l)
        (let r
          (apply f xs)
          (let loop
            (lambda (loop xs)
              (if (null? xs)
                ""
                (string-append ", " (inspect (car xs)) (loop loop (cons xs)))))         
            (begin
              (print (inspect r) " << APPLY[" (inspect f) (loop xs) "]@" l)
              r))))

(
  meta-application
  "1:1"
  begin
  (
    list
    (
      meta-application
      "2:3"
      print
      (
        list
        (
          meta-literal
          "2:10"
          "Solver for \"a * x^2 + b * x + c\":")))
    (let a
      (
        meta-application
        "4:5"
        begin
        (
          list
          (
            meta-application
            "5:7"
            print
            (
              list
              (
                meta-literal
                "5:14"
                "Enter a number for 'a' ...")))
          (
            meta-application
            "6:7"
            string->number
            (
              list
              (
                meta-application
                "6:23"
                read-line
                (
                  list))))))
      (let b
        (
          meta-application
          "8:7"
          begin
          (
            list
            (
              meta-application
              "9:9"
              print
              (
                list
                (
                  meta-literal
                  "9:16"
                  "Enter a number for 'b' ...")))
            (
              meta-application
              "10:9"
              string->number
              (
                list
                (
                  meta-application
                  "10:25"
                  read-line
                  (
                    list))))))
        (let c
          (
            meta-application
            "12:9"
            begin
            (
              list
              (
                meta-application
                "13:11"
                print
                (
                  list
                  (
                    meta-literal
                    "13:18"
                    "Enter a number for 'c' ...")))
              (
                meta-application
                "14:11"
                string->number
                (
                  list
                  (
                    meta-application
                    "14:27"
                    read-line
                    (
                      list))))))
          (let d
            (
              meta-application
              "16:11"
              sqrt
              (
                list
                (
                  meta-application
                  "16:17"
                  -
                  (
                    list
                    (
                      meta-application
                      "16:20"
                      expt
                      (
                        list
                        b
                        (
                          meta-literal
                          "16:28"
                          2)))
                    (
                      meta-application
                      "16:31"
                      *
                      (
                        list
                        (
                          meta-literal
                          "16:34"
                          4)
                        a
                        c))))))
            (let r1
              (
                meta-application
                "18:13"
                /
                (
                  list
                  (
                    meta-application
                    "18:16"
                    -
                    (
                      list
                      (
                        meta-application
                        "18:19"
                        -
                        (
                          list
                          b))
                      d))
                  (
                    meta-application
                    "18:28"
                    *
                    (
                      list
                      (
                        meta-literal
                        "18:31"
                        2)
                      a))))
              (let r2
                (
                  meta-application
                  "20:15"
                  /
                  (
                    list
                    (
                      meta-application
                      "20:18"
                      +
                      (
                        list
                        (
                          meta-application
                          "20:21"
                          -
                          (
                            list
                            b))
                        d))
                    (
                      meta-application
                      "20:30"
                      *
                      (
                        list
                        (
                          meta-literal
                          "20:33"
                          2)
                        a))))
                (
                  meta-application
                  "21:15"
                  print
                  (
                    list
                    (
                      meta-literal
                      "21:22"
                      "Sol1 = ")
                    r1
                    (
                      meta-literal
                      "21:35"
                      ", Sol2 = ")
                    r2))))))))))


)))
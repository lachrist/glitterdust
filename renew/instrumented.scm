(
  meta-application
  begin
  (
    list
    (
      meta-application
      print
      (
        list
        (
          meta-literal
          "Solver for \"a * x^2 + b * x + c\":"
          "2:10"))
      "2:3")
    (let a
      (
        meta-application
        begin
        (
          list
          (
            meta-application
            print
            (
              list
              (
                meta-literal
                "Enter a number for 'a' ..."
                "5:14"))
            "5:7")
          (
            meta-application
            string->number
            (
              list
              (
                meta-application
                read-line
                (
                  list)
                "6:23"))
            "6:7"))
        "4:5")
      (let b
        (
          meta-application
          begin
          (
            list
            (
              meta-application
              print
              (
                list
                (
                  meta-literal
                  "Enter a number for 'b' ..."
                  "9:16"))
              "9:9")
            (
              meta-application
              string->number
              (
                list
                (
                  meta-application
                  read-line
                  (
                    list)
                  "10:25"))
              "10:9"))
          "8:7")
        (let c
          (
            meta-application
            begin
            (
              list
              (
                meta-application
                print
                (
                  list
                  (
                    meta-literal
                    "Enter a number for 'c' ..."
                    "13:18"))
                "13:11")
              (
                meta-application
                string->number
                (
                  list
                  (
                    meta-application
                    read-line
                    (
                      list)
                    "14:27"))
                "14:11"))
            "12:9")
          (let d
            (
              meta-application
              sqrt
              (
                list
                (
                  meta-application
                  -
                  (
                    list
                    (
                      meta-application
                      expt
                      (
                        list
                        b
                        (
                          meta-literal
                          2
                          "16:28"))
                      "16:20")
                    (
                      meta-application
                      *
                      (
                        list
                        (
                          meta-literal
                          4
                          "16:34")
                        a
                        c)
                      "16:31"))
                  "16:17"))
              "16:11")
            (let r1
              (
                meta-application
                /
                (
                  list
                  (
                    meta-application
                    -
                    (
                      list
                      (
                        meta-application
                        -
                        (
                          list
                          b)
                        "18:19")
                      d)
                    "18:16")
                  (
                    meta-application
                    *
                    (
                      list
                      (
                        meta-literal
                        2
                        "18:31")
                      a)
                    "18:28"))
                "18:13")
              (let r2
                (
                  meta-application
                  /
                  (
                    list
                    (
                      meta-application
                      +
                      (
                        list
                        (
                          meta-application
                          -
                          (
                            list
                            b)
                          "20:21")
                        d)
                      "20:18")
                    (
                      meta-application
                      *
                      (
                        list
                        (
                          meta-literal
                          2
                          "20:33")
                        a)
                      "20:30"))
                  "20:15")
                (
                  meta-application
                  print
                  (
                    list
                    (
                      meta-literal
                      "Sol1 = "
                      "21:22")
                    r1
                    (
                      meta-literal
                      ", Sol2 = "
                      "21:35")
                    r2)
                  "21:15"))))))))
  "1:1")

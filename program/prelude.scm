
(begin
  ;; list
  (define list0 (lambda () #n))
  (define list1 (lambda (x1) (cons x1 #n)))
  (define list2 (lambda (x1 x2) (cons x1 (list1 x2))))
  (define list3 (lambda (x1 x2 x3) (cons x1 (list2 x2 x3))))
  (define list4 (lambda (x1 x2 x3 x4) (cons x1 (list3 x2 x3 x4))))
  (define list5 (lambda (x1 x2 x3 x4 x5) (cons x1 (list4 x2 x3 x4 x5))))
  (define list? (lambda (x)
    (|| (eq? x #n) (&& (eq? (typeof x) "cons") (list? (cdr x))))))
  (define foldl (lambda f z xs)
    (if (eq? xs #n) z
      (foldl f (f z (car xs)) (cdr xs))))
  (define foldr (lambda f z xs)
    (if (eq? xs #n) z
      (f (car xs) (foldr f z (cdr xs)))))
  (define length (lambda (xs)
    (foldr (lambda (_ n) (+ n 1)) 0 xs)))
  (define map (lambda (f xs)
    (foldr (lambda (x xs) (cons (f x) xs)) #n xs)))
  (define filter (lambda (f xs)
    (foldr (lambda (x xs) (if (f x) (cons x xs) xs)) #n xs)))
  (define last (lambda xs)
    (foldr (lambda (x _) x) #n xs))
  (define intersperse (lambda (x xs)
    (if (eq? xs #n) #n
      (if (eq? (cdr xs) #n) xs
        (cons (car x) (cons x (intersperse x (cdr xs))))))))
  ;; lambda
  (define apply (lambda (f xs)
    (begin
      (define l (length xs))
      (if (== l 0) (f)
        (if (== l 1) (f (car xs))
          (if (== l 2) (f (car xs) (car (cdr xs)))
            (if (== l 3) (f (car xs) (car (cdr xs)) (car (cdr (cdr xs))))
              (if (== l 4) (f (car xs) (car (cdr xs)) (car (cdr (cdr xs))) (car (cdr (cdr (cdr xs)))))
                (raise "apply cannot handle more than 4 arguments")))))))))
  ;; string
  (define string-join (xs)
    (foldr (lambda (x s) (string-append x s)) "" xs))
  (define show (x)
    (if (eq? x #n) "#n"
      (if (eq? x #t) "#t"
        (if (eq? x #f) "#f"
          (if (list? x) (join (list3 "(" (string-join (intersperse " " x)) ")")) 
          (begin
            (define t (typeof x))
            (if (eq? t "number") (number->string x)
              (if (eq? t "string") (join (list3 "\"" x "\""))
                (if (eq? t "cons") (join (list5 "(" (show (car x)) " . " (show (cdr x)) ")"))
                  t)))))))))
  ;; functional
  (define identity (lambda (x) x))
  ;; pairs
  (define pairs-lookup (lambda (x xys)
      (if (eq? xys #n)
        #n
        (if (eq? (car (car xys)) x)
          (cdr (car xys))
          (pairs-lookup (x (cdr xys)))))))
  ;; nameof
  (define nameof ((lambda ()
    (begin
      (define names #n)
      (define add (lambda (x s)
        (set! names (cons (cons (eval s) s) names))))
      (add "list1")
      (add "list2")
      (add "list3")
      (add "list4")
      (add "list5")
      (add "list?")
      (add "foldr")
      (lambda (x) (pairs-lookup x names)))))))

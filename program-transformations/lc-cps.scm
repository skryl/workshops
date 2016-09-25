(let ()
  (define empty-env (lambda (y) 0))
  
  (define lc-cps
    (lambda (exp env k)
      (begin
        (show exp)
        (if (symbol? exp)
            (k (env exp))
            (if (pair? exp)
                (if (eq? (car exp) 'sub1)
                    (lc-cps (car (cdr exp)) env (lambda (v) (k (- v 1))))
                    (if (eq? (car exp) 'zero?)
                        (lc-cps (car (cdr exp)) env (lambda (v) (k (= v 0))))
                        (if (eq? (car exp) '*)
                            (lc-cps (car (cdr exp)) env (lambda (v1)
                                                          (lc-cps (car (cdr (cdr exp))) env (lambda (v2)
                                                                                              (k (* v1 v2))))))
                            (if (eq? (car exp) 'if)
                                (lc-cps (car (cdr exp)) env (lambda (vc)
                                                              (if vc
                                                                  (lc-cps (car (cdr (cdr exp))) env k)
                                                                  (lc-cps (car (cdr (cdr (cdr exp)))) env k))))
                                (if (eq? (car exp) 'lambda)
                                    (k (lambda (a k)
                                         (lc-cps (car (cdr (cdr exp)))
                                                 (lambda (y) (if (eq? (car (car (cdr exp))) y) a (env y)))
                                                 k)))
                                    (lc-cps (car exp) env (lambda (vrator)
                                                            (lc-cps (car (cdr exp)) env (lambda (vrand)
                                                                                          (vrator vrand k))))))))))
                (k exp))))))

  (define lc
    (lambda (exp env)
      (lc-cps exp env (lambda (v) v))))

  (define factorial6
    '(((lambda (fun)
     ((lambda (F)
        (F F))
      (lambda (F)
        (fun (lambda (x) ((F F) x))))))
       (lambda (factorial)
         (lambda (n)
           (if (zero? n)
               1
                      (* n (factorial (sub1 n)))))))
      5))
  
  (show (lc factorial6 empty-env)))

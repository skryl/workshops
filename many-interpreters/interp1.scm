(load "prereqs.scm")

(display "Time for the star of the show: lambda.")
(newline)

(define init-env
  (lambda (y)
    (if ((ceq? '+) y) c+
    (if ((ceq? '*) y) c*
    (if ((ceq? 'add1) y) add1
    (if ((ceq? 'sub1) y) sub1
        error))))))

(define value-of
  (lambda (exp)
    (lambda (env)
      (pmatch exp
        [,n (guard (number? n)) n]
        [,x (guard (symbol? x)) (env x)]
        [(lambda (,x) ,body)
         (lambda (a)
           ((value-of body)
;;          add the environment
            ))]
        [(,rator ,rand)
         (((value-of rator) env) ((value-of rand) env))]))))

(define eval-exp
  (lambda (exp)
    ((value-of exp) init-env)))

;;;;;;;;;;;;;;;;; Test Suite

(load "test-data.scm")

(test-check "fifteen is 15"
  (eval-exp '15)
  15)

(test-check "basic interp"
  (eval-exp '((* 5) ((+ 5) 6)))
  55)

(test-check "application"
  (eval-exp '((lambda (x) x) 5))
  5)

(test-check "basic lambda-calc test"
  (eval-exp '(((lambda (y)
                  (lambda (x)
                    ((+ x) (sub1 y))))
                15)
               11))
  25)

(test-check "complex countdown"
  (eval-exp complex-countdown)
  1)


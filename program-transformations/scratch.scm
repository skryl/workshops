;; Interpreters
;;

(load "test-check.scm")
(load "pmatch.scm")
(load "sugar.scm")

;;; Lambda Calculus
;;; e = x              (variable)
;;;     (lambda (x) e) (abstraction)
;;;     (e e)          (application)

;; Scheme = Extended Call-by-value lambda calculus

;;; env: variable name -> value
;;;
(define empty-env 
  (lambda (x) (error 'env-lookup "unbound variable"))) 

(define eval-exp
  (lambda (exp env)
    (pmatch
      exp
      (,x (guard (symbol? x)) (env x))
      (,n (guard (number? n)) n)
      (,b (guard (boolean? b)) b)
      ((zero? ,e)
       (zero? (eval-exp e env)))
      ((sub1 ,e)
       (sub1 (eval-exp e env)))
      ((* ,e1 ,e2)
       (* (eval-exp e1 env) (eval-exp e2 env)))
      ((if ,c ,a ,b)
       (if (eval-exp c env)
           (eval-exp a env)
           (eval-exp b env)))
      ((lambda (,x) ,body)
       (lambda (a)
         (eval-exp body (lambda (y) (if (eq? y x)
                                      a
                                      (env y))))))
      ((,e1 ,e2)
       ((eval-exp e1 env)
        (eval-exp e2 env)))
      )))

(define eval-top
  (lambda (exp)
    (eval-exp exp empty-env)))

;; testing
(eval-exp 'x
          (lambda (y) (if (eq? y 'x) 1 (empty-env y))))

(eval-top '((lambda (x) 1) 2))

(eval-top '(* 3 4))

;;; Program Transformations
;;; CPS: continuation-passing style

;;; continuuations
;;;
(+ 1 2 (+ 3 4))
((lambda (HOLE) (+ 1 2 HOLE)) (+ 3 4))
((lambda (HOLE) (+ 1 2 HOLE)) 9)

;;; continuation-passing-style
;;;

(define factorial
  (lambda (n)
    (if (= n 0)
      1
      (* n (factorial (- n 1))))))

(trace factorial)
(factorial 5)

(define factorial
  (lambda (n acc)
    (if (= n 0)
      acc
      (factorial (- n 1) (* acc n)))))

(factorial 5 1)

(define factorial-cps
  (lambda (n k)
    (if (= n 0)
      (k 1)
      (factorial-cps (- n 1) (lambda (v) (k (* n v)))))))

(define factorial
  (lambda (n)
    (factorial-cps n (lambda (v) v))))

(factorial 5)

(define (fib n)
  (if (< n 2) n
    (+ (fib (- n 1)) (fib (- n 2)))))

(fib 10)

(define (fib-cps n k)
  (if (< n 2) (k n)
    (fib-cps (- n 2) (lambda (v1) 
                       (fib-cps (- n 1) 
                                (lambda (v2) (k (+ v1 v2))))))))

(define (fib n)
  (lambda (n) 
    (fib-cps n (lambda (v) v))))

;;; Trampolining

(define factorial-cps
  (lambda (n k)
    (lambda ()
      (if (= n 0)
        (k 1)
        (factorial-cps (- n 1) (lambda (v) (k (* n v)))))))) 

(define factorial
  (lambda (n)
    (let ((r (factorial-cps n (lambda (v) v)) ))
      (while (procedure? r)
             (set! r (r))
             r))
    ))

(factorial 5)


;;; interpreter cps
;;;
(define empty-env 
  (lambda (x) (error 'env-lookup "unbound variable"))) 

(define eval-exp-cps
  (lambda (exp env k)
    (pmatch
      exp
      (,x (guard (symbol? x)) (k (env x)))
      (,n (guard (number? n)) (k n))
      (,b (guard (boolean? b)) (k b))
      ((zero? ,e)
       (eval-exp-cps e env (lambda (v)
                             (k (zero? v)))))
      ((sub1 ,e)
       (eval-exp-cps e env (lambda (v)
                             (k (sub1 v)))))
      ((* ,e1 ,e2)
       (eval-exp-cps e1 env (lambda (v1)
                              (eval-exp-cps e2 env (lambda (v2)
                                                     (k (* v1 v2)))))))
      ((if ,c ,a ,b)
       (eval-exp-cps c env (lambda (vc)
                             (if vc
                               (eval-exp-cps a env k)
                               (eval-exp-cps b env k)))))
      ((lambda (,x) ,body)
       (k (lambda (a k^)
         (eval-exp-cps body 
                       (lambda (y) 
                         (if (eq? y x)
                                      a
                                      (env y))) k^))))
      ((,e1 ,e2)
       (eval-exp-cps e1 env (lambda (p) 
                              (eval-exp-cps e2 env (lambda (a)
                                                     (p a k)))))))))

(define eval-top
  (lambda (exp)
    (eval-exp-cps exp empty-env (lambda (v) v))))

(eval-top '(* 3 4))

;;; Representation independence

(define apply-k
  (lambda (k v) (k v)))

(define empty-k
  (lambda ()
    (lambda (v) v)))

(define fib-k
  (lambda (n k)
    (lambda (v)
      (apply-k k (* n v)))))

(define factorial-cps
  (lambda (n k)
    (if (= n 0)
      (apply-k k 1)
      (factorial-cps (- n 1) (fib-k n k)))))

(define factorial
  (lambda (n)
    (factorial-cps n (empty-k))))

(factorial 5)

;;; first order representation for continuations
;;; defunctionalization

(define apply-k
  (lambda (k^ v) 
    (pmatch
      k^
      ((empty-k)
       v)
      ((factorial-k ,n ,k)
        (apply-k k (* n v)))  
      (,k^ (guard (procedure? k^))
          (k^ v))
      )))

(define empty-k
  (lambda ()
    `(empty-k)))

(define factorial-k
  (lambda (n k)
    `(factorial-k ,n ,k)))

(define factorial-cps
  (lambda (n k)
    (if (= n 0)
      (apply-k k 1)
      (factorial-cps (- n 1) (factorial-k n k)))))

(define factorial
  (lambda (n)
    (factorial-cps n (empty-k))))

(factorial 5)

;;; first order rep for interpreter

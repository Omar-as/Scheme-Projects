#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp

      (const-exp (num) (num-val num))

      (var-exp (var) (apply-env env var))

      (if-exp (exp1 exp2 exp3 exp4 exp5) 
              (let ((pred (expval->bool (value-of exp1 env)))
                    (body1 (expval->num (value-of exp2 env)))
                    (body2 (expval->num (value-of exp5 env))))
                    (if pred
                        (num-val body1)
                        (if (not (null? exp3))
                          (value-of (if-exp (car exp3) (car exp4) (cdr exp3) (cdr exp4) exp5) env) 
                          (num-val body2)))))

      (op-exp (exp1 exp2 op)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                  (let ((num1 (expval->rational val1))
                        (num2 (expval->rational val2)))
                      (cond 
                        ((and (number? num1) (number? num2))
                          (num-val
                            (cond 
                              ((= op 1) (+ num1 num2))
                              ((= op 2) (* num1 num2))
                                    ;; -----------------------
                                    ;; INSERT YOUR CODE HERE 
                                    ;; -----------------------
                              ((= op 3) (/ num1 num2))
                              (else (- num1 num2))
                                    ;; -----------------------
                              )))
                        
                        ((and (number? num1) (not (number? num2)))
                          (rational-val
                          (let ((num2top (car num2))
                                (num2bot (cdr num2)))
                            (cond 
                              ((= op 1) (cons (+ (* num1 num2bot) num2top) num2bot))
                              ((= op 2) (cons (* num1 num2top) num2bot))
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE 
                              ;; -----------------------
                              ((= op 3) (cons (* num1 num2bot) num2top))
                              (else (cons (- (* num1 num2bot) num2top) num2bot))
                              ;; -----------------------

                              
                              ))))

                        ((and (number? num2) (not (number? num1)))
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1)))
                            (cond 
                              ((= op 1) (cons (+ (* num1bot num2) num1top) num1bot))
                              ((= op 2) (cons (* num1top num2) num1bot))
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE 
                              ;; -----------------------
                              ((= op 3) (cons num1top (* num2 num1bot)))
                              (else (cons (- num1top (* num2 num1bot)) num1bot))
                              ;; -----------------------
                              ))))

                        (else
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1))
                                (num2top (car num2))
                                (num2bot (cdr num2)))
                            (cond 
                              ((= op 1) (cons (+ (* num1top num2bot) (* num1bot num2top)) (* num1bot num2bot))) ;; add
                              ((= op 2) (cons (* num1top num2top) (* num1bot num2bot))) ;; multiply
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE 
                              ;; -----------------------
                              ((= op 3) (cons (* num1top num2bot) (* num1bot num2top)))
                              (else (cons (- (* num1top num2bot) (* num2top num1bot)) (* num1bot num2bot)))
                              ;; ----------------------- 
                            ))))))))
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->rational val1)))
                     (if (number? num1)
                        (bool-val (zero? num1))
                          ;; -----------------------
                          ;; INSERT YOUR CODE HERE 
                          ;; -----------------------
                        (bool-val (zero? (car num1)))
                          ;; ----------------------- 
                        ))))

      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))

      ;; -----------------------
      ;; INSERT YOUR CODE HERE 
      ;; -----------------------

      (list-exp ()
              (list-val '()))

      (cons-exp (exp1 exp2)
              (let ((val1 (expval->num (value-of exp1 env)))
                    (val2 (expval->list (value-of exp2 env))))
                   (list-val (cons val1 val2))))

      (sum-exp (exp1) 
               (let ((val1 (expval->list (value-of exp1 env))))
                (num-val (sum-helper val1))))

      (rational-exp (num1 num2)
                    (if (zero? num2)
                        (eopl:error "the value of the denominator cannot be 0")
                        (rational-val (cons num1 num2))))
      (simpl-exp (exp1)
                 (let ((val1 (expval->rational (value-of exp1 env))))
                      (let ((gcd1 (gcd-simpl (car val1) (cdr val1)))
                            (num (car val1))
                            (denum (cdr val1)))
                           (rational-val (cons (/ num gcd1) (/ denum gcd1))))))
      
      ;; -----------------------

      )))
(define (sum-helper lst)
            (if (null? lst)
                 0
                 (+ (car lst) (sum-helper (cdr lst)))))

(define (gcd-simpl n1 n2)
        (if (zero? n2) 
            n1
            (gcd-simpl n2 (remainder n1 n2))))


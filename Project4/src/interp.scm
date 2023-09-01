(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))
      
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (newref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (ref-val (newref v1))))

        (deref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (let ((ref1 (expval->ref v1)))
              (deref ref1))))

        (setref-exp (exp1 exp2)
          (let ((ref (expval->ref (value-of exp1 env))))
            (let ((v2 (value-of exp2 env)))
              (begin
                (setref! ref v2)
                (num-val 23)))))

        ; #####################################################
        ; ###### ENTER YOUR CODE HERE
        ; ###### value-of cases for new expressions, remember
        ; ###### that you need to use memory functionalities. 
        ; #####################################################
    
	(newvector-exp (exp1 exp2)
	  (let ((len (expval->num (value-of exp1 env)))
		(val (value-of exp2 env)))
	    (let ((ref (newref val)))
	      (begin 
		(newref-helper (- len 1) val)
		(vec-val (vect len ref))))))
	
	(update-vector-exp (exp1 exp2 exp3)
	  (let ((vec1 (expval->vec (value-of exp1 env)))
		(index (expval->num (value-of exp2 env)))
		(val (value-of exp3 env)))
	    (begin
	      (cases vec vec1
		     (vect (len ref)
	      (setref! (+ index ref) val))))))

	(read-vector-exp (exp1 exp2)
	  (let ((vec1 (expval->vec (value-of exp1 env)))
		(index (expval->num (value-of exp2 env))))
	    (begin
	      (cases vec vec1
		     (vect (len ref)
	      (deref (+ index ref)))))))


	(length-vector-exp (exp1)
	  (let ((vec1 (expval->vec (value-of exp1 env))))
	      (cases vec vec1
		     (vect (len ref)
		(num-val len)))))

	(swap-vector-exp (exp1 exp2 exp3)
	  (let ((vec1 (expval->vec (value-of exp1 env)))
		(index1 (expval->num (value-of exp2 env)))
		(index2 (expval->num (value-of exp3 env))))
	    (cases vec vec1
		   (vect (len ref)
			(let ((val1 (deref (+ index1 ref)))
			      (val2 (deref (+ index2 ref))))
			  (begin
			  	(setref! (+ ref index1) val2)
			  	(setref! (+ ref index2) val1)))))))



	(copy-vector-exp (exp1)
	  (let ((vec1 (expval->vec (value-of exp1 env))))
	      (cases vec vec1
		     (vect (len ref)
			(let ((vecref (newref (deref ref))))
			  (begin 
			   (copy-vector-helper (- len 1) (+ ref 1))
			   (vec-val (vect len vecref))))))))
			 
    (newstack-exp (exp1)
                  (let ((len (+ (expval->num (value-of exp1 env)) 1)))
                       (value-of (newvector-exp (const-exp len) (const-exp 0)) env)))

    (push-exp (exp1 exp2)
              (let ((stack (expval->vec (value-of exp1 env))))
                   (cases vec stack
                          (vect (len ref)
                                (let ((nextIdx (expval->num (value-of (read-vector-exp exp1 (const-exp (- len 1))) env))))
                                     (if (<= nextIdx (- len 2))
                                         (begin 
                                           (value-of (update-vector-exp exp1 (const-exp nextIdx) exp2) env)
                                           (value-of (update-vector-exp exp1 (const-exp (- len 1)) (const-exp (+ nextIdx 1))) env))
                                         (eopl:error 'stack-overflow "error: stack is full")))))))

    (pop-exp (exp1) 
              (let ((stack (expval->vec (value-of exp1 env)))
                    (stackEmpty (expval->bool (value-of (empty-stack?-exp exp1) env))))
                    (if stackEmpty
                       (num-val -1)
                       (cases vec stack
                           (vect (len ref)
                                 (let ((nextIdx (expval->num (value-of (read-vector-exp exp1 (const-exp (- len 1))) env))))
                                       (let ((output (value-of (read-vector-exp exp1 (const-exp (- nextIdx 1))) env)))
                                         (begin 
                                            (value-of (update-vector-exp exp1 (const-exp nextIdx) (const-exp 0)) env)
                                            (value-of (update-vector-exp exp1 (const-exp (- len 1)) (const-exp (- nextIdx 1))) env)
                                            output))))))))

    (stack-size-exp (exp1)
                    (let ((stack (expval->vec (value-of exp1 env))))
                          (cases vec stack
                            (vect (len ref)
                                (let ((nextIdx (expval->num (value-of (read-vector-exp exp1 (const-exp (- len 1))) env))))
                                      (num-val nextIdx))))))

    (peek-exp (exp1) 
              (let ((stack (expval->vec (value-of exp1 env))))
                   (cases vec stack
                          (vect (len ref)
                                (let ((nextIdx (expval->num (value-of (read-vector-exp exp1 (const-exp (- len 1))) env))))
                                      (let ((output (value-of (read-vector-exp exp1 (const-exp (- nextIdx 1))) env)))
                                           output))))))

    (empty-stack?-exp (exp1)
                    (let ((stack (expval->vec (value-of exp1 env))))
                          (cases vec stack
                            (vect (len ref)
                                (let ((nextIdx (expval->num (value-of (read-vector-exp exp1 (const-exp (- len 1))) env))))
                                      (bool-val (zero? nextIdx)))))))

    (print-stack-exp (exp1)
                    (let ((stack (expval->vec (value-of exp1 env))))
                          (cases vec stack
                            (vect (len ref)
                                (let ((nextIdx (expval->num (value-of (read-vector-exp exp1 (const-exp (- len 1))) env))))
                                      (print-stack-helper ref (- nextIdx 1)))))))

    (vec-mult-exp (exp1 exp2)
	  (let ((vec1 (expval->vec (value-of exp1 env)))
            (vec2 (expval->vec (value-of exp2 env))))
            (cases vec vec1 
                   (vect (len1 ref1)
                         (cases vec vec2 
                            (vect (len2 ref2)
                                  (if (= len1 len2)
                                    (let ((ref3 (newref (num-val (* (expval->num (deref ref1)) (expval->num (deref ref2)))))))
                                         (begin 
                                           (vec-mult-helper (+ ref1 1) (+ ref2 1) len1 0)
                                           (vec-val (vect len1 ref3))))
                                    (eopl:error 'vector-sizes-not-equal "error: vectors have different sizes"))))))))
	
        ; #####################################################
        )))

  ; ###### YOU CAN WRITE HELPER FUNCTIONS HERE

  (define (newref-helper len val) 
    (if (= len 0)
      0
      (begin 
	    (newref val)
	    (newref-helper (- len 1) val))))

  (define (copy-vector-helper len ref)
    (if (<= len 0)
      0
      (begin
	    (newref (deref ref))
	    (copy-vector-helper (- len 1) (+ ref 1)))))

  (define (vec-mult-helper ref1 ref2 len counter)
          (if (= (- len 1) counter)
              0
              (begin 
                (newref (num-val (* (expval->num (deref ref1)) (expval->num (deref ref2)))))
                (vec-mult-helper (+ ref1 1) (+ ref2 1) len (+ counter 1)))))

  (define (print-stack-helper ref currIdx)
          (if (< currIdx 0)
              (num-val 0)
              (begin 
                (pretty-print (expval->num (deref (+ ref currIdx))))
                (print-stack-helper ref (- currIdx 1)))))
  
  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; 
  ;; uninstrumented version
  ;;   (define apply-procedure
  ;;    (lambda (proc1 arg)
  ;;      (cases proc proc1
  ;;        (procedure (bvar body saved-env)
  ;;          (value-of body (extend-env bvar arg saved-env))))))

  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
	  (let ((r arg))
	    (let ((new-env (extend-env var r saved-env)))
	      (when (instrument-let)
		(begin
		  (eopl:printf
		    "entering body of proc ~s with env =~%"
		    var)
		  (pretty-print (env->list new-env))
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env)))))))


  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))
 
  )
  


  

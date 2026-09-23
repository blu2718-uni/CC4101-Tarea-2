#lang play

#|
Nombre: Julio Yáñez
|#

(print-only-errors #t)

#|
Ejercicio 1
|#

;; b)

(test (parse-prop 'true) (tt))
(test (parse-prop 'false) (ff))
(test (parse-prop 'x) (p-id 'x))
(test (parse-prop '(not true)) (p-not (tt)))
(test/exn (parse-prop '(and true)) "parse-prop: and expects at least two operands.")
(test/exn (parse-prop '(or)) "parse-prop: or expects at least two operands.")
(test (parse-prop '(and true false true)) (p-and (list (tt) (ff) (tt))))
(test (parse-prop '(or false true true)) (p-or (list (ff) (tt) (tt))))
(test (parse-prop '(with x false (and true x))) (p-with 'x (ff) (p-and (list (tt) (p-id 'x)))))

;; c)

(test (p-subst (p-id 'x) 'x (tt)) (tt))
(test (p-subst (p-id 'x) 'y (tt)) (p-id 'x))
(test (p-subst (p-with 'x (ff) (p-and (list (tt) (p-id 'x)))) 'x (tt)) 
      (p-with 'x (ff) (p-and (list (tt) (p-id 'x)))))
(test (p-subst (p-with 'y (ff) (p-and (list (tt) (p-id 'x)))) 'x (tt)) 
      (p-with 'y (ff) (p-and (list (tt) (tt)))))

(test (nest '() (num 3)) (num 3))
(test (nest '(y z) (num 3)) (fun '(y) (fun '(z) (num 3))))

(test (currying (closureV '(x) (num 5) (mtEnv)))
      (closureV '(x) (num 5) (mtEnv)))
(test (currying (closureV '(x y z) (add (id 'x) (add (id 'y) (id 'z))) (mtEnv)))
      (closureV '(x)
                (fun '(y) (fun '(z) (add (id 'x) (add (id 'y) (id 'z)))))
                (mtEnv)))

(test (uncurrying (closureV '(x y) (add (id 'x) (id 'y)) (mtEnv)))
      (closureV '(x y) (add (id 'x) (id 'y)) (mtEnv)))
(test (uncurrying
       (closureV '(x) (fun '(y) (fun '(z) (add (id 'x) (add (id 'y) (id 'z)))))
                 (mtEnv)))
      (closureV '(x y z) (add (id 'x) (add (id 'y) (id 'z))) (mtEnv)))

(test (swapping (closureV '(x) (num 1) (mtEnv)))
      (closureV '(x) (num 1) (mtEnv)))   ; un parámetro: igual
(test (swapping (closureV '(x y) (add (id 'x) (id 'y)) (mtEnv)))
      (closureV '(y x) (add (id 'x) (id 'y)) (mtEnv)))

(test (uncurrying
       (currying (closureV '(x y z) (add (id 'x) (add (id 'y) (id 'z))) (mtEnv))))
      (closureV '(x y z) (add (id 'x) (add (id 'y) (id 'z))) (mtEnv)))
(test (swapping (swapping (closureV '(x y z) (num 0) (mtEnv))))
      (closureV '(x y z) (num 0) (mtEnv)))


;; d)

(test (p-eval (tt)) #t)
(test (p-eval (ff)) #f)
(test/exn (p-eval (p-id 'x)) "p-eval: Free variable.")
(test/exn (p-eval (p-with 'x (ff) (p-and (list (p-id 'y) (p-id 'x))))) "p-eval: Free variable.")
(test/exn (p-eval (p-subst (p-or (list (ff) (p-id 'y))) 'x (tt))) "p-eval: Free variable.")
(test (p-eval (p-and (list (tt) (ff)))) #f)
(test (p-eval (p-or (list (tt) (ff)))) #t)
(test (p-eval (p-not (ff))) #t)
(test (p-eval (p-with 'x (tt) (p-and (list (tt) (p-id 'x))))) #t)
(test (p-eval (p-with 'x (ff) (p-and (list (tt) (p-id 'x))))) #f)
(test (p-eval (p-subst (p-or (list (ff) (p-id 'x))) 'x (tt))) #t)
(test (p-eval (p-or (list (tt) (p-id 'x)))) #t)

#|
Ejercicio 2
|#

;; a)

(test (parser '(fun (x y) (+ x y))) (fun '(x y) (add (id 'x) (id 'y))))
(test (parser '((fun (x y) (+ x y)) (2 3))) 
      (app (fun '(x y) (add (id 'x) (id 'y))) (list (num 2) (num 3))))
(test/exn (parser '(fun () 10)) "parser: Function expects at least one argument.")

;; b)

(test (interp (num 10) (mtEnv)) (numV 10))
(test (interp (add (num 2) (num 3)) (mtEnv)) (numV 5))

(test (interp (mul (num 2) (num 3)) (mtEnv)) (numV 6))
(test (interp (if0 (num 0) (num 1) (num 2)) (mtEnv)) (numV 1))
(test (interp (if0 (num 1) (num 1) (num 2)) (mtEnv)) (numV 2))

(test (interp (id 'x) (extend 'x (numV 3) (mtEnv))) (numV 3))
(test/exn (interp (id 'x) (mtEnv)) "lookup: Variable not found.")

(test (interp (fun '(x) (id 'x)) (mtEnv))
      (closureV '(x) (id 'x) (mtEnv)))

(test (interp (with 'x (num 3) (fun '(y) (id 'y))) (mtEnv)) 
      (closureV '(y) (id 'y) (xtEnv 'x (num 3) (mtEnv))))
(test (interp (with 'x (num 3) (add (id 'x) (num 1))) (mtEnv)) 
      (numV 4))

(test (interp (app (fun '(x y) (add (id 'x) (id 'y)))
		   (list (num 1) (num 2))) (mtEnv))
      (numV 3))

(test/exn (interp (app (fun '(x y) (add (id 'x) (id 'y)))
		   (list (num 1))) (mtEnv))
      "interp: Arity mismatch")

(test (bind-args '(x) (list (numV 1)) (mtEnv))
      (cons '() (xtEnv 'x (numV 1) (mtEnv))))
(test (bind-args '(x y z) (list (numV 1) (numV 2)) (mtEnv))
      (cons '(z) (xtEnv 'y (numV 2) (xtEnv 'x (numV 1) (mtEnv)))))
(test (bind-args '(x y) (list) (mtEnv))
      (cons '(x y) (mtEnv)))

(define id2 (fun '(x y) (add (id 'x) (id 'y))))
(test (apply-closure (closureV '(x y) (add (id 'x) (id 'y)) (mtEnv))
                     (list (numV 1) (numV 2)))
      (numV 3))
(test (apply-closure (closureV '(x y) (add (id 'x) (id 'y)) (mtEnv))
                     (list (numV 1)))
      (closureV '(y) (add (id 'x) (id 'y))
                (xtEnv 'x (numV 1) (mtEnv))))
(test/exn (apply-closure (closureV '(x y) (add (id 'x) (id 'y)) (mtEnv))
                         (list (numV 1) (numV 2) (numV 3)))
          "interp: Arity mismatch")

;; c)

(test (interp (curry* 
		(fun '(x y z) (add (id 'x) (add (id 'y) (id 'z))))) (mtEnv)) 
      (closureV '(x) (fun '(y) (fun '(z) (add (id 'x) (id 'z)))) (mtEnv)))

(test (interp (uncurry* 
		(fun '(x) (fun '(y) (fun '(z) (add (id 'x) (add (id 'y) (id 'z))))))) (mtEnv))
      (closureV '(x y z) (add (id 'x) (add (id 'y) (id 'z))) (mtEnv)))

(test (interp (swap* (fun '(x y) (add (id 'x) (id 'y)))) (mtEnv))
      (closureV '(y x) (add (id 'x) (id 'y)) (mtEnv)))

;; d)

(test (run '((fun (x y) (+ x y)) (4 6))) (numV 10))
(test (run '((fun (x y) (* x y)) (2 3))) (numV 6)) 
(test (run '(if0 ((fun (x y) (- x y)) (5 5)) (6) (1))) (numV 6))
(test (run '(with x (+ 3 1) (- 4 x))) (numV 0))

(require "T2.rkt")

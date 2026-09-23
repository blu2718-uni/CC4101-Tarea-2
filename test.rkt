#lang play

#|
Nombre: Julio Yáñez
|#

(require "T2.rkt")

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
(test (parse-prop '(or false true)) (p-or (list (ff) (tt))))
(test (parse-prop '(or false true true)) (p-or (list (ff) (tt) (tt))))
(test (parse-prop '(with x false (and true x)))
      (p-with 'x (ff) (p-and (list (tt) (p-id 'x)))))
(test (parse-prop '(not x)) (p-not (p-id 'x)))
(test (parse-prop '(and x (not false))) (p-and (list (p-id 'x) (p-not (ff)))))

;; c)

(test (p-subst (p-id 'x) 'x (tt)) (tt))
(test (p-subst (p-id 'x) 'y (tt)) (p-id 'x))
(test (p-subst (tt) 'x (tt)) (tt))
(test (p-subst (ff) 'x (tt)) (ff))
(test (p-subst (p-not (p-id 'x)) 'x (tt)) (p-not (tt)))
(test (p-subst (p-and (list (tt) (p-id 'x))) 'x (ff)) (p-and (list (tt) (ff))))
(test (p-subst (p-or (list (p-id 'x) (p-id 'y))) 'x (ff)) (p-or (list (ff) (p-id 'y))))
(test (p-subst (p-with 'x (ff) (p-and (list (tt) (p-id 'x)))) 'x (tt))
      (p-with 'x (ff) (p-and (list (tt) (p-id 'x)))))
(test (p-subst (p-with 'y (ff) (p-and (list (tt) (p-id 'x)))) 'x (tt))
      (p-with 'y (ff) (p-and (list (tt) (tt)))))
(test (p-subst (p-with 'y (p-id 'x) (p-id 'x)) 'x (tt))
      (p-with 'y (tt) (tt)))
(test (p-subst (p-with 'x (p-id 'x) (p-id 'x)) 'x (tt))
      (p-with 'x (tt) (p-id 'x)))

;; d)

(test (p-eval (tt)) #t)
(test (p-eval (ff)) #f)
(test (p-eval (p-not (tt))) #f)
(test (p-eval (p-not (ff))) #t)
(test/exn (p-eval (p-id 'x)) "p-eval: Free variable.")
(test/exn (p-eval (p-with 'x (ff) (p-and (list (p-id 'y) (p-id 'x))))) "p-eval: Free variable.")
(test/exn (p-eval (p-subst (p-or (list (ff) (p-id 'y))) 'x (tt))) "p-eval: Free variable.")
(test (p-eval (p-and (list (tt) (tt)))) #t)
(test (p-eval (p-and (list (tt) (ff)))) #f)
(test (p-eval (p-or (list (ff) (tt)))) #t)
(test (p-eval (p-or (list (tt) (ff)))) #t)
(test (p-eval (p-or (list (ff) (ff)))) #f)
(test (p-eval (p-and (list (tt) (tt) (ff) (tt)))) #f)
(test (p-eval (p-with 'x (tt) (p-and (list (tt) (p-id 'x))))) #t)
(test (p-eval (p-with 'x (ff) (p-and (list (tt) (p-id 'x))))) #f)
(test (p-eval (p-subst (p-or (list (ff) (p-id 'x))) 'x (tt))) #t)

;; Cortocircuito: si el resto no se evalúa, la variable libre no debiera
;; provocar error (de lo contrario p-eval lanzaría "Free variable").
(test (p-eval (p-or (list (tt) (p-id 'x)))) #t)
(test (p-eval (p-or (list (ff) (tt) (p-id 'x)))) #t)
(test (p-eval (p-and (list (ff) (p-id 'x)))) #f)
(test (p-eval (p-and (list (tt) (ff) (p-id 'x)))) #f)
(test (p-eval (p-and (list (p-or (list (tt) (p-id 'x)))
                           (p-and (list (ff) (p-id 'y)))))) #f)
;; Si el argumento con variable libre sí le toca ser evaluado, falla.
(test/exn (p-eval (p-and (list (tt) (p-id 'x)))) "p-eval: Free variable.")
(test/exn (p-eval (p-or (list (ff) (p-id 'x)))) "p-eval: Free variable.")

#|
Ejercicio 2
|#

;; helpers de valores (pruebas unitarias)

(test (numV+ (numV 2) (numV 3)) (numV 5))
(test/exn (numV+ (numV 2) (closureV '(x) (id 'x) (mtEnv))) "numV+: Wrong operands")
(test (numV* (numV 2) (numV 3)) (numV 6))
(test/exn (numV* (closureV '(x) (id 'x) (mtEnv)) (numV 3)) "numV*: Wrong operands")
(test (is-zero-numV? (numV 0)) #t)
(test (is-zero-numV? (numV 7)) #f)
(test/exn (is-zero-numV? (closureV '(x) (id 'x) (mtEnv))) "is-zero-numV?: Wrong operand")

(test (extend 'x (numV 1) (mtEnv)) (xtEnv 'x (numV 1) (mtEnv)))
(test (extend 'y (numV 2) (xtEnv 'x (numV 1) (mtEnv)))
      (xtEnv 'y (numV 2) (xtEnv 'x (numV 1) (mtEnv))))
(test (lookup 'x (extend 'x (numV 1) (mtEnv))) (numV 1))
(test (lookup 'x (extend 'y (numV 2) (extend 'x (numV 1) (mtEnv)))) (numV 1))
(test/exn (lookup 'x (mtEnv)) "lookup: Variable not found.")

;; a)

(test (parser '(fun (x y) (+ x y))) (fun '(x y) (add (id 'x) (id 'y))))
(test (parser '((fun (x y) (+ x y)) (2 3)))
      (app (fun '(x y) (add (id 'x) (id 'y))) (list (num 2) (num 3))))
(test/exn (parser '(fun () 10)) "parser: Function expects at least one argument.")
(test (parser '(fun (x) x)) (fun '(x) (id 'x)))
(test (parser '(curry* (fun (x y) (+ x y))))
      (curry* (fun '(x y) (add (id 'x) (id 'y)))))
(test (parser '(uncurry* (fun (x) (fun (y) (+ x y)))))
      (uncurry* (fun '(x) (fun '(y) (add (id 'x) (id 'y))))))
(test (parser '(swap* (fun (x y) (+ x y))))
      (swap* (fun '(x y) (add (id 'x) (id 'y)))))

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
      (closureV '(y) (id 'y) (xtEnv 'x (numV 3) (mtEnv))))
(test (interp (with 'x (num 3) (add (id 'x) (num 1))) (mtEnv))
      (numV 4))
(test (interp (with 'x (add (num 1) (num 2)) (mul (id 'x) (id 'x))) (mtEnv))
      (numV 9))

(test (interp (app (fun '(x y) (add (id 'x) (id 'y)))
                   (list (num 1) (num 2))) (mtEnv))
      (numV 3))
(test (interp (app (fun '(x) (id 'x)) (list (num 5))) (mtEnv))
      (numV 5))

;; aplicación parcial: quedan parámetros sin cubrir
(test (interp (app (fun '(x y) (add (id 'x) (id 'y)))
                   (list (num 1))) (mtEnv))
      (closureV '(y) (add (id 'x) (id 'y))
                (xtEnv 'x (numV 1) (mtEnv))))

;; aridad excedida
(test/exn (interp (app (fun '(x) (id 'x))
                       (list (num 1) (num 2))) (mtEnv))
          "interp: Arity mismatch")

(test (bind-args '(x) (list (numV 1)) (mtEnv))
      (cons '() (xtEnv 'x (numV 1) (mtEnv))))
(test (bind-args '(x y z) (list (numV 1) (numV 2)) (mtEnv))
      (cons '(z) (xtEnv 'y (numV 2) (xtEnv 'x (numV 1) (mtEnv)))))
(test (bind-args '(x y) (list) (mtEnv))
      (cons '(x y) (mtEnv)))

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
      (closureV '(x) (num 1) (mtEnv)))
(test (swapping (closureV '(x y) (add (id 'x) (id 'y)) (mtEnv)))
      (closureV '(y x) (add (id 'x) (id 'y)) (mtEnv)))

(test (uncurrying
       (currying (closureV '(x y z) (add (id 'x) (add (id 'y) (id 'z))) (mtEnv))))
      (closureV '(x y z) (add (id 'x) (add (id 'y) (id 'z))) (mtEnv)))
(test (swapping (swapping (closureV '(x y z) (num 0) (mtEnv))))
      (closureV '(x y z) (num 0) (mtEnv)))

(test (interp (curry*
               (fun '(x y z) (add (id 'x) (add (id 'y) (id 'z))))) (mtEnv))
      (closureV '(x)
                (fun '(y) (fun '(z) (add (id 'x) (add (id 'y) (id 'z)))))
                (mtEnv)))

(test (interp (uncurry*
               (fun '(x) (fun '(y) (fun '(z) (add (id 'x) (add (id 'y) (id 'z))))))) (mtEnv))
      (closureV '(x y z) (add (id 'x) (add (id 'y) (id 'z))) (mtEnv)))

(test (interp (swap* (fun '(x y) (add (id 'x) (id 'y)))) (mtEnv))
      (closureV '(y x) (add (id 'x) (id 'y)) (mtEnv)))

;; uso de las transformaciones vía app
(test (interp (app (curry* (fun '(x y) (add (id 'x) (id 'y))))
                   (list (num 1))) (mtEnv))
      (closureV '(y) (add (id 'x) (id 'y))
                (xtEnv 'x (numV 1) (mtEnv))))
(test (interp (app (uncurry* (fun '(x) (fun '(y) (add (id 'x) (id 'y)))))
                   (list (num 1) (num 2))) (mtEnv))
      (numV 3))
;; swap* invierte los argumentos: (if0 x y 0) con (0 5)
;; sin swap: x=0, y=5  -> 5 ; con swap: y=0, x=5 -> 0
(test (interp (app (fun '(x y) (if0 (id 'x) (id 'y) (num 0)))
                   (list (num 0) (num 5))) (mtEnv))
      (numV 5))
(test (interp (app (swap* (fun '(x y) (if0 (id 'x) (id 'y) (num 0))))
                   (list (num 0) (num 5))) (mtEnv))
      (numV 0))

;; d)

(test (run '((fun (x y) (+ x y)) (4 6))) (numV 10))
(test (run '((fun (x y) (* x y)) (2 3))) (numV 6))
(test (run '(if0 ((fun (x y) (+ x y)) (5 -5)) 6 1)) (numV 6))
(test (run '(with x (+ 3 1) (* 2 x))) (numV 8))
(test (run '(curry* (fun (x y) (+ x y))))
      (closureV '(x) (fun '(y) (add (id 'x) (id 'y))) (mtEnv)))
(test (run '(swap* (fun (x y) (+ x y))))
      (closureV '(y x) (add (id 'x) (id 'y)) (mtEnv)))
(test (run '((uncurry* (fun (x) (fun (y) (* x y)))) (3 4))) (numV 12))

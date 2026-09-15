#lang play

#|
Nombre: Julio Yáñez
|#

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
      (p-with 'x (ff) (p-and (list (tt) (p'id 'x)))))
(test (p-subst (p-with 'y (ff) (p-and (list (tt) (p-id 'x)))) 'x (tt)) 
      (p-with 'y (ff) (p-and (list (tt) (tt)))))

;; d)

(test (p-eval (tt)) #t)
(test (p-eval (ff)) #f)
(test/exn (p-eval (p-id 'x)) "p-eval: Free variable.")
(test/exn (p-eval (p-with 'x (ff) (p-and (list 'y 'x)))) "p-eval: Free variable.")
(test/exn (p-eval (p-subst (p-or (list (ff) 'y)) 'x (tt))) "p-eval: Free variable.")
(test (p-eval (p-and (tt) (ff))) #f)
(test (p-eval (p-or (tt) (ff))) #t)
(test (p-eval (p-not (ff))) #t)
(test (p-eval (p-with 'x (tt) (p-and (tt) 'x))) #t)
(test (p-eval (p-with 'x (ff) (p-and (list (tt) 'x)))) #f)
(test (p-eval (p-subst (p-or (list (ff) 'x)) 'x (tt))) #t)
(test (p-eval (p-or (list (tt) 'x))) #t)



(require "T2.rkt")

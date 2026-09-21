#lang play

#|
Nombre: Julio Yáñez
¿Utilizó Whiteboard Policy? (SI o NO): NO
En caso afirmativo, ¿con quién?: _
¿en qué ejercicio(s)?: _
|#

;;------------ ;;
;;==== P1 ==== ;;
;;------------ ;;

#| Parte A |#

#|
<prop> ::= (tt)
         | (ff)
         | (p-id <id>)
         | (p-not <prop>)
         | (p-and (<prop> <prop>+))
         | (p-or (<prop> <prop>+))
         | (p-with <id> <prop> <prop>)
|#

(deftype Prop
  (tt)
  (ff)
  (p-id id)
  (p-not p)
  (p-and p-mult)
  (p-or p-mult)
  (p-with id val p))

#| Parte B |#

#|
<s-prop> ::= true
           | false
           | <id>
	   | (list 'not <s-prop>) 
	   | (list 'and <s-prop> <s-prop>+)
	   | (list 'or <s-prop> <s-prop>+)
	   | (list 'with <id> <s-prop> <s-prop>)
|#

;; parse-prop :: <s-prop> -> Prop
;; Parsea programas escritos en sintaxis concreta a progamas escritos en sintaxis abstracta.
(define (parse-prop p)
  (match p
    [(? symbol? v) #:when (eq? v 'true) (tt)]
    [(? symbol? v) #:when (eq? v 'false) (ff)]
    [(? symbol? x) (p-id x)]
    [(list 'not prop) (p-not (parse-prop prop))]
    [(list 'and props ...) (if (<= 2 (length props))
			       (p-and (map parse-prop props))
			       (error "parse-prop: and expects at least two operands."))]
    [(list 'or props ...) (if (<= 2 (length props))
			      (p-or (map parse-prop  props))
			      (error "parse-prop: or expects at least two operands."))]
    [(list 'with (? symbol? id) value prop) 
     (p-with id (parse-prop value) (parse-prop prop))]
    ))

#| Parte C |#

;; p-subst :: Prop Symbol Prop -> Prop
;; Sustituye un identificador dentro de una expresión.
(define (p-subst expr x val) 
  (match expr
    [(tt) (tt)]
    [(ff) (ff)]
    [(p-id id) (if (eq? x id)
		   val
		   (p-id id))]
    [(p-not p) (p-not (p-subst p x val))]
    [(p-and p-mult) (p-and (map (lambda (p) (p-subst p x val)) p-mult))]
    [(p-or p-mult) (p-or (map (lambda (p) (p-subst p x val)) p-mult))]
    [(p-with id w-val p) (if (eq? x id)
			   (p-with id w-val p)
			   (p-with id w-val (p-subst p x val)))]
   ))

#| Parte D |#

;; p-eval :: Prop -> Boolean
(define (p-eval p)
  (match p
    [(tt) #t]
    [(ff) #f]
    [(p-id id) (error "p-eval: Free variable.")]
    [(p-not p) (not (p-eval p))]
    [(p-and p-mult) (cond
		      [(= (length p-mult) 0) #t]
		      [(not (p-eval (first p-mult))) #f]
		      [(p-eval (first p-mult)) (p-eval (p-and (rest p-mult)))])]
    [(p-or p-mult) (cond
		     [(= (length p-mult) 0) #f]
		     [(p-eval (first p-mult)) #t]
		     [(not (p-eval (first p-mult))) (p-eval (p-and (rest p-mult)))])]
    [(p-with id val p) (p-eval (p-subst p id val))]
    ))



;;------------ ;;
;;==== P2 ==== ;;
;;------------ ;;

#| Parte A y C |#

#|
<expr> ::= <number>
         | (id <id>)
         | (add <expr> <expr>)
         | (mul <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (with <id> <expr> <expr>)
	 | (fun <id>+ <expr>)
	 | (app <expr> <expr>+)
|#

(deftype Expr
  (num n)
  (id x)
  (add l r)
  (mul l r)
  (if0 c t f)
  (with x v exp)
  (fun a exp)
  (app f a)
)

#|
<s-expr> ::= <number>
           | <symbol>
           | (list + <s-expr> <s-expr>)
           | (list * <s-expr> <s-expr>)
           | (list if0 <s-expr> <s-expr> <s-expr>)
           | (list with <symbol> <s-expr> <s-expr>)
           ...
|#

;; parser :: <s-expr> -> Expr
(define (parser expr)
  (match expr
    [x #:when (number? x) (num x)]
    [x #:when (symbol? x) (id x)]
    [(list '+ a b) (add (parser a) (parser b))]
    [(list '* a b) (mul (parser a) (parser b))]
    [(list 'if0 c t f) (if0 (parser c) (parser t) (parser f))]
    [(list 'with x v exp) (with x (parser v) (parser exp))]
    [(list 'fun a exp) (if (>= (length a) 1) 
			   (fun a (parser exp))
			   (error "parser: Function expects at least one argument"))]
    [(list f v) (app (parser f) (map (lambda (e) (parser e)) v))]
    ))

#| Parte B y C|#

(deftype EValue
  (numV n)
  ;...
  )

;; numV+ :: EValue EValue -> EValue
(define (numV+ l r)
  (match (cons l r)
    [(cons (numV x) (numV y)) (numV (+ x y))]
    [_ (error 'numV+ "Wrong operands")]))

;; numV* :: EValue EValue -> EValue
(define (numV* l r)
  (match (cons l r)
    [(cons (numV x) (numV y)) (numV (* x y))]
    [_ (error 'numV* "Wrong operands")]))

;; is-zero-numV? :: EValue -> Boolean
(define (is-zero-numV? x)
  (match x
    [(numV n) (zero? n)]
    [_ (error 'is-zero-numV? "Wrong operand")]))

(deftype Env
  (mtEnv)
  (xtEnv x v p-env))

;; extend :: Symbol EValue Env -> Env
(define (extend x v prev-env)
  (xtEnv x v prev-env))

;; lookup :: Symbol Env -> EValue
(define (lookup x env)
  (match env
    [(mtEnv) (error 'lookup "Variable not found.")]
    [(xtEnv y v prev) (if (equal? x y)
                          v
                          (lookup x prev))]))

;; interp :: Expr Env -> EValue
(define (interp expr env) '???)


#| Parte C |#

;; currying :: EValue -> EValue

;; uncurrying :: EValue -> EValue

;; swapping :: EValue -> EValue


#| Parte D |#

;; run :: <s-expr> -> EValue
(define (run expr) '???)

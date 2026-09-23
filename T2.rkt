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
           | (not <s-prop>)
           | (and <s-prop> <s-prop>+)
           | (or <s-prop> <s-prop>+)
           | (with <id> <s-prop> <s-prop>)
|#

;; parse-prop :: <s-prop> -> Prop
;; Parsea programas escritos en sintaxis concreta a programas escritos en sintaxis abstracta.
(define (parse-prop p)
  (match p
    ['true (tt)]
    ['false (ff)]
    [(? symbol? x) (p-id x)]
    [(list 'not prop) (p-not (parse-prop prop))]
    [(list 'and props ...)
     (if (<= 2 (length props))
         (p-and (map parse-prop props))
         (error "parse-prop: and expects at least two operands."))]
    [(list 'or props ...)
     (if (<= 2 (length props))
         (p-or (map parse-prop props))
         (error "parse-prop: or expects at least two operands."))]
    [(list 'with (? symbol? id) value prop)
     (p-with id (parse-prop value) (parse-prop prop))]))

#| Parte C |#

;; p-subst :: Prop Symbol Prop -> Prop
;; Sustituye todas las ocurrencias libres del identificador por la
;; expresión dada. Si un with redefine el identificador, la sustitución
;; no entra a su cuerpo (pero sí a su valor).
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
                             (p-with id (p-subst w-val x val) p)
                             (p-with id (p-subst w-val x val) (p-subst p x val)))]))

#| Parte D |#

;; p-eval :: Prop -> Boolean
;; Reduce una proposición a un booleano de Racket. Implementa
;; cortocircuito manual en and/or (izquierda a derecha): si un argumento
;; de and es falso (o uno de or es verdadero) el resto no se evalúa.
;; Las variables libres provocan el error "p-eval: Free variable.".
(define (p-eval p)
  (match p
    [(tt) #t]
    [(ff) #f]
    [(p-id id) (error "p-eval: Free variable.")]
    [(p-not p) (not (p-eval p))]
    [(p-and p-mult)
     (let ([v (p-eval (first p-mult))])
       (cond
         [(not v) #f]
         [(null? (rest p-mult)) #t]
         [else (p-eval (p-and (rest p-mult)))]))]
    [(p-or p-mult)
     (let ([v (p-eval (first p-mult))])
       (cond
         [v #t]
         [(null? (rest p-mult)) #f]
         [else (p-eval (p-or (rest p-mult)))]))]
    [(p-with id val p) (p-eval (p-subst p id val))]))



;;------------ ;;
;;==== P2 ==== ;;
;;------------ ;;

#| Parte A y C |#

#|
Expr representa el AST del lenguaje aritmético con funciones de primera
clase: números, identificadores, operaciones aritméticas, condicional,
definiciones locales, funciones y aplicación (más curry*, uncurry* y
swap*).

<expr> ::= (num <number>)
         | (id <id>)
         | (add <expr> <expr>)
         | (mul <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (with <id> <expr> <expr>)
         | (fun (<id>+) <expr>)
         | (app <expr> (<expr>*))
         | (curry* <expr>)
         | (uncurry* <expr>)
         | (swap* <expr>)
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
  (curry* e)
  (uncurry* e)
  (swap* e))

#|
Sintaxis concreta: los programas se escriben como s-expressions de Racket.

<s-expr> ::= <number>
           | <id>
           | (list '+ <s-expr> <s-expr>)
           | (list '* <s-expr> <s-expr>)
           | (list 'if0 <s-expr> <s-expr> <s-expr>)
           | (list 'with <id> <s-expr> <s-expr>)
           | (list 'fun (<id>+) <s-expr>)
           | (<s-expr> (<s-expr>*))
           | (list 'curry* <s-expr>)
           | (list 'uncurry* <s-expr>)
           | (list 'swap* <s-expr>)
|#

;; parser :: <s-expr> -> Expr
;; Transforma un programa en sintaxis concreta a sintaxis abstracta.
(define (parser expr)
  (match expr
    [x #:when (number? x) (num x)]
    [x #:when (symbol? x) (id x)]
    [(list '+ a b) (add (parser a) (parser b))]
    [(list '* a b) (mul (parser a) (parser b))]
    [(list 'if0 c t f) (if0 (parser c) (parser t) (parser f))]
    [(list 'with (? symbol? x) v exp) (with x (parser v) (parser exp))]
    [(list 'fun args exp)
     (if (and (list? args) (andmap symbol? args) (pair? args))
         (fun args (parser exp))
         (error "parser: Function expects at least one argument."))]
    [(list 'curry* e) (curry* (parser e))]
    [(list 'uncurry* e) (uncurry* (parser e))]
    [(list 'swap* e) (swap* (parser e))]
    [(list f v) #:when (list? v) (app (parser f) (map parser v))]))

#| Parte B y C |#

#|
<EValue> ::= (numV <number>)
           | (closureV (<id>*) <expr> <env>)
|#

(deftype EValue
  (numV n)
  (closureV args body env))

;; numV+ :: EValue EValue -> EValue
;; Suma dos valores numéricos.
(define (numV+ l r)
  (match (cons l r)
    [(cons (numV x) (numV y)) (numV (+ x y))]
    [_ (error 'numV+ "Wrong operands")]))

;; numV* :: EValue EValue -> EValue
;; Multiplica dos valores numéricos.
(define (numV* l r)
  (match (cons l r)
    [(cons (numV x) (numV y)) (numV (* x y))]
    [_ (error 'numV* "Wrong operands")]))

;; is-zero-numV? :: EValue -> Boolean
;; Indica si un valor numérico es cero.
(define (is-zero-numV? x)
  (match x
    [(numV n) (zero? n)]
    [_ (error 'is-zero-numV? "Wrong operand")]))

#|
<env> ::= (mtEnv)
        | (xtEnv <id> <EValue> <env>)
|#

(deftype Env
  (mtEnv)
  (xtEnv x v p-env))

;; extend :: Symbol EValue Env -> Env
;; Agrega la asociación de un identificador a un valor sobre un ambiente.
(define (extend x v prev-env)
  (xtEnv x v prev-env))

;; lookup :: Symbol Env -> EValue
;; Busca el valor asociado a un identificador en el ambiente.
(define (lookup x env)
  (match env
    [(mtEnv) (error 'lookup "Variable not found.")]
    [(xtEnv y v prev) (if (equal? x y)
                          v
                          (lookup x prev))]))

;; bind-args :: (listof Symbol) (listof EValue) Env -> (cons (listof Symbol) Env)
;; Asocia cada valor con su parámetro (de izquierda a derecha) en el
;; ambiente, y retorna los parámetros que quedaron sin usar junto al
;; nuevo ambiente.
(define (bind-args params vals env)
  (if (null? vals)
      (cons params env)
      (bind-args (rest params) (rest vals)
                 (extend (first params) (first vals) env))))

;; apply-closure :: EValue (listof EValue) -> EValue
;; Aplica una clausura a sus argumentos. Si sobran parámetros se
;; retorna una nueva clausura (aplicación parcial); si faltan
;; parámetros se lanza el error de aridad.
(define (apply-closure closure values*)
  (match closure
    [(closureV params body fenv)
     (cond
       [(> (length values*) (length params))
        (error 'interp "Arity mismatch")]
       [else
        (def (cons rparams renv) (bind-args params values* fenv))
        (if (null? rparams)
            (interp body renv)
            (closureV rparams body renv))])]))

;; interp :: Expr Env -> EValue
;; Reduce una expresión a su valor.
(define (interp expr env)
  (match expr
    [(num n) (numV n)]
    [(id x) (lookup x env)]
    [(add l r) (numV+ (interp l env) (interp r env))]
    [(mul l r) (numV* (interp l env) (interp r env))]
    [(if0 c t f) (if (is-zero-numV? (interp c env))
                     (interp t env)
                     (interp f env))]
    [(with x v exp) (interp exp (extend x (interp v env) env))]
    [(fun a exp) (closureV a exp env)]
    [(app f a) (apply-closure (interp f env)
                              (map (lambda (e) (interp e env)) a))]
    [(curry* e) (currying (interp e env))]
    [(uncurry* e) (uncurrying (interp e env))]
    [(swap* e) (swapping (interp e env))]))

#| Parte C |#

;; nest :: (listof Symbol) Expr -> Expr
;; Envuelve un cuerpo en un fun por cada parámetro.
(define (nest ps b)
  (if (null? ps)
      b
      (fun (list (first ps)) (nest (rest ps) b))))

;; currying :: EValue -> EValue
;; Individualiza los argumentos de una clausura: produce una clausura
;; de un parámetro cuyo cuerpo es una cadena de fun unarios.
(define (currying val)
  (match val
    [(closureV (cons x xs) body env)
     (if (null? xs)
         val
         (closureV (list x) (nest xs body) env))]))

;; uncurrying :: EValue -> EValue
;; Colapsa una cadena de funciones cuyos parámetros vienen dados de a
;; uno (o de a varios) en una única clausura multi-parámetro.
(define (uncurrying val)
  (match val
    [(closureV params body env)
     (let loop ([ps params] [b body])
       (match b
         [(fun inner-ps inner) (loop (append ps inner-ps) inner)]
         [_ (closureV ps b env)]))]))

;; swapping :: EValue -> EValue
;; Invierte el orden de los parámetros de la clausura.
(define (swapping val)
  (match val
    [(closureV params body env)
     (closureV (reverse params) body env)]))

#| Parte D |#

;; run :: <s-expr> -> EValue
;; Dado un programa bien escrito en sintaxis concreta, retorna su valor.
(define (run expr)
  (interp (parser expr) (mtEnv)))

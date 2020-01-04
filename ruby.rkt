#lang eopl
(require racket/string); string-trim

;; Proyecto FLP: Ruby in Racket
;; 2018-2
;; Fundamentos de Lenguajes de Programación // 750095M // Grupo 01 // Profesor: Jesús Alexandrer Aranda // Monitor: Santiago Giraldo
;; Integrantes:
;; -> Juan Marcos Caicedo Mejía [Código 1730504]
;; -> Santiago Mejía Martínez [Código 1731253]
;; -> Juan Sebastián Díaz Villota [Código 1731637]
;; -> Kevin David Loaiza Giraldo [Código 1730526]

;; Especificación Léxica
(define lexical-spec
'((white-sp (whitespace) skip)
  (comment ("#" (arbno (not #\newline))) skip)
  (identifier ((arbno "@") letter (arbno (or letter digit "_" "?" "=" ))) symbol)
  (number (digit (arbno digit)) number)
  (number ("-" digit (arbno digit)) number)
  (text ("\"" (or letter whitespace)
              (arbno (or letter digit whitespace ":" "?" "=" "'")) "\"") string)
  )
)

;; Gramática
(define grammar-spec
  '( ;;Representa un programa de ruby     
     (ruby-program ("ruby" exp-batch "end") a-program)
     ;; Parte 2: Ruby con objetos
     ;; cambiar a: (ruby-program ("ruby" (arbno class-decl) exp-batch "end") a-program)
     
     ;;Exp-batch: Representa una cerradura de expresiones
     (exp-batch (expression (arbno expression)) a-batch)

     ;;Expresión:
     (expression (simple-exp) a-simple-exp)
     ;Declare-exp: al menos uno o más identificadores (deben inicializarse en 'nil)
     (expression ("declare" identifier (arbno "," identifier) ";") declare-exp)
     ;Puts-exp: al menos un valor compuesto para imprimir
     (expression ("puts" (separated-list comp-value ",") ";") puts-exp)

     (expression ("if" comp-value (arbno "then") exp-batch
                       (arbno "elsif" comp-value (arbno "then") exp-batch)
                       (arbno "else" exp-batch) "end") if-exp)
     
     (expression ("unless" comp-value (arbno "then")
                           exp-batch
                           (arbno "else" exp-batch) "end") unless-exp)

     (expression ("while" comp-value (arbno "do") exp-batch "end") while-exp)
     (expression ("until" comp-value (arbno "do") exp-batch "end") until-exp)

     (expression ("for" identifier "in" comp-value (arbno "do") exp-batch "end") for-exp)

     (expression ("def" identifier "(" (separated-list identifier ",") ")"
                  exp-batch                  
                  "end") function-exp)
     (expression ("return" comp-value ";") return-exp)

     ;;Expresión simple
     (simple-exp (simple-value complement ";") val-exp)
     
     ;;Complemento
     (complement ("=" comp-value calls) assign)
     (complement (assign-op comp-value calls) assign-and)
     (complement (calls) comp-calls)

     ;;Calls
     ;; 0 o muchas llamadas
     (calls ((arbno call)) some-calls)

     ;;Call
     (call (arguments) arguments-call)
     ;; (call ("." identifier arguments) a-method-call) ;; Parte 2: Ruby con Objetos

     ;;Argumentos
     ;; llamar una función puede tener 0 argumentos o muchos
     (arguments ("(" (separated-list comp-value ",") ")") some-arguments)
     ;; almenos 1 argumento para llamar acceder a un elemento en un arreglo
     ;; máximo 2, ejemplo: a=[1,2,3]; a[1] #output 2; a[1,2] #output [2,3];
     ;;                    a[1,2,3] #output Error
     (arguments ("[" comp-value (arbno "," comp-value) "]") arr-arguments)

     ;;Valores compuestos
     (comp-value (value) a-value)
     (comp-value (un-op comp-value) unop-value)
     
     (value (simple-value) a-s-val)
     (value ("(" comp-value val-compl ")") compl-val)

     ;;Complemento para valores
     ;; llamadas a un valor:
     ;; Ejemplo: sirve para ("hola"+(mundo())) donde mundo() retorna "mundo"
     (val-compl (calls) val-call) 
     ;; operacion inorden con otro valor
     (val-compl (bin-op comp-value) binop-val)

     ;; Valores simples
     (simple-value (identifier) id-val)
     (simple-value (number) int-val)
     (simple-value (text) str-val) ;; recordar hacer string-trim cuando se evalue
     (simple-value ("true") true-val)
     (simple-value ("false") false-val)
     (simple-value ("nil") nil-val)
     ;; arreglo con 0 o muchos valores
     (simple-value ("["(separated-list comp-value ",")"]") arr-val)
     
     ;;Operacion Inorden
     (bin-op ("+") add)
     (bin-op ("-") diff)
     (bin-op ("*") mult)
     (bin-op ("/") div)
     (bin-op ("%") mod)
     (bin-op ("**") pow)
     (bin-op (">") great)
     (bin-op (">=") great-eq)
     (bin-op ("<") less)
     (bin-op ("<=") less-eq)
     (bin-op ("==") equal)
     (bin-op ("!=") not-equal)
     (bin-op ("and") and-op)
     (bin-op ("&&") and-op)
     (bin-op ("or") or-op)
     (bin-op ("||") or-op)
     ;;Rangos:
     ;; Solo admite 2 argumentos, no se puede operar más de 1 vez
     ;;Inclusivo: va hasta el limite superior
     (bin-op ("..") in-range)
     ;;Exclusivo: va hasta un step antes del limite superior
     (bin-op ("...") ex-range)
     ;; Ejemplo: (1..5) => (1 2 3 4 5)
     ;; Ejemplo: (1...5) => (1 2 3 4)
     ;; Ejemplo: ((1..5) .. 6) => Error
     (bin-op ("step") st-range)
     ;; Ejemplo: ((1..5) step 2) => (1 3 5)
     ;; Ejemplo: ((1..5) step -1) => Error
     ;; Ejemplo: ((-1..-5) step -2) => (-1 -3 -5)
     ;; Ejemplo: ((1..-5) step 2) => Error
     
     ;;Operación asignación
     (assign-op ("+=") add-eq)
     (assign-op ("-=") diff-eq)
     (assign-op ("*=") mult-eq)
     (assign-op ("/=") div-eq)
     (assign-op ("**=") pow-eq)

     ;;Operación unitaria
     (un-op ("not") not-op)
     (un-op ("!") not-op)

     ;;##############################################
     ;; Parte 2: Ruby con objetos
     ;(class-decl ("class" identifier
     ;                     (arbno "<" identifier)
     ;                     "attr" (separated-list ":" identifier ",") ";"
     ;                     (arbno method-decl) "end") a-class-decl)

     ;(method-decl ("def" identifier "(" (separated-list identifier ",") ")"
     ;             exp-batch                  
     ;             "end") a-method-decl)

  )
)

;Construidos automáticamente:


(sllgen:make-define-datatypes lexical-spec grammar-spec)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexical-spec grammar-spec)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser lexical-spec grammar-spec))

;El Analizador Léxico (Scanner)

(define scan
  (sllgen:make-string-scanner lexical-spec grammar-spec))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program pgm)) 
    (sllgen:make-stream-parser 
      lexical-spec
      grammar-spec)))

;*******************************************************************************************
;*******************************************************************************************
;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body exp-batch?)
   (env environment?)
   ))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args env)
    (cases procval proc
      (closure (ids body env)
               (eval-exp-batch body (extend-env ids args env))
               )
      )
    )
  )
;*******************************************************************************************

;; eval-program: Recibe un programa de Ruby y pasa el batch a eval-exp-batch para que sea evaluado
(define (eval-program pgm)
  (cases ruby-program pgm
    (a-program (a-batch) (eval-exp-batch a-batch (empty-env)))
    )
  )

;; eval-exp-batch: Recibe un batch de Ruby y lo pasa a eval-expressions para que evalúe las expressiones
(define (eval-exp-batch batch env)
  (cases exp-batch batch
    (a-batch (exp exps) (eval-expressions exp exps env))
    )
  )

;; eval-expressions: el programa principal que recibe una expresión y una lista de cero o más expresiones, lo que hace
;; es basándose en los casos de expression evalúa y ejecuta lo que corresponde a cada caso
(define (eval-expressions exp exps env)
(cases expression exp
  (puts-exp (vals)
            (if (eq? exps '())
                (begin (for-each (lambda (arg)
                        (eopl:printf "~a\n" arg))
                      (eval-multiple-comp-values vals env)) '=>nil)
                (begin (for-each (lambda (arg)
                        (eopl:printf "~a\n" arg))
                      (eval-multiple-comp-values vals env))
                       (eval-expressions (car exps) (cdr exps) env)
                       '=>nil)))

  (if-exp (c-value exp-batch1 lst-c-vals1 lst-exp-batchs1 lst-exp-batchs2)
          (if (and (eq? lst-c-vals1 '()) (eq? lst-exp-batchs1 '()))
              (if (eq? (eval-comp-value c-value env) #t)
                  (eval-exp-batch exp-batch1 env)
                  (if (not (eq? lst-exp-batchs2 '()))
                      (eval-exp-batch (car lst-exp-batchs2) env)
                      (eval-expressions (car exps) (cdr exps) env)))
              (cond
                [(eq? (eval-comp-value c-value env) #t) (eval-exp-batch exp-batch1 env)]
                [(search #t (eval-multiple-comp-values lst-c-vals1 env)) (eval-exp-batch (list-ref lst-exp-batchs1 (where-is? (eval-multiple-comp-values lst-c-vals1 env) #t)) env)]
                [else (eval-exp-batch (car lst-exp-batchs2) env)])))

  (unless-exp (c-value exp-batch lst-exp-batchs)
              (if (eq? (eval-comp-value c-value env) #f)
                  (eval-exp-batch exp-batch env)
                  (eval-exp-batch (car lst-exp-batchs) env)))

  (while-exp (comp-value exp-batch)
             (eval-while comp-value exp-batch env exps))

  (until-exp (comp-value exp-batch)
             (eval-until comp-value exp-batch env exps))
  
  (declare-exp (id ids) (if (eq? exps '())
                            (if (eq? ids '())
                            (extend-env (cons id '()) (copy-aux 1 'nil) env)
                            (extend-env (cons id ids) (copy-aux (+ 1 (length ids)) 'nil) env))
                            (if (eq? ids '())
                            (eval-expressions (car exps) (cdr exps) (extend-env (cons id '()) (copy-aux 1 'nil) env))
                            (eval-expressions (car exps) (cdr exps) (extend-env (cons id ids) (copy-aux (+ 1 (length ids)) 'nil) env)))))
  
  (a-simple-exp (simpl-exp) (if (eq? exps '())
                                (eval-simple-exp simpl-exp env)
                                (begin (eval-simple-exp simpl-exp env)
                                       (eval-expressions (car exps) (cdr exps) env))))
  
  (for-exp (id c-value e-batch)
           (for-each (lambda (val) (eval-exp-batch e-batch (extend-env (list id) (list val) env)))
                     (eval-comp-value c-value env)))
  
  
  (function-exp (id-of-proc args body)
                (if (eq? exps '())
                    (let ((name-of-function id-of-proc))
                      name-of-function)
                    (eval-expressions (car exps) (cdr exps) (extend-env-recursively (list id-of-proc) (list args) (list body) env))
                    ))
  
  (return-exp (comp-value)
              (let ((value-to-return
                     (eval-comp-value comp-value env)
                     ))
                value-to-return)
              )
  
  (else "TO DO")
  
  
  
  ))


;; Funciones auxiliares 1er nivel: son las funciones auxiliares de las que se sirve el eval-expressions y otras funciones
;; de este mismo nivel para poder evaluar las expresiones de manera correcta.


;; eval-while: Su tarea es evaluar un while, si la condición del while es falsa, simplemente sigue ejecutando las
;; expresiones que siguen (lo ignora). Si es verdadera, evalua la exp-batch dentro del while y vuelve a evaluar el while
;; con el nuevo ambiente creado a partir de la primera ejecución del exp-batch (revisa permanentemente la condición
;; del while, en el momento que se vuelva falsa, pasa a seguir con el resto del programa).
(define eval-while
  (lambda (c-value exp-batch env exps)
    (if (eq? (eval-comp-value c-value env) #f)
        (if (eq? exps '())
            (begin (eopl:printf ""))
            (eval-expressions (car exps) (cdr exps) env))
        (begin (eval-exp-batch exp-batch env)
               (eval-while c-value exp-batch env exps)))))

;; eval-until: Similar a eval-while solo que su caso base es que la condición que le entra sea falsa (si es verdadera
;; ignora el until y sigue con el programa, si es falsa, lo ejecuta y hace un proceso similar a eval-while)
(define eval-until
  (lambda (c-value exp-batch env exps)
    (if (eq? (eval-comp-value c-value env) #t)
        (if (eq? exps '())
            (begin (eopl:printf ""))
            (eval-expressions (car exps) (cdr exps) env))
        (begin (eval-exp-batch exp-batch env)
               (eval-until c-value exp-batch env exps)))))

;; eval-comp-value-arguments: Dado los casos de lista de argumentos para función o lista de argumentos para
;; acceder a array, coge la lista de argumentos y aplica la función eval-multiple-comp-values que lo que
;; hace es aplicar eval-comp-value (evaluar valor compuesto) a todos los elementos de la lista
(define eval-comp-value-arguments
  (lambda (args env)
    (cases arguments args
      (some-arguments (lst-args) (eval-multiple-comp-values lst-args env))
      (arr-arguments (arr-arg lst-arr-args) (eval-multiple-comp-values (cons arr-arg lst-arr-args) env)))))

;; eval-simple-exp: Evalua una simple-expression en su único caso; val-exp. Esta función le pasa el simple-value
;; y el complemento a la funcinó eval-complement.
(define eval-simple-exp
  (lambda (s-exp env)
    (cases simple-exp s-exp
      (val-exp (s-value complem) (eval-complement s-value complem env)))))

;; eval-complement: Tal vez la función auxiliar de 1er nivel más extensa de todo el proyecto, ya que maneja los 3 casos
;; de complement: assign, assign-and y comp-calls. Todos estos 3 casos manejan los calls, entonces por eso es
;; una función importante y compleja. En el primer caso de assign, de ser la lista de calls vacía, simplemente
;; realiza la asignación tal cual se trabaja en el interpretador de asignación al valor compuesto que tiene a la derecha.
;; Si la lista de calls no es vacía, realiza el proceso de averiguar si el comp-value que estaba a la izquiera de
;; la lista de calls corresponde a un identificador que hace referencia a una función definida previamente,
;; si es este caso, el valor a asignar será lo que retorne el llamado a la función con los argumentos proveidos
;; en el primer call de la lista de calls. Si se trataba de un arreglo, entonces busca un arreglo asociado con el
;; identificador del comp-value y el valor que devuelva el llamado con el arreglo, será asignado. Esta dinámica
;; es similar a los procesos de assign-and que lo que hacen es también aplicar los operadores correspondientes
;; a assign-op (+, -, *, /, **). En el caso de comp-calls revisa si el identificador asociado al simple-value que
;; antecede al complemento hace referencia a algún array o función, y realiza la debida invocación al igual que en
;; los casos anteriores. Cabe resaltar que los llamados solo funcionan si los identificadores que le anteceden
;; hacen referencia a funciones o arrays definidos previamente. Si no, lanzan error (si son numeros, booleanos, etc.).
(define eval-complement
  (lambda (s-value complem env)
    (cases complement complem
      (assign (comp-value calls) (if (eq? (eval-calls calls) '())
                                     (begin
                                       (setref!
                                        (apply-env-ref env (just-id s-value env))
                                        (eval-comp-value comp-value env))
                                       1)
                                     (begin
                                       (setref!
                                        (apply-env-ref env (just-id s-value env))
                                        (let ((proc (eval-comp-value comp-value env))
                                              (args (pass-it (car (eval-calls calls)) env))
                                              (long (length (eval-calls calls))))
                                          (if (or (number? proc) (boolean? proc) (string? proc))
                                              (eopl:error "Can't apply args to ~a")
                                              (if (list? proc)
                                                  (cond [(= long 1) (access-to-array proc args)]
                                                        [(= long 2) (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env))]
                                                        [(= long 3) (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env))]
                                                        [(= long 4) (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env))] 
                                                        [(= long 5) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env))]
                                                        [(= long 6) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env))]
                                                        [(= long 7) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env))]
                                                        [(= long 8) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env)) (pass-it (cadddr (cddddr (eval-calls calls))) env))]
                                                        [(= long 9) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env)) (pass-it (cadddr (cddddr (eval-calls calls))) env)) (pass-it (car (cddddr (cddddr (eval-calls calls)))) env))]
                                                        [(= long 10) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env)) (pass-it (cadddr (cddddr (eval-calls calls))) env)) (pass-it (car (cddddr (cddddr (eval-calls calls)))) env)) (pass-it (cadr (cddddr (cddddr (eval-calls calls)))) env))]
                                                        )
                                                  (if (procval? proc)
                                                      (apply-procedure proc args env)
                                                      (eopl:error 'eval-expression
                                                                  "Attempt to apply non-procedure ~s" proc))))))
                                       1))) 
      (assign-and (assignn-op comp-value calls)
                  (cases assign-op assignn-op
                    (add-eq () (if (eq? (eval-calls calls) '())
                                     (begin
                                       (setref!
                                        (apply-env-ref env (just-id s-value env))
                                        (+ (eval-simple-value s-value env) (eval-comp-value comp-value env)))
                                       1)
                                     (begin
                                       (setref!
                                        (apply-env-ref env (just-id s-value env))
                                        (+ (eval-simple-value s-value env) (let ((proc (eval-comp-value comp-value env))
                                                                                    (args (pass-it (car (eval-calls calls)) env))
                                                                                    (long (length (eval-calls calls))))
                                                                             (if (or (number? proc) (boolean? proc) (string? proc))
                                                                                 (eopl:error "Can't apply args to ~a")
                                                                                 (if (list? proc)
                                                                                     (cond [(= long 1) (access-to-array proc args)]
                                                                                           [(= long 2) (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env))]
                                                                                           [(= long 3) (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env))]
                                                                                           [(= long 4) (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env))]
                                                                                           [(= long 5) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env))]
                                                                                           [(= long 6) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env))]
                                                                                           [(= long 7) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env))]
                                                                                           [(= long 8) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env)) (pass-it (cadddr (cddddr (eval-calls calls))) env))]
                                                                                           [(= long 9) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env)) (pass-it (cadddr (cddddr (eval-calls calls))) env)) (pass-it (car (cddddr (cddddr (eval-calls calls)))) env))]
                                                                                           [(= long 10) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env)) (pass-it (cadddr (cddddr (eval-calls calls))) env)) (pass-it (car (cddddr (cddddr (eval-calls calls)))) env)) (pass-it (cadr (cddddr (cddddr (eval-calls calls)))) env))])
                                                                                     (if (procval? proc)
                                                                                         (apply-procedure proc args env)
                                                                                         (eopl:error 'eval-expression
                                                                                                     "Attempt to apply non-procedure ~s" proc)))))))
                                       1))) 
                    (diff-eq () (if (eq? (eval-calls calls) '())
                                     (begin
                                       (setref!
                                        (apply-env-ref env (just-id s-value env))
                                        (- (eval-simple-value s-value env) (eval-comp-value comp-value env)))
                                       1)
                                     (begin
                                       (setref!
                                        (apply-env-ref env (just-id s-value env))
                                        (- (eval-simple-value s-value env) (let ((proc (eval-comp-value comp-value env))
                                                                                    (args (pass-it (car (eval-calls calls)) env))
                                                                                    (long (length (eval-calls calls))))
                                                                             (if (or (number? proc) (boolean? proc) (string? proc))
                                                                                 (eopl:error "Can't apply args to ~a")
                                                                                 (if (list? proc)
                                                                                     (cond [(= long 1) (access-to-array proc args)]
                                                                                           [(= long 2) (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env))]
                                                                                           [(= long 3) (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env))]
                                                                                           [(= long 4) (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env))]
                                                                                           [(= long 5) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env))]
                                                                                           [(= long 6) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env))]
                                                                                           [(= long 7) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env))]
                                                                                           [(= long 8) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env)) (pass-it (cadddr (cddddr (eval-calls calls))) env))]
                                                                                           [(= long 9) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env)) (pass-it (cadddr (cddddr (eval-calls calls))) env)) (pass-it (car (cddddr (cddddr (eval-calls calls)))) env))]
                                                                                           [(= long 10) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env)) (pass-it (cadddr (cddddr (eval-calls calls))) env)) (pass-it (car (cddddr (cddddr (eval-calls calls)))) env)) (pass-it (cadr (cddddr (cddddr (eval-calls calls)))) env))]
                                                                                           )
                                                                                     (if (procval? proc)
                                                                                         (apply-procedure proc args env)
                                                                                         (eopl:error 'eval-expression
                                                                                                     "Attempt to apply non-procedure ~s" proc)))))))
                                       1))) 
                    (mult-eq () (if (eq? (eval-calls calls) '())
                                     (begin
                                       (setref!
                                        (apply-env-ref env (just-id s-value env))
                                        (* (eval-simple-value s-value env) (eval-comp-value comp-value env)))
                                       1)
                                     (begin
                                       (setref!
                                        (apply-env-ref env (just-id s-value env))
                                        (* (eval-simple-value s-value env) (let ((proc (eval-comp-value comp-value env))
                                                                                    (args (pass-it (car (eval-calls calls)) env))
                                                                                    (long (length (eval-calls calls))))
                                                                             (if (or (number? proc) (boolean? proc) (string? proc))
                                                                                 (eopl:error "Can't apply args to ~a")
                                                                                 (if (list? proc)
                                                                                     (cond [(= long 1) (access-to-array proc args)]
                                                                                           [(= long 2) (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env))]
                                                                                           [(= long 3) (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env))]
                                                                                           [(= long 4) (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env))]
                                                                                           [(= long 5) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env))]
                                                                                           [(= long 6) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env))]
                                                                                           [(= long 7) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env))]
                                                                                           [(= long 8) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env)) (pass-it (cadddr (cddddr (eval-calls calls))) env))]
                                                                                           [(= long 9) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env)) (pass-it (cadddr (cddddr (eval-calls calls))) env)) (pass-it (car (cddddr (cddddr (eval-calls calls)))) env))]
                                                                                           [(= long 10) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env)) (pass-it (cadddr (cddddr (eval-calls calls))) env)) (pass-it (car (cddddr (cddddr (eval-calls calls)))) env)) (pass-it (cadr (cddddr (cddddr (eval-calls calls)))) env))]
                                                                                           )
                                                                                     (if (procval? proc)
                                                                                         (apply-procedure proc args env)
                                                                                         (eopl:error 'eval-expression
                                                                                                     "Attempt to apply non-procedure ~s" proc)))))))
                                       1))) 
                    (div-eq () (if (eq? (eval-calls calls) '())
                                     (begin
                                       (setref!
                                        (apply-env-ref env (just-id s-value env))
                                        (/ (eval-simple-value s-value env) (eval-comp-value comp-value env)))
                                       1)
                                     (begin
                                       (setref!
                                        (apply-env-ref env (just-id s-value env))
                                        (/ (eval-simple-value s-value env) (let ((proc (eval-comp-value comp-value env))
                                                                                    (args (pass-it (car (eval-calls calls)) env))
                                                                                    (long (length (eval-calls calls))))
                                                                             (if (or (number? proc) (boolean? proc) (string? proc))
                                                                                 (eopl:error "Can't apply args to ~a")
                                                                                 (if (list? proc)
                                                                                     (cond [(= long 1) (access-to-array proc args)]
                                                                                           [(= long 2) (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env))]
                                                                                           [(= long 3) (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env))]
                                                                                           [(= long 4) (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env))]
                                                                                           [(= long 5) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env))]
                                                                                           [(= long 6) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env))]
                                                                                           [(= long 7) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env))]
                                                                                           [(= long 8) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env)) (pass-it (cadddr (cddddr (eval-calls calls))) env))]
                                                                                           [(= long 9) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env)) (pass-it (cadddr (cddddr (eval-calls calls))) env)) (pass-it (car (cddddr (cddddr (eval-calls calls)))) env))]
                                                                                           [(= long 10) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env)) (pass-it (cadddr (cddddr (eval-calls calls))) env)) (pass-it (car (cddddr (cddddr (eval-calls calls)))) env)) (pass-it (cadr (cddddr (cddddr (eval-calls calls)))) env))]
                                                                                           )
                                                                                     (if (procval? proc)
                                                                                         (apply-procedure proc args env)
                                                                                         (eopl:error 'eval-expression
                                                                                                     "Attempt to apply non-procedure ~s" proc)))))))
                                        1))) 
                    (pow-eq () (if (eq? (eval-calls calls) '())
                                     (begin
                                       (setref!
                                        (apply-env-ref env (just-id s-value env))
                                        (expt (eval-simple-value s-value env) (eval-comp-value comp-value env)))
                                       1)
                                     (begin
                                       (setref!
                                        (apply-env-ref env (just-id s-value env))
                                        (expt (eval-simple-value s-value env) (let ((proc (eval-comp-value comp-value env))
                                                                                    (args (pass-it (car (eval-calls calls)) env))
                                                                                    (long (length (eval-calls calls))))
                                                                                (if (or (number? proc) (boolean? proc) (string? proc))
                                                                                    (eopl:error "Can't apply args to ~a")
                                                                                    (if (list? proc)
                                                                                        (cond [(= long 1) (access-to-array proc args)]
                                                                                              [(= long 2) (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env))]
                                                                                              [(= long 3) (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env))]
                                                                                              [(= long 4) (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env))]
                                                                                              [(= long 5) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env))]
                                                                                              [(= long 6) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env))]
                                                                                              [(= long 7) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env))]
                                                                                              [(= long 8) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env)) (pass-it (cadddr (cddddr (eval-calls calls))) env))]
                                                                                              [(= long 9) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env)) (pass-it (cadddr (cddddr (eval-calls calls))) env)) (pass-it (car (cddddr (cddddr (eval-calls calls)))) env))]
                                                                                              [(= long 10) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env)) (pass-it (cadddr (cddddr (eval-calls calls))) env)) (pass-it (car (cddddr (cddddr (eval-calls calls)))) env)) (pass-it (cadr (cddddr (cddddr (eval-calls calls)))) env))]
                                                                                              )
                                                                                        (if (procval? proc)
                                                                                            (apply-procedure proc args env)
                                                                                            (eopl:error 'eval-expression
                                                                                                        "Attempt to apply non-procedure ~s" proc)))))))
                                        1))))) 
      (comp-calls (calls) (let ((proc (eval-simple-value s-value env))
                                (args (pass-it (car (eval-calls calls)) env))
                                (long (length (eval-calls calls))))
                            (if (or (number? proc) (boolean? proc) (string? proc))
                                (eopl:error "Can't apply args to ~a")
                                (if (list? proc)
                                    (cond [(= long 1) (access-to-array proc args)]
                                          [(= long 2) (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env))]
                                          [(= long 3) (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env))]
                                          [(= long 4) (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env))]
                                          [(= long 5) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env))]
                                          [(= long 6) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env))]
                                          [(= long 7) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env))]
                                          [(= long 8) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env)) (pass-it (cadddr (cddddr (eval-calls calls))) env))]
                                          [(= long 9) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env)) (pass-it (cadddr (cddddr (eval-calls calls))) env)) (pass-it (car (cddddr (cddddr (eval-calls calls)))) env))]
                                          [(= long 10) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env)) (pass-it (cadddr (cddddr (eval-calls calls))) env)) (pass-it (car (cddddr (cddddr (eval-calls calls)))) env)) (pass-it (cadr (cddddr (cddddr (eval-calls calls)))) env))]
                                          )
                                    (if (procval? proc)
                                        (apply-procedure proc args env)
                                        (eopl:error 'eval-expression
                                                    "Attempt to apply non-procedure ~s" proc)))))))))

;; eval-calls: Dada una lista de llamados (calls), la retorna.
(define eval-calls
  (lambda (s-calls)
    (cases calls s-calls
      (some-calls (lst-calls) lst-calls))))

;; eval-multiple-comp-values: Aplica map a la lista de valores entrantes, con la función de eval-comp-value.
;; Es decir, evalua todos los valores compuestos de la lista.
(define eval-multiple-comp-values
  (lambda (vals env)
    (map (lambda (x) (eval-comp-value x env)) vals)))

;; eval-comp-value: Evalua el valor compuesto que recibe. Si se trata de un value, lo pasa a eval-value.
;; Si se trata del caso unop-value, que recibe un valor compuesto y un operador unario, el valor compuesto
;; que le debe estar entrando debe retornar booleano, asi que lo evalua (llamado recursivo) y segun el
;; booleano que sea, lo niega (invierte).
(define eval-comp-value
  (lambda (c-value env)
    (cases comp-value c-value
      (a-value (value) (eval-value value env))
      (unop-value (un-op comp-value) (cond [(eq? (eval-comp-value comp-value env) #t) #f]
                                           [(eq? (eval-comp-value comp-value env) #f) #t])))))

;; eval-value: Evalua un value. Si es un simple-value, se lo pasa a eval-simple-value.
;; Si es un valor complementario (val-compl) lo pasa a eval-val-compl con el comp-value que recibe evaluado.
(define eval-value
  (lambda (a-val env)
    (cases value a-val
      (a-s-val (simple-value) (eval-simple-value simple-value env))
      (compl-val (comp-value val-compl) (eval-val-compl (eval-comp-value comp-value env) val-compl env)))))


;; eval-val-compl: Tiene el caso de val-call y de binop-val. Para el caso de val-calls, aplica la misma evaluación
;; de llamados que se vio en eval-complement. Para el caso de binop-val, llama a eval-binop con a-val sin evaluar
;; (porque debe venir ya evaluado) y con el comp-value a la derecha evaluado.
(define eval-val-compl
  (lambda (a-val a-v-compl env)
    (cases val-compl a-v-compl
      (val-call (calls) (let ((proc a-val)
                              (args (pass-it (car (eval-calls calls)) env))
                              (long (length (eval-calls calls))))
                          (if (or (number? proc) (boolean? proc) (string? proc))
                              (eopl:error "Can't apply args to ~a")
                              (if (list? proc)
                                  (cond [(= long 1) (access-to-array proc args)]
                                        [(= long 2) (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env))]
                                        [(= long 3) (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env))]
                                        [(= long 4) (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env))]
                                        [(= long 5) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env))]
                                        [(= long 6) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env))]
                                        [(= long 7) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env))]
                                        [(= long 8) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env)) (pass-it (cadddr (cddddr (eval-calls calls))) env))]
                                        [(= long 9) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env)) (pass-it (cadddr (cddddr (eval-calls calls))) env)) (pass-it (car (cddddr (cddddr (eval-calls calls)))) env))]
                                        [(= long 10) (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array (access-to-array proc args) (pass-it (cadr (eval-calls calls)) env)) (pass-it (caddr (eval-calls calls)) env)) (pass-it (cadddr (eval-calls calls)) env)) (pass-it (car (cddddr (eval-calls calls))) env)) (pass-it (cadr (cddddr (eval-calls calls))) env)) (pass-it (caddr (cddddr (eval-calls calls))) env)) (pass-it (cadddr (cddddr (eval-calls calls))) env)) (pass-it (car (cddddr (cddddr (eval-calls calls)))) env)) (pass-it (cadr (cddddr (cddddr (eval-calls calls)))) env))]
                                        )
                                  (if (procval? proc)
                                      (apply-procedure proc args env)
                                      (eopl:error 'eval-expression
                                                  "Attempt to apply non-procedure ~s" proc)))))) 
      (binop-val (bin-op comp-value) (eval-binop bin-op a-val (eval-comp-value comp-value env))))))

;; eval-simple-value: Es por decirlo así el caso base de los valores permitidos en el interpretador,
;; dado sobre todo en parte por la gramática y cubre los casos de id, si es id busca en el ambiente
;; a que lleva referencia, si es un numero simplemente lo devuelve, si es string lo devuelve
;; (primero aplicando string-trim para arreglar los backslash), si es true-val devuelve el #t de Racket,
;; si es false-val devuelve el #f de Racket, si es nil-val simplemente devuelve el char 'nil, y si
;; es arr-val aplica eval-multiple-comp-values a la lista de comp-values dentro de los corchetes.
(define eval-simple-value
  (lambda (s-val env)
    (cases simple-value s-val
      (id-val (id) (apply-env env id))
      (int-val (datum) datum)
      (str-val (str) (string-trim str "\""))
      (true-val () #t)
      (false-val () #f)
      (nil-val () 'nil)
      (arr-val (vals) (eval-multiple-comp-values vals env)))))

;; eval-binop: También una función importante del interpretador, pues nos brinda las operaciones primitivas
;; del Ruby implementado. Posee todos los casos del bin-op y sus respectivos errores tanto específicos
;; como genéricos, y un cada tipo adicionalmente tiene un error genérico si no cumple con las condiciones
;; necesarias iniciales y no es ningún error específico.
(define eval-binop
  (lambda (binop val1 val2)
    (cases bin-op binop
      (add () (cond [(and (number? val1) (number? val2)) (+ val1 val2)]
                    [(and (string? val1) (string? val2)) (string-append val1 val2)]
                    [(and (list? val1) (list? val2)) (append val1 val2)]
                    [(and (string? val1) (number? val2)) (eopl:error 'eval-binop "No implicit conversion of Integer into String")]
                    [(and (number? val1) (string? val2)) (eopl:error 'eval-binop "String can't be coerced into Integer")]
                    [(and (boolean? val1) (number? val2)) (eopl:error 'eval-binop "Not defined method <bin-op> for boolean")]
                    [(and (number? val1) (boolean? val2)) (eopl:error 'eval-binop "Boolean can't be coerced into Integer")]
                    [(and (number? val1) (list? val2)) (eopl:error 'eval-binop "Array can't be coerced into Integer")]
                    [(and (list? val1) (number? val2)) (eopl:error 'eval-binop "No implicit conversion of Integer into Array")]
                    [else (eopl:error 'eval-binop "There was an error (add error)")]))
      (diff () (cond [(and (number? val1) (number? val2)) (- val1 val2)]
                     [(and (boolean? val1) (number? val2)) (eopl:error 'eval-binop "Not defined method <bin-op> for boolean")]
                     [(and (number? val1) (boolean? val2)) (eopl:error 'eval-binop "Boolean can't be coerced into Integer")]
                     [(and (number? val1) (list? val2)) (eopl:error 'eval-binop "Array can't be coerced into Integer")]
                     [(and (list? val1) (number? val2)) (eopl:error 'eval-binop "No implicit conversion of Integer into Array")]
                     [else (eopl:error 'eval-binop "There was an error (diff error)")]))
      (mult () (cond [(and (number? val1) (number? val2)) (* val1 val2)]
                    [(and (string? val1) (number? val2)) (string-mult-aux val1 val2)]
                    [(and (list? val1) (number? val2)) (lst-mult-aux val1 val2)]
                    [(and (boolean? val1) (number? val2)) (eopl:error 'eval-binop "Not defined method <bin-op> for boolean")]
                    [(and (number? val1) (boolean? val2)) (eopl:error 'eval-binop "Boolean can't be coerced into Integer")]
                    [(and (number? val1) (list? val2)) (eopl:error 'eval-binop "Array can't be coerced into Integer")]
                    [else (eopl:error 'eval-binop "There was an error (mult error)")]
                    ))
      (div () (cond [(and (number? val1) (number? val2)) (/ val1 val2)]
                    [(and (string? val1) (number? val2)) (eopl:error 'eval-binop "No implicit conversion of Integer into String")]
                    [(and (number? val1) (string? val2)) (eopl:error 'eval-binop "String can't be coerced into Integer")]
                    [(and (boolean? val1) (number? val2)) (eopl:error 'eval-binop "Not defined method <bin-op> for boolean")]
                    [(and (number? val1) (boolean? val2)) (eopl:error 'eval-binop "Boolean can't be coerced into Integer")]
                    [(and (number? val1) (list? val2)) (eopl:error 'eval-binop "Array can't be coerced into Integer")]
                    [(and (list? val1) (number? val2)) (eopl:error 'eval-binop "No implicit conversion of Integer into Array")]
                    [else (eopl:error 'eval-binop "There was an error (div error)")]))
      (mod () (cond [(and (number? val1) (number? val2)) (modulo val1 val2)]
                    [(and (boolean? val1) (number? val2)) (eopl:error 'eval-binop "Not defined method <bin-op> for boolean")]
                    [(and (number? val1) (boolean? val2)) (eopl:error 'eval-binop "Boolean can't be coerced into Integer")]
                    [(and (number? val1) (list? val2)) (eopl:error 'eval-binop "Array can't be coerced into Integer")]
                    [(and (list? val1) (number? val2)) (eopl:error 'eval-binop "No implicit conversion of Integer into Array")]
                    [else (eopl:error 'eval-binop "There was an error (mod error)")]))
      (pow () (cond [(and (number? val1) (number? val2)) (expt val1 val2)]
                    [(and (boolean? val1) (number? val2)) (eopl:error 'eval-binop "Not defined method <bin-op> for boolean")]
                    [(and (number? val1) (boolean? val2)) (eopl:error 'eval-binop "Boolean can't be coerced into Integer")]
                    [(and (number? val1) (list? val2)) (eopl:error 'eval-binop "Array can't be coerced into Integer")]
                    [(and (list? val1) (number? val2)) (eopl:error 'eval-binop "No implicit conversion of Integer into Array")]
                    [else (eopl:error 'eval-binop "There was an error (pow error)")]))
      (great () (cond [(and (number? val1) (number? val2)) (> val1 val2)]
                      [(and (boolean? val1) (number? val2)) (eopl:error 'eval-binop "Not defined method <bin-op> for boolean")]
                      [(and (number? val1) (boolean? val2)) (eopl:error 'eval-binop "Boolean can't be coerced into Integer")]
                      [(and (number? val1) (list? val2)) (eopl:error 'eval-binop "Array can't be coerced into Integer")]
                      [(and (list? val1) (number? val2)) (eopl:error 'eval-binop "No implicit conversion of Integer into Array")]
                      [else (eopl:error 'eval-binop "There was an error (great error)")]))
      (great-eq () (cond [(and (number? val1) (number? val2)) (>= val1 val2)]
                         [(and (boolean? val1) (number? val2)) (eopl:error 'eval-binop "Not defined method <bin-op> for boolean")]
                         [(and (number? val1) (boolean? val2)) (eopl:error 'eval-binop "Boolean can't be coerced into Integer")]
                         [(and (number? val1) (list? val2)) (eopl:error 'eval-binop "Array can't be coerced into Integer")]
                         [(and (list? val1) (number? val2)) (eopl:error 'eval-binop "No implicit conversion of Integer into Array")]
                         [else (eopl:error 'eval-binop "There was an error (great-eq error)")]))
      (less () (cond [(and (number? val1) (number? val2)) (< val1 val2)]
                     [(and (boolean? val1) (number? val2)) (eopl:error 'eval-binop "Not defined method <bin-op> for boolean")]
                     [(and (number? val1) (boolean? val2)) (eopl:error 'eval-binop "Boolean can't be coerced into Integer")]
                     [(and (number? val1) (list? val2)) (eopl:error 'eval-binop "Array can't be coerced into Integer")]
                     [(and (list? val1) (number? val2)) (eopl:error 'eval-binop "No implicit conversion of Integer into Array")]
                     [else (eopl:error 'eval-binop "There was an error (less error)")]))
      (less-eq () (cond [(and (number? val1) (number? val2)) (<= val1 val2)]
                        [(and (boolean? val1) (number? val2)) (eopl:error 'eval-binop "Not defined method <bin-op> for boolean")]
                        [(and (number? val1) (boolean? val2)) (eopl:error 'eval-binop "Boolean can't be coerced into Integer")]
                        [(and (number? val1) (list? val2)) (eopl:error 'eval-binop "Array can't be coerced into Integer")]
                        [(and (list? val1) (number? val2)) (eopl:error 'eval-binop "No implicit conversion of Integer into Array")]
                        [else (eopl:error 'eval-binop "There was an error (less-eq error)")]))
      (equal () (cond [(and (number? val1) (number? val2)) (= val1 val2)]
                      [(and (boolean? val1) (number? val2)) (eopl:error 'eval-binop "Not defined method <bin-op> for boolean")]
                      [(and (number? val1) (boolean? val2)) (eopl:error 'eval-binop "Boolean can't be coerced into Integer")]
                      [(and (number? val1) (list? val2)) (eopl:error 'eval-binop "Array can't be coerced into Integer")]
                      [(and (list? val1) (number? val2)) (eopl:error 'eval-binop "No implicit conversion of Integer into Array")]
                      [else (eopl:error 'eval-binop "There was an error (equal error)")]))
      (not-equal () (cond [(and (number? val1) (number? val2)) (not (= val1 val2))]
                          [(and (boolean? val1) (number? val2)) (eopl:error 'eval-binop "Not defined method <bin-op> for boolean")]
                          [(and (number? val1) (boolean? val2)) (eopl:error 'eval-binop "Boolean can't be coerced into Integer")]
                          [(and (number? val1) (list? val2)) (eopl:error 'eval-binop "Array can't be coerced into Integer")]
                          [(and (list? val1) (number? val2)) (eopl:error 'eval-binop "No implicit conversion of Integer into Array")]
                          [else (eopl:error 'eval-binop "There was an error (not-equal error)")]))
      (and-op () (cond [(and (boolean? val1) (boolean? val2)) (and val1 val2)]
                       [(and (boolean? val1) (number? val2)) (eopl:error 'eval-binop "Not defined method <bin-op> for boolean")]
                       [(and (number? val1) (boolean? val2)) (eopl:error 'eval-binop "Boolean can't be coerced into Integer")]
                       [(and (number? val1) (list? val2)) (eopl:error 'eval-binop "Array can't be coerced into Integer")]
                       [(and (list? val1) (number? val2)) (eopl:error 'eval-binop "No implicit conversion of Integer into Array")]
                       [else (eopl:error 'eval-binop "There was an error (and error)")]))
      (or-op () (cond [(and (boolean? val1) (boolean? val2)) (or val1 val2)]
                      [(and (boolean? val1) (number? val2)) (eopl:error 'eval-binop "Not defined method <bin-op> for boolean")]
                      [(and (number? val1) (boolean? val2)) (eopl:error 'eval-binop "Boolean can't be coerced into Integer")]
                      [(and (number? val1) (list? val2)) (eopl:error 'eval-binop "Array can't be coerced into Integer")]
                      [(and (list? val1) (number? val2)) (eopl:error 'eval-binop "No implicit conversion of Integer into Array")]
                      [else (eopl:error 'eval-binop "There was an error (or error)")])
             )
      (in-range () (cond [(and (number? val1) (number? val2)) (inclusive-range val1 val2)]
                          [(and (boolean? val1) (number? val2)) (eopl:error 'eval-binop "Not defined method <bin-op> for boolean")]
                          [(and (number? val1) (boolean? val2)) (eopl:error 'eval-binop "Boolean can't be coerced into Integer")]
                          [(and (number? val1) (list? val2)) (eopl:error 'eval-binop "Array can't be coerced into Integer")]
                          [(and (list? val1) (number? val2)) (eopl:error 'eval-binop "No implicit conversion of Integer into Array")]
                          [else (eopl:error 'eval-binop "There was an error (in-range error)")]))
      (ex-range () (cond [(and (number? val1) (number? val2)) (exclusive-range val1 val2)]
                          [(and (boolean? val1) (number? val2)) (eopl:error 'eval-binop "Not defined method <bin-op> for boolean")]
                          [(and (number? val1) (boolean? val2)) (eopl:error 'eval-binop "Boolean can't be coerced into Integer")]
                          [(and (number? val1) (list? val2)) (eopl:error 'eval-binop "Array can't be coerced into Integer")]
                          [(and (list? val1) (number? val2)) (eopl:error 'eval-binop "No implicit conversion of Integer into Array")]
                          [else (eopl:error 'eval-binop "There was an error (ex-range error)")]))
      (st-range () (cond [(and (list? val1) (number? val2)) (step val1 val2)]
                          [(and (boolean? val1) (number? val2)) (eopl:error 'eval-binop "Not defined method <bin-op> for boolean")]
                          [(and (number? val1) (boolean? val2)) (eopl:error 'eval-binop "Boolean can't be coerced into Integer")]
                          [(and (number? val1) (list? val2)) (eopl:error 'eval-binop "Array can't be coerced into Integer")]
                          [else (eopl:error 'eval-binop "There was an error (st-range error)")]))
      )
    )
  )

;; Funciones auxiliares 2do nivel: son las funciones auxiliares que seguramente son usadas por las funciones
;; auxiliares de 1er nivel, y que casi que seguramente no son ejecutadas en el eval-expressions.

;; just-id: Dado un simple-value, en el caso de ser un id-val simplemente devuelve el id.
(define just-id
  (lambda (s-value env)
    (cases simple-value s-value
      (id-val (id) id)
      (else eopl:error('just-id "Can't give value to a value that isn't id")))))

;; search: Dado un elemento y una lista, si el elemento se encuentra en la lista, devuelve #t, si no se encuentra,
;; devuelve #f.
(define search;by diaz <3
  (lambda (x ls)
    (cond
      [(null? ls) #f]
      [(equal? x (car ls)) #t]
      [else (search x (cdr ls))]
      )
    )
  )

;; where-is?: Dada una lista y un elemento, si el elemento no está en la lista, arroja error. Si efectivamente
;; el elemento se encuentra en la lista, devuelve su posición (de 0 a (length lst)) de la lista.
(define where-is?
  (lambda (lst elem)
    (cond
      [(eq? lst '()) (eopl:error 'where-is? "Element does not belong to that list")]
      [(eq? (car lst) elem) 0]
      [else (+ (where-is? (cdr lst) elem) 1)])))

;; pass-it: Trata el caso de call, dado un llamado de argumentos, le aplica eval-comp-value-arguments
;; a todos los argumentos del llamado.
(define pass-it
  (lambda (minicall env)
    (cases call minicall
      (arguments-call (args) (eval-comp-value-arguments args env)))))

;; access-to-array: Función que recibe un array y una lista de accesos al array, si la lista de accesos
;; tiene tamaño uno quiere decir que es el caso array[0] y simplemente devuelve el elemento del
;; array que tiene esa posición. Si tiene tamaño 2 realiza lo correspondiente a lo que dice el documento
;; ejemplo a = [1,2,3,4,5,6,7] entonces a[0,4] arroja '(1 2 3 4). Si el tamaño es mayor o igual a 3
;; resulta en error, y si los indices en llamado doble cualquiera es negativo también arroja error.
(define access-to-array
  (lambda (array lst)
    (cond
      [(= 1 (length lst)) (list-ref array (car lst))]
      [(= 2 (length lst)) (if (or (< (car lst) 0) (< (cadr lst) 0))
                              (eopl:error 'access-to-array "Expected 1 to 2 positive arguments")
                              (give-me-array array (numbers-between (car lst) (cadr lst))))]
      [(>= 3 (length lst)) (eopl:error 'access-to-array "Can't access to an array given 3 or more arguments")])))

;; give-me-array: La funcinó que tiene la tarea de realizar el efecto de llamados del tipo a[m,n] en arrays
(define give-me-array
  (lambda (lst1 lst2)
    (cond
      [(eq? lst2 '()) '()]
      [else (cons (list-ref lst1 (car lst2)) (give-me-array lst1 (cdr lst2)))])))

;; numbers-between: Dado dos limites, arroja una lista de numeros entre esos dos limites.
;; Si la diferencia entre los dos numeros es 1, retorna una lista con ambos numeros. 
(define numbers-between
  (lambda (num1 num2)
    (if (= (- num2 num1) 1)
        (list num1 num2)
        (map (lambda (x) (- x 1)) (reverse (minus num2 num1))))))

;; minus: Función auxiliar usada en numbers-between para lograr el efecto.
(define minus
  (lambda (num1 num2)
    (cond [(eq? num1 num2) '()]
          [else (cons num1 (minus (- num1 1) num2))])))

;; string-mult-aux: Función encargada de realizar multiplicaciones de strings.    
(define string-mult-aux
  (lambda (str times)
    (cond [(= times 1) str]
          [(> times 1) (string-append str (string-mult-aux str (- times 1)))])))

;; lst-mult-aux: Función encargada de realizar multiplicaciones de listas.
(define lst-mult-aux
  (lambda (lst times)
    (cond [(= times 1) lst]
          [(> times 1) (append lst (lst-mult-aux lst (- times 1)))])))

;; copy-aux: Función que dado un número y un elemento, devuelve una lista con el elemento repetido tantas veces como sea
;; el número. Fue una función utilizada en el taller 1.
(define copy-aux
  (lambda (n x) (cond
                  [(< n 0) (eopl:error 'copy-aux "No se puede numeros negativos, solo enteros iguales o mayores a 0")]
                  [(= n 0) '()]
                  [else (cons x (copy-aux (- n 1) x))])))

;; ordenar: Ordena una lista de números de menor a mayor.
(define ordenar
  (lambda (lst)
    (cond 
      [(eq? lst '()) '()] 
      [(list? lst) (insertar (car lst) 
                             (ordenar (cdr lst)))])))

;; insertar: Función auxiliar usada en la función ordenar.
(define insertar
  (lambda (n lst)
    (cond 
      [(eq? lst '()) (cons n '())] 
      [else (cond 
              [(< n (car lst)) (cons n lst)] 
              [else (cons (car lst) 
                          (insertar n (cdr lst)))])])))

;; step: Función que llama a la función step-aux con el contador en 0.
(define step
  (lambda (lst step)
    (step-aux lst step 0)))

;; step-aux: Función que mediante operaciones con módulo, y dadas una lista de números y el step indicado,
;; y el contador inicializado en 0 siempre (con ayuda de la función step), devuelve una lista con los números
;; y el step indicado.
(define step-aux
  (lambda (lst step contador)
    (if (= (modulo step 2) 0)
        (cond
          [(or (eq? lst '()) (> contador (- (length lst) 1))) '()]
          [else (if (= (modulo contador step) 0)
                    (cons (list-ref lst contador) (step-aux lst step (+ contador 1)))
                    (step-aux lst step (+ contador 1)))])
        (cond
          [(or (> contador (length lst)) (> contador (- (length lst) 1))) '()]
          [else (if (= (modulo contador step) 0)
                    (cons (list-ref lst contador) (step-aux lst step (+ contador 1)))
                    (step-aux lst step (+ contador 1)))]))))

;; inclusive-range: Dados dos limites, devuelve una lista de números entre los dos números, incluyendo el límite superior.
(define inclusive-range
  (lambda (val1 val2)
    (ordenar (cons val2 (numbers-between val1 val2)))))

;; exclusive-range: Dados dos limites, devuelve una lista de números entre los dos números, sin
;; incluir el límite superior.
(define exclusive-range
  (lambda (val1 val2)
    (numbers-between val1 val2)))

;*******************************************************************************************
;Referencias
(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

;*******************************************************************************************
;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec  vector?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (closure ids body env)))
            (iota len) idss bodies)
          env)))))

; |Ambiente recursivo para un solo procedimiento

(define (a-recursive-env a-proc-name ids body env)
  (let ((vec (make-vector 1)))
    (let ((env (extended-env-record (list a-proc-name) vec env)))
          (vector-set! vec 0 (closure ids body env))
          env)
    )
  )

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'Error "undefined local variable or method ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))

;*******************************************************************************************
;*******************************************************************************************
;Ambiente inicial

(define (init-env) (empty-env))
;*******************************************************************************************
;*******************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;*******************************************************************************************
;*******************************************************************************************
(interpretador)

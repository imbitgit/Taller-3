#lang eopl

;funcion para probar el intérprete, se puede usar para evaluar cualquier programa del lenguaje.
;(provide scan&parse evaluar-programa valor-verdad?)
; =========================================
; ESPECIFICACIÓN LÉXICA
; =========================================
; Define:
; - números enteros y decimales
; - textos
; - identificadores
; - espacios y comentarios

(define especificacion-lexica

  '(
    
    (white-space
      (whitespace)
      skip)

    (comment
      ("%" (arbno (not #\newline)))
      skip)

    (identificador
      ("@" letter (arbno (or letter digit "_" "?")))
      symbol)

    (numero
      (digit (arbno digit))
      number)

    (numero
      ("-" digit (arbno digit))
      number)

    (numero
      (digit (arbno digit) "." digit (arbno digit))
      number)

    (numero
      ("-" digit (arbno digit) "." digit (arbno digit))
      number)

    (texto
      (letter (arbno (or letter digit "_" " ")))
      string)
))


; =========================================
; GRAMÁTICA
; =========================================
; Define la sintaxis del lenguaje.
; Incluye:
; - números
; - textos
; - variables
; - primitivas binarias y unarias


(define gramatica

  '(
    
    (programa
      (expresion)
      un-programa)

    (expresion
      (numero)
      numero-lit)

    (expresion
      ("\"" texto "\"")
      texto-lit)

    (expresion
      (identificador)
      var-exp)

    (expresion
      ("(" expresion primitiva-binaria expresion ")")
      primapp-bin-exp)
    (expresion
     (primitiva-unaria "(" expresion ")")
     primapp-un-exp)
    (expresion
     ("procedimiento"
      "(" (separated-list identificador ",") ")"
      "{" expresion "}")
     procedimiento-exp)
    (expresion
     ("evaluar"
      expresion
      "(" (separated-list expresion ",") ")"
      "finEval")
     app-exp)
    

    

    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("*") primitiva-multiplicacion)
    (primitiva-binaria ("/") primitiva-division)
    (primitiva-binaria ("concat") primitiva-concat)
    (primitiva-binaria (">") primitiva-mayor)
    (primitiva-binaria ("<") primitiva-menor)
    (primitiva-binaria (">=") primitiva-mayor-igual)
    (primitiva-binaria ("<=") primitiva-menor-igual)
    (primitiva-binaria ("!=") primitiva-diferente)
    (primitiva-binaria ("==") primitiva-comparador-igual)

    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    (primitiva-unaria ("neg") primitiva-negacion)
))

; =========================================
; GENERACIÓN AUTOMÁTICA DE DATATYPES
; =========================================
; Construye automáticamente:
; - scanner
; - parser
; - define-datatype

(sllgen:make-define-datatypes
  especificacion-lexica
  gramatica)

(define scan&parse

  (sllgen:make-string-parser
    especificacion-lexica
    gramatica))

; =========================================
;DATATYPE: ambiente
;=========================================
;PROPÓSITO: Definir la estructura recursiva para los ambientes léxicos.
;VARIANTES:
;- (vacio): Representa un ambiente sinvariables ligadas.
;- (extendido ids vals old-env):Añade una lista de simbolos (ids)y sus
;correspondientes valores (vals) sobre un ambiente previo (old-env).

(define scheme-value?
  (lambda (v) #t))


(define-datatype ambiente ambiente?

  (vacio)

  (extendido
    (ids (list-of symbol?))
    (vals (list-of scheme-value?))
    (old-env ambiente?)))


; =========================================
; DATATYPE: procVal
; =========================================
; Representa procedimientos (closures).
;
; Una cerradura almacena:
; - parámetros
; - cuerpo
; - ambiente léxico
; =========================================

(define-datatype procVal procVal?
  (cerradura
   (lista-ID
    (list-of symbol?))
   (cuerpo
    expresion?)
   (amb
    ambiente?)))



(define ambiente-inicial

  (extendido
    '(@a @b @c @d @e)
    '(1 2 3 "hola" "FLP")
    (vacio)))

;; Busca la posición de un identificador
;; dentro de una lista de símbolos.

(define buscar-posicion

  (lambda (id lista)

    (let loop ((lst lista)
               (pos 0))

      (cond

        [(null? lst)
         #f]

        [(eqv? id (car lst))
         pos]

        [else
         (loop (cdr lst)
               (+ pos 1))]))))


; =========================================
;FUNCIÓN: buscar-variable
;=========================================
;PROPÓSITO:Buscar el valor asociado a un identificador (@id) en un ambiente dado

;ARGUMENTOS:
;- id: El símbolo que representa la variable a buscar
;- env: El ambiente actual en donde se realiza la búsqueda.

;RETORNA: El valor de la variable si existe, o produce un error si llega a (vacio).

(define buscar-variable

  (lambda (id env)

    (cases ambiente env

      (vacio ()

        (eopl:error
          'buscar-variable
          "Error, variable no existe ~s"
          id))

      (extendido (ids vals old-env)

        (let ((posicion
                (buscar-posicion id ids)))

          (if posicion
              (list-ref vals posicion)
              (buscar-variable id old-env)))))))

;; Evalúa primitivas binarias como:
;; +, ~, *, /, concat, etc.


(define evaluar-primitiva-binaria

  (lambda (prim val1 val2)

    (cases primitiva-binaria prim

      (primitiva-suma ()
        (+ val1 val2))

      (primitiva-resta ()
        (- val1 val2))

      (primitiva-multiplicacion ()
        (* val1 val2))

      (primitiva-division ()
        (/ val1 val2))

      (primitiva-concat ()
        (string-append val1 val2))

      (primitiva-mayor ()
        (> val1 val2))

      (primitiva-menor ()
        (< val1 val2))

      (primitiva-mayor-igual ()
        (>= val1 val2))

      (primitiva-menor-igual ()
        (<= val1 val2))

      (primitiva-diferente ()
        (not (equal? val1 val2)))

      (primitiva-comparador-igual ()
        (equal? val1 val2))

      (else
        (eopl:error
          'primitiva-binaria
          "No implementada")))))

;; Evalúa primitivas unarias como:
;; add1, sub1, longitud y neg.


(define evaluar-primitiva-unaria

  (lambda (prim val)

    (cases primitiva-unaria prim

      (primitiva-add1 ()
        (+ val 1))

      (primitiva-sub1 ()
        (- val 1))

      (primitiva-longitud ()
        (string-length val))
      
      (primitiva-negacion ()
        (- val))
      (else
        (eopl:error
          'primitiva-unaria
          "No implementada")))))

; =========================================
; FUNCIÓN: aplicar-procedimiento
; =========================================
; Aplica una cerradura a una lista
; de argumentos ya evaluados.
;
; proc -> closure
; argumentos -> valores evaluados
; =========================================

(define aplicar-procedimiento

  (lambda (proc argumentos)

    (cases procVal proc

      (cerradura
        (ids cuerpo amb)

        (evaluar-expresion

          cuerpo

          (extendido
            ids
            argumentos
            amb))))))


; =========================================
;FUNCIÓN: evaluar-expresion
;=========================================
;PROPÓSITO:Evaluar de forma recursiva una estructura de Sintaxis Abstracta (AST)
;bajo un contexto de ambiente determinado.
;
;ARGUMENTOS:
;- exp: Árbol de sintaxis abstracta producido por el parser.
;- env: Ambiente en el que se resolverán los identificadores hallados.
;
;RETORNA:El resultado expresado de la evaluación (Número, Texto, Booleano, etc.)


(define evaluar-expresion

  (lambda (exp env)

    (cases expresion exp

      (numero-lit (num)
        num)

      (texto-lit (txt)
        txt)

      (var-exp (id)
        (buscar-variable id env))

      (primapp-bin-exp (exp1 prim exp2)

        (let (
              (val1 (evaluar-expresion exp1 env))
              (val2 (evaluar-expresion exp2 env)))

          (evaluar-primitiva-binaria
            prim
            val1
            val2)))

      (primapp-un-exp (prim exp)
                      (let (
                            (val (evaluar-expresion exp env)))    
         (evaluar-primitiva-unaria
          prim
          val)))

; ===================================
; procedimiento-exp
; ===================================
      (procedimiento-exp
       (ids cuerpo)
       (cerradura
        ids
        cuerpo
        env))

; ===================================
; app-exp
; ===================================
      (app-exp
       (rator rands)
       (let (

             (proc
              (evaluar-expresion
               rator
               env))
             (args
              (map
               (lambda (x)
                 (evaluar-expresion
                  x
                  env))
               rands)))
         (aplicar-procedimiento
          proc
          args))))))


;; En una expresión numérica, 0 es falso y cualquier otro valor es verdadero.
;; Devuelve 0 para falso y 1 para verdadero.
(define valor-verdad?

  (lambda (valor)

    (cond

      [(number? valor)
       (if (zero? valor) 0 1)]

      [else
       (eopl:error
         'valor-verdad?
         "Error, valor no numérico ~s"
         valor)])))


;; Evalúa un programa completo
;; utilizando el ambiente inicial.



(define evaluar-programa

  (lambda (pgm)

    (cases programa pgm

      (un-programa (exp)

        (evaluar-expresion
          exp
          ambiente-inicial)))))


; =========================================
; Ejemplos
; =========================================

(evaluar-programa
  (scan&parse "5"))
(evaluar-programa
  (scan&parse "\"hola\""))
(evaluar-programa
  (scan&parse "@a"))




;=========================================
;PRUEBAS
;=========================================
(define correr-prueba
  (lambda (nombre string-codigo resultado-esperado)
    (let ((resultado (evaluar-programa (scan&parse string-codigo))))
      (if (equal? resultado resultado-esperado)
          (eopl:printf "Prueba [~a]: PASÓ (Retornó: ~v)\n" nombre resultado)
          (eopl:printf "Prueba [~a]: FALLÓ (Esperaba: ~v | Obtuvo: ~v)\n" nombre resultado-esperado resultado)))))

;Pruebas de Literales (Números y Textos)
(correr-prueba "Literal Numérico Entero Positivo" "42" 42)
(correr-prueba "Literal Numérico Decimal Positivo" "3.1416" 3.1416)
(correr-prueba "Literal Numérico Entero Negativo" "-10" -10)
(correr-prueba "Literal Numérico Decimal Negativo" "-2.5" -2.5)
(correr-prueba "Literal de Texto simple" "\"Hola Mundo\"" "Hola Mundo")
(correr-prueba "Literal de Texto con guiones" "\"clase_3_flp\"" "clase_3_flp")

;Pruebas de Ambiente Inicial
(correr-prueba "Variable entera @a" "@a" 1)
(correr-prueba "Variable entera @b" "@b" 2)
(correr-prueba "Variable de texto @d" "@d" "hola")
(correr-prueba "Variable de texto @e" "@e" "FLP")

;Pruebas de Primitivas Binarias Básicas
(correr-prueba "Suma simple" "(4 + 5)" 9)
(correr-prueba "Sesta con virgulilla simple" "(10 ~ 4)" 6)
(correr-prueba "Sesta que produce negativo" "(4 ~ 5)" -1)
(correr-prueba "Operaciones anidadas basicas" "((2 + 3) + @a)" 6)

;Pruebas de Primitivas Unarias Básicas
(correr-prueba "Sustracción unitaria sub1" "sub1(5)" 4)
(correr-prueba "Adición unitaria add1" "add1(@c)" 4)

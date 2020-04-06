#lang racket
(define ola "ola")
(define pollito "pollito")
(define murcielago "murcielago")
(define universidad "universidad")
(define pereira "pereira")
;------------------------------------------------------------------------------------------------------------------------------------------------------
(define (eleccionDePalabras a ola pollito murcielago universidad pereira)
;------------------------------------------------------------------------------------------------------------------------------------------------------
  (define (reemplazadora palabra numeroDeErrores LongitudDePalabra letra contadora palabraAMostrar aciertos letrasjugadas mia numeroDeLetrsMalas)
          (if (= contadora -1);con esta condición queremos analizar letra por letra de la palabra 
              (begin
                (set! letrasjugadas (string-append letrasjugadas letra))
                (cond ;acá se hace una seteo al número de errores
                  [(= numeroDeLetrsMalas LongitudDePalabra)(set! numeroDeErrores (+ numeroDeErrores 1))]
                 )
                (ahorcado palabra numeroDeErrores LongitudDePalabra aciertos letrasjugadas mia))
              (begin
                (if (equal? letra (~a(string-ref palabra contadora)))
                    (begin
                      (string-set! mia contadora (string-ref palabra contadora));con este seteo reemplazamos el espacio en el que corresponde la letra correcta
                      (reemplazadora palabra numeroDeErrores LongitudDePalabra letra (- contadora 1) palabraAMostrar (+ 1 aciertos) letrasjugadas mia numeroDeLetrsMalas)
                      )
                    (begin
                      (reemplazadora palabra numeroDeErrores LongitudDePalabra letra (- contadora 1) palabraAMostrar aciertos letrasjugadas mia (+ numeroDeLetrsMalas 1))
                      )
                )
              )
    ))
;-------------------------------------------------------------------------------------------------------------------------------------------------------
  (define (ahorcado palabra numeroDeErrores LongitudDePalabra aciertos letrasjugadas mia);con esta función muestro la interfaz del juego
    (display "AHORCADO \n")
          (printf "~a~a~a" "+-----------+ \n" "|" (make-string 12 #\ ))
          (cond ;estos cond´s son usados para ver cuantos errores lleva y si hay la necesidad de imprimir el muñeco
            [(< 0 numeroDeErrores)(display "O")]
            )
          (printf "~a~a~a~a~a" (make-string 6 #\ ) mia "\n" "|" (make-string 11 #\ ))
          (cond
            [(< 1 numeroDeErrores)(printf "~a" "/")]
            [else (printf "~a" (make-string 7 #\ ))]
          )
          (cond
            [(< 2 numeroDeErrores)(printf "~a" "|" )])
          (cond
            [(< 3 numeroDeErrores)(printf "~a~a" (substring "\\" 0 1) (make-string 5 #\ ))])
          (printf "~a" "\n|")
          (cond
            [(< 4 numeroDeErrores)(printf "~a~a" (make-string 12 #\ ) "|" )]
            (printf "~a" "|")
            [else (printf "~a "(make-string 18 #\ ))]
            )
          (printf "~a" "\n|")
          (cond
            [(< 5 numeroDeErrores)(printf "~a~a" (make-string 11 #\ ) "/ ")])
           (cond
            [(< 6 numeroDeErrores)(printf "~a~a" (substring "\\" 0 1) (make-string 5 #\ ))])
          (printf "~a~a~a~a" "\n|" (make-string 18 #\ ) "Letras jugadas: " letrasjugadas)
          (printf "~a" "\n -------------")
          (printf "~a~a" "\n" "Ingresa una letra ")
          (cond ;Con esta condicional analizamos si ya perdió ó ganó el juego, o si sigue el juego 
            [(= numeroDeErrores 7)(display "***PERDISTE***")]
            [(= aciertos 7)(display "***GANASTE***")]
            [else (reemplazadora palabra numeroDeErrores LongitudDePalabra (~a(read)) (- LongitudDePalabra 1) "" aciertos letrasjugadas mia 0)]
            ) 
    )
;-------------------------------------------------------------------------------------------------------------------------------------------------------
  [cond;aquí se escoge la palabra con el número random
    ((= a 1)(ahorcado ola 0 3 0 "" (string-copy "---") ))
    ((= a 2)(ahorcado pollito 0 7 0 "" (string-copy "-------") ))
    ((= a 3)(ahorcado murcielago 0 10 0 "" (string-copy "----------")))
    ((= a 4)(ahorcado universidad 0 11 0 "" (string-copy "-----------")))
    ((= a 5)(ahorcado pereira 0 7 0 "" (string-copy "-------")))
    ]
  )
(eleccionDePalabras (random 1 6) ola pollito murcielago universidad pereira);Aquí pasamos el random más las palabras para ser escogidas
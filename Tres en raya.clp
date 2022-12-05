
;;-------Tic Tac Toe--------------
;;Es un juego para dos jugadores, X y O, que toman turnos marcando los espacios en una cuadrícula de 3 × 3. 
;;El jugador que logre colocar tres marcas respectivas en una fila horizontal, vertical o diagonal gana el juego.


;;Funcion que permite jugar nuevamente.
(deffunction Nuevo ()
 (clear-window)
 (reset)
 (run)
)

(deftemplate celda
  ;(slot id (type NUMBER))
  (slot fila (type NUMBER))
  (slot columna (type NUMBER))
  (multislot valor (type NUMBER))
)

(deftemplate Contenido-celda
  (slot valor)
  (slot parte1)
  (slot parte2)
  (slot parte3)
  (slot parte4)
)

(deffacts hechos-iniciales 

(ingresar-tablero)
(imprime 1 1)
(parteImprimir 1)
(actualizar-tablero)
(contador 0)
(elegir-turno)

 (Contenido-celda
  (valor 0)   
  (parte1 "      ")
  (parte2 "      ")
  (parte3 "      ")
  (parte4 "      ")
 )

 (Contenido-celda
 (valor 1)
 (parte1 " \\  / ")
 (parte2 "  \\/  ")
 (parte3 "  /\\  ")
 (parte4 " /  \\ ")
 )

 (Contenido-celda
 (valor 2)
 (parte1 "  --  ")
 (parte2 " |  | ")
 (parte3 " |  | ")
 (parte4 "  --  ")
 )
 (no-activar 1)
 (empates 0)
 (maquina 0)
)


;;Regla donde se ingresan los identificadores
(defrule ingresar-identificadores-celda
;Damos mas prioridad a la regla
(declare (salience 100))
  
  ?h <- (ingresar-tablero)
  =>
  (retract ?h)

  (loop-for-count (?i 1 3)do
    (loop-for-count (?j 1 3)do
        
         (assert (celda (fila ?i)(columna ?j)(valor 0)))
         
    ) 
  )
)

;;---------------------------------------------------------------------------
;;---------------------------------------------------------------------------

;Regla que me permite elegir el turno.

(defrule elegir-turno
   (declare (salience 99))
   ?h <- (elegir-turno)
   
   =>
   ;Menu para los turnos
   (printout t "Escoja su turno" crlf)
   (printout t "1) primer turno"crlf)
   (printout t "2) segundo turno"crlf)
   ;Creamos una variable para almacenar un numero
   (bind ?j(read))
   ;creamos una variable turno
   (bind ?turno 0) 
  ;creamos un hecho turno
  (assert (turno ?turno))
  ;por medio de estos if determinamos si inicia el usuario o la IA:ademas de que no permite ingresar datos fuera del rango establecido.
  (if(and (< ?j 3) (> ?j 0) )then
      (if (= ?j 1)then
          (assert (actualizar-tablero))
          (assert (turno (+ ?turno 1)))
          (clear-window)
      )
      (if (= ?j 2)then
         (assert (inicia-IA))
         (assert (actualizar-tablero))
         (clear-window)
   
      )
  else
  (printout t "DATOS FUERA DE RANGO"crlf)
  )
)

;;---------------------------------------------------------------------------
;;---------------------------------------------------------------------------

;esta regla me permite iniciar con la IA siempre y cuando el usuario lo decida
(defrule inicia-IA
  (declare (salience 98))
   ?h <- (inicia-IA)
   (turno ?turno)
   ;por medio de este hecho optenemos la celda en que ira la IA
   ?m-maquina <- (celda (fila 2 ) (columna 2 ) (valor 0))
=>
(retract ?h)
  (if (= ?turno 0)then
   (modify ?m-maquina (valor 2))
   (assert (actualizar-tablero))
  ) 
)

;;---------------------------------------------------------------------------
;;---------------------------------------------------------------------------

;Regla que me permite dibujar el tablero del juego
(defrule Tablero-completo
  (declare (salience 96))
  ?h <- (imprime ?i ?j&:(<= (* ?i ?j) 9))

  (Contenido-celda  (valor ?value)(parte1 ?p1)(parte2 ?p2)(parte3 ?p3)(parte4 ?p4))

  ?f <- (parteImprimir ?parteImprimir)

  ?g <- (actualizar-tablero)
  

  (celda (fila ?i)(columna ?j)(valor ?value $?))
   =>
  
   (retract ?h)

    (if (and (= ?i 1) (= ?j 1) (= ?parteImprimir 1)) then
     (printout t crlf crlf" ------ ------ ------ "crlf)
    )

   
    (if (and (and (<= ?i 3) (< ?j 3)) (= ?parteImprimir 1))then
     (printout t "|" ?p1)
     (assert (imprime ?i (+ ?j 1)))
    )

    (if (and (and (<= ?i 3) (= ?j 3)) (= ?parteImprimir 1))then
      (printout t "|"?p1"|" crlf)
      (assert (imprime ?i 1))
      (assert (parteImprimir 2))
      (retract ?f)
    )

    (if (and (and (<= ?i 3) (< ?j 3)) (= ?parteImprimir 2))then
     (printout t "|" ?p2)
     (assert (imprime ?i (+ ?j 1)))
    )
     
    (if (and (and (<= ?i 3) (= ?j 3)) (= ?parteImprimir 2))then
      (printout t "|"?p2"|" crlf)
      (assert (imprime ?i 1))
      (assert (parteImprimir 3))
      (retract ?f)
    )
    
    (if (and (and (<= ?i 3) (< ?j 3)) (= ?parteImprimir 3))then
     (printout t "|" ?p3)
     (assert (imprime ?i (+ ?j 1)))
    )
    
    (if (and (and (<= ?i 3) (= ?j 3)) (= ?parteImprimir 3))then
      (printout t "|"?p3"| " ?i crlf)
      (assert (imprime ?i 1))
      (assert (parteImprimir 4))
      (retract ?f)
    )

    (if (and (and (<= ?i 3) (< ?j 3)) (= ?parteImprimir 4))then
     (printout t "|" ?p4)
     (assert (imprime ?i (+ ?j 1)))
    )

    (if (and (and (<= ?i 3) (= ?j 3)) (= ?parteImprimir 4))then
      (printout t "|"?p4"|"  crlf 
                  " ------ ------ ------ " crlf)
      (assert (imprime (+ ?i 1) 1))
      (assert (parteImprimir 1))
      (retract ?f)
    )

    (if (and (and (= ?i 3) (= ?j 3)) (= ?parteImprimir 4))then
     (printout t "    " (- ?i 2) "      " (- ?i 1) "      " ?i crlf crlf)
     (assert (parteImprimir 1))
     (retract ?g)
     (assert (imprime 1 1))
     (assert(pedir-movimientos))
    ) 
  
)


;Regla que me permite pedir los movimientos del usuario
(defrule Pedir-movimientos

 (declare (salience 95))

 ?h <- (pedir-movimientos)
 =>
 (bind ?t TRUE)
 (while (eq ?t TRUE)do

 (retract ?h)
 ;menu en el cual se pide la fila y la columna pero con dicho rango [1-3] para 
 (printout t "Rango entre filas y columnos es de entre [1-3]"crlf)
 (printout t "Ingrese el numero de la fila" crlf)
 (printout t "--Fila : " )
 (bind ?fila (read))

 (printout t "Ingrese el numero de la columna" crlf)
 (printout t "--Columna : " )
 (bind ?columna (read))

;if que me permite determinar el rango de los imput dados por el usuario
  (if(and (< ?fila 4) (< ?columna 4) (> ?fila 0) (> ?columna 0))then
  (assert (fila-columna-agregar ?fila ?columna))
  (assert (comprobar-celda-ocupada))
  ;(clear-window)
  (bind ?t FALSE)
  
  else
  (printout t "DATOS FUERA DE RANGO"crlf)
  )
 )
)

;Regla que me permite comprobar si una celda esta ocupada por una ficha
(defrule comprobar-celda-ocupada
 (declare (salience 94))
  ?h <- (comprobar-celda-ocupada)

  ?g <- (contador ?c)

  ?f <- (fila-columna-agregar ?f1 ?c1)

  (celda (fila ?f1)(columna ?c1)(valor ?value $?))
  =>
  (retract ?h)
  (retract ?g)
  
  ;if que me permite determinar si el usuario ingreso una celda en desocupada, en el caso que este ocupada:se pedira nuevo ingreso.
  (if  (= ?value 0 )then
    (assert(movimiento-usuario ?f1 ?c1 1))
    (assert (ejecutar-movimiento-jugador))
    
   else 
     (if (= ?value 2 )then
      (retract ?f)
      (assert (pedir-movimientos))
      (assert (actualizar-tablero))
      (printout t crlf "Esta celda ya está ocupada"crlf"------NUEVO INGRESO--------"crlf)
     )
  )
  (assert (contador (+ ?c 1)))
)

;;Regla del movimiento del jugador
(defrule ejecutar-movimiento-jugador
  (declare (salience 93))  
   
  ?h <- (ejecutar-movimiento-jugador)

  ?f <- (fila-columna-agregar ?f1 ?c1)
  ;imput de la celda del jugador
  ?celda-poner <- (celda (fila ?f1)(columna ?c1)(valor ?))

  (turno ?t)

  =>  
  (retract ?h)
  (retract ?f)
  
  ;modificamos la celda del usuario
  (modify ?celda-poner (valor 1))
  
  ;condicion con la que determino si inicio la IA se efectuaran unas reglas especificas para el inicio de IA: caso contrario se ejecutaran las reglas que siguen con nomalidad
  (if (= ?t 0)then
   (assert (siguiente-movimiento-inicial-fila-IA))
   (assert (siguiente-movimiento-inicial-columna-IA))
   (assert (siguiente-movimiento-inicial-diagonal-IA))
   (assert (turno (+ ?t 1)))
   
   else 
   (if (> ?t 0)then
       (assert (movimiento-inicial))
   )
  )  
)

;;---------------------------------------------------------------------------
;;---------------------------------------------------------------------------

;;Serie de reglas para el movimiento inicial que puede tomas el usuario

;;Regla donde si el usuario ubica dicha ficha en el centro la IA ubica en una esaquina.

;  ----- ----- -----
; |     |     |     |             
;  ----- ----- -----
; |     |  x  |     |
;  ----- ----- -----
; |     |     |  0  |
;  ----- ----- -----


;;Regla donde si el esuario ubica dicha ficha en cualquiera de las celdas, pero menos en 
;;en la del centro. La IA ubica la ficha en el centro.

;  ----- ----- -----
; |  x  |  x  |  x  |
;  ----- ----- -----
; |  x  |  0  |  x  |
;  ----- ----- -----
; |  x  |  x  |  x  |
;  ----- ----- -----


(defrule movimiento-inicial
    
   ?h <- (movimiento-inicial)
      (movimiento-usuario ?f ?c ?v)
    ;imputs que me traen dos celdas diferentes
   (celda (fila ?fila1 & 2) (columna ?columna1 & 2) (valor ?v1))
   (celda (fila ?fila2 & 3) (columna ?columna2 & 3) (valor ?v2))
    (contador ?c1)
   =>
   (retract ?h)

  ;condicion que me permite determinar si el usurio inicia en el centro la IA se ubica en la celda [3-3], caso contrario 
  ;si el usuario no inicia en el centro la IA inicia en el centro
   (if (= ?c1 1)then
       (if (and (and (= ?f 2) (= ?c 2)) (= ?v 1))then
           (assert (movimiento-maquina-c ?fila2 ?columna2 ?v2))
           (assert (movimiento-maquina))   
         else 
           (assert (movimiento-maquina-c ?fila1 ?columna1 ?v1))
           (assert (movimiento-maquina))  
       ) 
    else 
      (if (and (> ?c1 1) (< ?c1 5))then
      
      ;Reglas estableciada para la IA
      (assert (siguiente-movimiento-gana-fila-IA))
      (assert (siguiente-movimiento-gana-fila-central-IA))
      (assert (siguiente-movimiento-gana-columna-IA))
      (assert (siguiente-movimiento-gana-columna-central-IA))
      (assert (siguiente-movimiento-gana-diagonal-IA))
      (assert (siguiente-movimiento-especial-fila-IA))
      (assert (siguiente-movimiento-especial-columna-IA))
      (assert (siguiente-movimiento-filas-IA))
      (assert (siguiente-movimiento-columna-IA))
      (assert (siguiente-movimiento-interumpe-fila-IA))
      (assert (siguiente-movimiento-interumpe-columna-IA))
      (assert (siguiente-movimiento-intermedio-filas-IA))
      (assert (siguiente-movimiento-intermedio-columna-IA)) 
      (assert (siguiente-movimiento-diagonal-IA))
      (assert (siguiente-movimiento-intermedio-diagonal-IA))
      (assert (siguiente-movimiento-diagonal-esquina-IA))
      (assert (siguiente-movimiento-central-IA))
      (assert (siguiente-movimiento-centrall-IA))
      (assert (siguiente-movimiento-central-columna-IA))
      (assert (siguiente-movimiento-fila-final-IA))
      (assert (siguiente-movimiento-columna-final-IA))
      (assert (terminer-jugada-usuario))
      
      
      )
   ) 
)

(defrule terminer-jugada-usuario 
 (declare (salience 90))
 ?h <- (terminer-jugada-usuario)
 (contador ?c)
 =>
 (if (= ?c 5)then
  (assert (actualizar-tablero-finalizar))
  (assert (finalizar-empate))
 )
)



;;------------------------------------------------------------------
;;------------------------------------------------------------------
;regla que me permite determinar, que si es el turno de la IA y tiene dos 0 en la misma fila y un espacio vacio en cualquier lateral: ese espacio vacio lo cambie por un cero para ganar.
;  ----- ----- -----
; |     |  x  |  x  |             
;  ----- ----- -----
; |     |  x  |     |
;  ----- ----- -----
; |  0  |  0  |  0  |
;  ----- ----- -----

(defrule siguiente-movimiento-gana-fila-IA

 (declare (salience 90))
   
 ?h <- (siguiente-movimiento-gana-fila-IA)
 ?g <- (no-activar ?a)
;imputs que me trae tres tipos de celdas
 ;?s <- (maquina ?x)
   (celda (fila ?f1) (columna ?c1) (valor 2))
   (celda (fila ?f1) (columna ?c2&~?c1) (valor 2))
   (celda (fila ?f1) (columna ?c3&~?c2&~?c1) (valor 0))
   ?m-maquina <- (celda (fila ?f1) (columna ?c3) (valor 0))

 =>
 (retract ?h)
 (retract ?g)
 ;(retract ?s)
        (if (= ?a 1)then
          (modify ?m-maquina (valor 2))
         (assert (no-activar (+ ?a 1)))
         (assert (finalizar-juego-gana-maquina))
         ;(assert (maquina (+ ?s 1)))
         )
         
)

;;------------------------------------------------------------------
;;------------------------------------------------------------------
;regla que me permite determinar, que si es el turno de la IA y tiene dos 0 en la misma fila y un espacio vacio en el centro: ese espacio vacio lo cambie por un cero para ganar.
;  ----- ----- -----
; |     |  x  |  x  |             
;  ----- ----- -----
; |     |  x  |     |
;  ----- ----- -----
; |  0  |  0  |  0  |
;  ----- ----- -----

(defrule siguiente-movimiento-gana-fila-central-IA

 (declare (salience 90))
   
 ?h <- (siguiente-movimiento-gana-fila-central-IA)
 ?g <- (no-activar ?a)
 ;imputs que me trae tres celdas
   (celda (fila ?f1) (columna ?c1) (valor 2))
   (celda (fila ?f1) (columna ?c2&~?c1) (valor 0))
   (celda (fila ?f1) (columna ?c3&~?c2&~?c1) (valor 2))
   ?m-maquina <- (celda (fila ?f1) (columna ?c2) (valor 0))

 =>
 (retract ?h)
 (retract ?g)
          (if (= ?a 1)then
          (modify ?m-maquina (valor 2))
         (assert (no-activar (+ ?a 1)))
         (assert (finalizar-juego-gana-maquina))
          )
)



;;------------------------------------------------------------------
;;------------------------------------------------------------------
;regla que me permite determinar, que si es el turno de la IA y tiene dos 0 en la misma columna y un espacio vacio en cualquier lateral: ese espacio vacio lo cambie por un cero para ganar.
;  ----- ----- -----
; |     |     |  0  |             
;  ----- ----- -----
; |  x  |  x  |  0  |
;  ----- ----- -----
; |  x  |     |  0  |
;  ----- ----- -----

(defrule siguiente-movimiento-gana-columna-IA
  (declare (salience 90))
   
 ?h <- (siguiente-movimiento-gana-columna-IA)
 ?g <- (no-activar ?a)
 ;imputs que me trae tres celdas
   (celda (fila ?f1) (columna ?c1) (valor 2))
   (celda (fila ?f2&~?f1) (columna ?c1) (valor 2))
   (celda (fila ?f3&~?f2&~?f1) (columna ?c1) (valor 0))
   ?m-maquina <- (celda (fila ?f1) (columna ?c1) (valor 0))
 =>
 (retract ?h)
 (retract ?g)
         (if (= ?a 1)then
         (modify ?m-maquina (valor 2))
         (assert (no-activar (+ ?a 1)))
         (assert (finalizar-juego-gana-maquina))
         )
)

;;------------------------------------------------------------------
;;------------------------------------------------------------------
;regla que me permite determinar, que si es el turno de la IA y tiene dos 0 en la misma columna y un espacio vacio en el centro: ese espacio vacio lo cambie por un cero para ganar.
;  ----- ----- -----
; |     |     |  0  |             
;  ----- ----- -----
; |  x  |  x  |  0  |
;  ----- ----- -----
; |  x  |     |  0  |
;  ----- ----- -----

(defrule siguiente-movimiento-gana-columna-central-IA
  (declare (salience 90))
   
 ?h <- (siguiente-movimiento-gana-columna-central-IA)
 ?g <- (no-activar ?a)
 ;imputs quew me traen tres celdas
   (celda (fila ?f1) (columna ?c1) (valor 2))
   (celda (fila ?f2&~?f1) (columna ?c1) (valor 0))
   (celda (fila ?f3&~?f2&~?f1) (columna ?c1) (valor 2))
   ?m-maquina <- (celda (fila ?f2) (columna ?c1) (valor 0))
 =>
 (retract ?h)
 (retract ?g)
         (if (= ?a 1)then
         (modify ?m-maquina (valor 2))
         (assert (no-activar (+ ?a 1)))
         (assert (finalizar-juego-gana-maquina))
         )
)

;;------------------------------------------------------------------
;;------------------------------------------------------------------
;regla que me permite determinar, que si es el turno de la IA y tiene dos 0 en la misma diagonal y un espacio vacio en una de las celdas: ese espacio vacio lo cambie por un cero para ganar.
;  ----- ----- -----
; |  x  |     |     |             
;  ----- ----- -----
; |     |  0  |     |
;  ----- ----- -----
; |  0  |     |     |
;  ----- ----- -----

(defrule siguiente-movimiento-gana-diagonal-IA

  (declare (salience 90))
 
   
 ?h <- (siguiente-movimiento-gana-diagonal-IA)
 ?g <- (no-activar ?a)

   (celda (fila ?f1) (columna ?c1) (valor 2))
   (celda (fila 2&~?f1) (columna 2&~?c1) (valor 2))
   (celda (fila ?f3&~2&~?f1) (columna ?c3&~2&~?c1) (valor 0))
   ?m-maquina <- (celda (fila ?f3) (columna ?c3) (valor 0))
 =>
 (retract ?h)
 (retract ?g)
         (if (= ?a 1)then
         (modify ?m-maquina (valor 2))
         (assert (no-activar (+ ?a 1)))
         (assert (finalizar-juego-gana-maquina))
         )
)


;;------------------------------------------------------------------
;;------------------------------------------------------------------
;Regla que me permite determinar que si a iniciado la IA, efectue si el usuario a puesto el imput en una de las celdas de las filas con respecto al centro, entonces ubicamod un cero
;tomando como referencia el mirmo contenido que tiene la variable ?c3 , de esta manera obligabos al usuario a defender, ya que el 0 simpre se ubicara en una esquina presionado 
;al usuario con el cero del 0.
;  ----- ----- -----
; |     |     |     |             
;  ----- ----- -----
; |  x  |  0  |     |
;  ----- ----- -----
; |     |     |  0  |
;  ----- ----- -----

(defrule siguiente-movimiento-inicial-fila-IA
   
 ?h <- (siguiente-movimiento-inicial-fila-IA)

   (celda (fila ?f1) (columna ?c1) (valor 1))
   (celda (fila ?f1) (columna ?c2&~?c1) (valor 2))
   (celda (fila ?f1) (columna ?c3&~?c2&~?c1) (valor 0))
   (no-activar ?a)
 =>
 (retract ?h)
 
    (if (neq ?a 2)then
    (assert (movimiento-maquina-c  ?c3 ?c3 0))
    (assert (movimiento-maquina))
    )
    
)


;;------------------------------------------------------------------
;;------------------------------------------------------------------
;Regla que me permite determinar que si a iniciado la IA, efectue si el usuario a puesto el imput en  una de las celdas de las columnas con respecto al centro, entonces ubicamos un cero
;tomando como referencia el mirmo contenido que tiene la variable ?f3 , de esta manera obligabos al usuario a defender, ya que el 0 simpre se ubicara en una esquina presionado 
;al usuario con el cero del 0.
;  ----- ----- -----
; |     |  x  |     |             
;  ----- ----- -----
; |     |  0  |     |
;  ----- ----- -----
; |     |     |  0  |
;  ----- ----- -----

(defrule siguiente-movimiento-inicial-columna-IA
  
 ?h <- (siguiente-movimiento-inicial-columna-IA)

   (celda (fila ?f1) (columna ?c1) (valor 1))
   (celda (fila ?f2&~?f1) (columna ?c1 ) (valor 2))
   (celda (fila ?f3&~?f2&~?f1) (columna ?c1 ) (valor 0))
  
 =>
 (retract ?h)
 
        
         (assert (movimiento-maquina-c ?f3 ?f3 0))
         (assert (movimiento-maquina))
             
)


;;------------------------------------------------------------------
;;------------------------------------------------------------------
;Regla que me permite determinar que si a iniciado la IA, efectue si el usuario a puesto el imput en  una de las celdas de las esquinas con respecto al centro, entonces ubicamos un cero
;tomando como referencia la fila de la variable esquinera, de esta manera obligabos al usuario a defender, ya que el 0 simpre se ubicara en una esquina presionado 
;al usuario con el cero del 0.
;  ----- ----- -----
; |  x  |     |     |             
;  ----- ----- -----
; |     |  0  |     |
;  ----- ----- -----
; |  0  |     |     |
;  ----- ----- -----

(defrule siguiente-movimiento-inicial-diagonal-IA
   
 ?h <- (siguiente-movimiento-inicial-diagonal-IA)

   (celda (fila ?f1) (columna ?c1) (valor 1))
   (celda (fila ?f2&~?f1) (columna ?c2&~?c1) (valor 2))
   (celda (fila ?f3&~?f2&~?f1) (columna ?c3&~?c2&~?c1) (valor 0))
  
 =>
 (retract ?h)
 
         
         (assert (movimiento-maquina-c ?f1 ?c3 0))
         (assert (movimiento-maquina))
         
        
)



;;------------------------------------------------------------------
;;------------------------------------------------------------------
;Regla que me permite determinar que si a iniciado la IA, efectue si el usuario a puesto el imput en  una de las celdas de las esquinas con respecto al centro, entonces ubicamos un cero
;tomando como referencia la fila de la variable esquinera, de esta manera obligabos al usuario a defender, ya que el 0 simpre se ubicara en una esquina presionado 
;al usuario con el cero del 0.
;  ----- ----- -----
; |  x  |     |     |             
;  ----- ----- -----
; |     |  0  |     |
;  ----- ----- -----
; |  0  |     |     |
;  ----- ----- -----

(defrule siguiente-movimiento-intermedio-diagonal-IA
   
 ?h <- (siguiente-movimiento-intermedio-diagonal-IA)

   (celda (fila ?f1) (columna ?c1) (valor 1))
   (celda (fila ?f2&~?f1) (columna ?c2&~?c1) (valor 2))
   (celda (fila ?f3&~?f2&~?f1) (columna ?c3&~?c2&~?c1) (valor 0))
    (no-activar ?a)
 =>
 (retract ?h)
         (if (neq ?a 2)then
         (assert (movimiento-maquina-c ?f1 ?c3 0))
         (assert (movimiento-maquina))
         )
)

;;------------------------------------------------------------------
;;------------------------------------------------------------------
;Esta regla es especial pera una jugada, ya que dicha jugada tiene mas ventaja para el usurio, asi que por lo tanto esta regla permite que la IA empate mas no permite que el usuario
;se lleve la victoria.
;en este caso seria para las filas.
;  ----- ----- -----
; |  0  |  x  |     |             
;  ----- ----- -----
; |     |  0  |     |
;  ----- ----- -----
; |  x  |     |     |
;  ----- ----- -----

(defrule siguiente-movimiento-especial-fila-IA
   (declare (salience 89))
 ?h <- (siguiente-movimiento-especial-fila-IA)
  (contador ?t)

   (celda (fila ?f1) (columna 2) (valor 1))
   (celda (fila ?f2&~?f1) (columna 2) (valor 2))
   (celda (fila ?f3&~?f2&~?f1) (columna ?c3&~2) (valor 1))
   
  
 =>
 (retract ?h)
   (if (= ?t 2)then
    (assert (movimiento-maquina-c  ?f1 ?c3 0))
    (assert (movimiento-maquina))
   )
    
)

;;------------------------------------------------------------------
;;------------------------------------------------------------------
;Esta regla es especial pera una jugada, ya que dicha jugada tiene mas ventaja para el usurio, asi que por lo tanto esta regla permite que la IA empate mas no permite que el usuario
;se lleve la victoria.
;cesta caso seria para la columna.
;  ----- ----- -----
; |  x  |     |  0  |             
;  ----- ----- -----
; |     |  0  |  x  |
;  ----- ----- -----
; |     |     |     |
;  ----- ----- -----

(defrule siguiente-movimiento-especial-columna-IA
   (declare (salience 89))
 ?h <- (siguiente-movimiento-especial-columna-IA)
  (contador ?t)
   (celda (fila 2) (columna ?c1) (valor 1))
   (celda (fila 2) (columna ?c2&~?c1) (valor 2))
   (celda (fila ?f1&~2) (columna ?c3&~?c2&~?c1) (valor 1))

 =>
 (retract ?h)
  (if (= ?t 2)then
    (assert (movimiento-maquina-c  ?f1 ?c1 0))
    (assert (movimiento-maquina))
  )
    
)



;;------------------------------------------------------------------
;;------------------------------------------------------------------
;Esta regla me permite determinar si una fila tiene dos fichas de usuario po lo tanto, por parte de la IA completa con un 0.
;para las filas .
;  ----- ----- -----
; |  x  |  x  |  0  |             
;  ----- ----- -----
; |     |  0  |     |
;  ----- ----- -----
; |     |     |     |
;  ----- ----- -----

(defrule siguiente-movimiento-filas-IA
   (declare (salience 88))
 ?h <- (siguiente-movimiento-filas-IA)
   (celda (fila ?f1) (columna ?c1) (valor 1))
   (celda (fila ?f1) (columna ?c2&~?c1 ) (valor 1))
   (celda (fila ?f1) (columna ?c3&~?c2&~?c1 ) (valor 0))
   (no-activar ?a)
 =>
 (retract ?h)

        (if (neq ?a 2)then
         (assert (movimiento-maquina-c ?f1 ?c3 0))
         (assert (movimiento-maquina))   
        )  
)



;;------------------------------------------------------------------
;;------------------------------------------------------------------
;Esta regla me permite determinar si una fila tiene dos fichas de usuario po lo tanto, por parte de la IA completa con un 0.
;para las columnas.
;  ----- ----- -----
; |  x  |     |     |             
;  ----- ----- -----
; |  x  |  0  |     |
;  ----- ----- -----
; |  0  |     |     |
;  ----- ----- -----

(defrule siguiente-movimiento-columna-IA
   (declare (salience 88))
 ?h <- (siguiente-movimiento-columna-IA)

   (celda (fila ?f1) (columna ?c1) (valor 1))
   (celda (fila ?f2&~?f1) (columna ?c1 ) (valor 1))
   (celda (fila ?f3&~?f2&~?f1) (columna ?c1 ) (valor 0))
   (no-activar ?a)
 =>
 (retract ?h)
         (if (neq ?a 2)then
         (assert (movimiento-maquina-c ?f3 ?c1 0))
         (assert (movimiento-maquina))
         )
)

;;------------------------------------------------------------------
;;------------------------------------------------------------------

;  ----- ----- -----
; |     |  x  |  x  |             
;  ----- ----- -----
; |     |  x  |     |
;  ----- ----- -----
; |  0  |  0  |  0  |
;  ----- ----- -----

(defrule siguiente-movimiento-interumpe-fila-IA

 (declare (salience 87))
   
 ?h <- (siguiente-movimiento-interumpe-fila-IA)

   (celda (fila ?f1) (columna ?c1) (valor 2))
   (celda (fila ?f1) (columna ?c2&~?c1) (valor 2))
   (celda (fila ?f1) (columna ?c3&~?c2&~?c1) (valor 1))
   (no-activar ?a)
 =>
 (retract ?h)
         (if (neq ?a 2)then
         (assert (movimiento-maquina-c ?c3 ?c3 0))
         (assert (movimiento-maquina))
         )
)


;;------------------------------------------------------------------
;;------------------------------------------------------------------

;  ----- ----- -----
; |     |  x  |  x  |             
;  ----- ----- -----
; |     |  x  |     |
;  ----- ----- -----
; |  0  |  0  |  0  |
;  ----- ----- -----

(defrule siguiente-movimiento-interumpe-columna-IA

 (declare (salience 87))
   
 ?h <- (siguiente-movimiento-interumpe-columna-IA)

   (celda (fila ?f1) (columna ?c1) (valor 2))
   (celda (fila ?f2&~?f1) (columna ?c1) (valor 2))
   (celda (fila ?f3&~?f2&~?f1) (columna ?c1) (valor 1))
   (no-activar ?a)
 =>
 (retract ?h)
         (if (neq ?a 2)then
         (assert (movimiento-maquina-c ?f3 ?f3 0))
         (assert (movimiento-maquina))
         )
)


;;------------------------------------------------------------------
;;------------------------------------------------------------------

;  ----- ----- -----
; |  x  |  0  |  x  |             
;  ----- ----- -----
; |     |  0  |     |
;  ----- ----- -----
; |     |     |     |
;  ----- ----- -----

(defrule siguiente-movimiento-intermedio-filas-IA
   
 ?h <- (siguiente-movimiento-intermedio-filas-IA)
   (celda (fila ?f1) (columna ?c1) (valor 1))
   (celda (fila ?f1) (columna ?c2&~?c1 ) (valor 0))
   (celda (fila ?f1) (columna ?c3&~?c2&~?c1 ) (valor 1))
  (no-activar ?a)
 =>
 (retract ?h)
         (if (neq ?a 2)then
         (assert (movimiento-maquina-c ?f1 ?c2 0))
         (assert (movimiento-maquina))
         )
      
)

;;------------------------------------------------------------------
;;------------------------------------------------------------------

;  ----- ----- -----
; |  x  |     |     |             
;  ----- ----- -----
; |  0  |  0  |     |
;  ----- ----- -----
; |  x  |     |     |
;  ----- ----- -----

(defrule siguiente-movimiento-intermedio-columna-IA
   
 ?h <- (siguiente-movimiento-intermedio-columna-IA)

   (celda (fila ?f1) (columna ?c1) (valor 1))
   (celda (fila ?f2&~?f1) (columna ?c1 ) (valor 0))
   (celda (fila ?f3&~?f2&~?f1) (columna ?c1 ) (valor 1))
   (no-activar ?a)
 =>
 (retract ?h)
         (if (neq ?a 2)then
         (assert (movimiento-maquina-c ?f2 ?c1 0))
         (assert (movimiento-maquina))
         )
)



;;------------------------------------------------------------------
;;------------------------------------------------------------------

;  ----- ----- -----
; |     |     |  x  |             
;  ----- ----- -----
; |     |  x  |     |
;  ----- ----- -----
; |  0  |     |  0  |
;  ----- ----- -----

(defrule siguiente-movimiento-diagonal-IA
   (declare (salience 89))
 ?h <- (siguiente-movimiento-diagonal-IA)

   (celda (fila ?f1) (columna ?c1) (valor 1))
   (celda (fila 2&~?f1) (columna 2&~?c1) (valor 1))
   (celda (fila ?f3&~2&~?f1) (columna ?c3&~2&~?c1) (valor 0))
   (no-activar ?a)
 =>
 (retract ?h)
         (if (neq ?a 2)then
         (assert (movimiento-maquina-c ?f3 ?c3 0))
         (assert (movimiento-maquina))
         )
)

;;------------------------------------------------------------------
;;------------------------------------------------------------------

;  ----- ----- -----
; |  x  |     |     |             
;  ----- ----- -----
; |     |  x  |     |
;  ----- ----- -----
; |  x  |     |  0  |
;  ----- ----- -----

(defrule siguiente-movimiento-diagonal-esquina-IA
   
 ?h <- (siguiente-movimiento-diagonal-esquina-IA)

   (celda (fila ?f1) (columna ?c1) (valor 1))
   (celda (fila ?f2&~?f1) (columna ?c2&~?c1) (valor 1))
   (celda (fila ?f3&~?f2&~?f1) (columna ?c3&~?c2&~?c1) (valor 2))
   (no-activar ?a)
 =>
 (retract ?h)
         (if (neq ?a 2)then
         (assert (movimiento-maquina-c ?f3 ?c1 0))
         (assert (movimiento-maquina))
         )
)


;;------------------------------------------------------------------
;;------------------------------------------------------------------

;  ----- ----- -----
; |  x  |     |  0  |             
;  ----- ----- -----
; |  0  |  0  |  x  |
;  ----- ----- -----
; |  x  |     |  0  |
;  ----- ----- -----

(defrule siguiente-movimiento-central-IA
   
 ?h <- (siguiente-movimiento-central-IA)

   (celda (fila ?f1) (columna ?c1) (valor 2))
   (celda (fila ?f1) (columna 2&~?c1) (valor 2))
   (celda (fila ?f1) (columna ?c3&~2&~?c1) (valor 1))

 =>
 (retract ?h)

         (assert (movimiento-maquina-c ?c3 ?c3 0))
         (assert (movimiento-maquina))
)

;;------------------------------------------------------------------
;;------------------------------------------------------------------

;  ----- ----- -----
; |  x  |  0  |  x  |             
;  ----- ----- -----
; |     |  0  |     |
;  ----- ----- -----
; |  0  |  x  |  0  |
;  ----- ----- -----

(defrule siguiente-movimiento-centrall-IA
   
 ?h <- (siguiente-movimiento-centrall-IA)

   (celda (fila ?f1) (columna ?c1) (valor 2))
   (celda (fila 2&~?f1) (columna ?c1) (valor 2))
   (celda (fila ?f3&~2&~?f1) (columna ?c1) (valor 1))

 =>
 (retract ?h)

         (assert (movimiento-maquina-c ?f3 ?f3 0))
         (assert (movimiento-maquina))
)


;;------------------------------------------------------------------
;;------------------------------------------------------------------

;  ----- ----- -----
; |  x  |  0  |  x  |             
;  ----- ----- -----
; |     |  0  |     |
;  ----- ----- -----
; |  0  |  x  |  0  |
;  ----- ----- -----

(defrule siguiente-movimiento-central-columna-IA
   
 ?h <- (siguiente-movimiento-central-columna-IA)

   (celda (fila ?f1) (columna ?c1) (valor 1))
   (celda (fila 2&~?f1) (columna ?c1) (valor 2))
   (celda (fila ?f3&~2&~?f1) (columna ?c1) (valor 0))
   (no-activar ?a)
 =>
 (retract ?h)
        (if (neq ?a 2)then
         (assert (movimiento-maquina-c ?f3 ?c1 0))
         (assert (movimiento-maquina))
        )
)

;;------------------------------------------------------------------
;;------------------------------------------------------------------

;  ----- ----- -----
; |  x  |  0  |     |             
;  ----- ----- -----
; |  0  |  0  |  x  |
;  ----- ----- -----
; |  x  |  x  |  0  |
;  ----- ----- -----

(defrule siguiente-movimiento-diagonal-inicio-IA
   
 ?h <- (siguiente-movimiento-diagonal-inicio-IA)

   (celda (fila ?f1) (columna ?c1) (valor 1))
   (celda (fila 2&~?f1) (columna 2&~?c1) (valor 2))
   (celda (fila ?f3&~2&~?f1) (columna ?c3&~2&~?c1) (valor 1))

 =>
 (retract ?h)

         (assert (movimiento-maquina-c ?f1 ?c3 0))
         (assert (movimiento-maquina))
)


;;------------------------------------------------------------------
;;------------------------------------------------------------------

;  ----- ----- -----
; |     |     |     |             
;  ----- ----- -----
; |     |     |     |
;  ----- ----- -----
; |     |     |     |
;  ----- ----- -----

(defrule siguiente-movimiento-fila-final-IA
   
 ?h <- (siguiente-movimiento-fila-final-IA)
   (contador ?c)

   (celda (fila ?f1) (columna ?c1) (valor 1))
   (celda (fila ?f1) (columna ?c2&~?c1) (valor 0))
   (celda (fila ?f1) (columna ?c3&~?c2&~?c1) (valor 2))
   (no-activar ?a)
 =>
 (retract ?h)
      (if  (and (= ?c 4) (neq ?a 2))then
         (assert (movimiento-maquina-c ?f1 ?c2 0))
         (assert (movimiento-maquina))
      )      
)

;;------------------------------------------------------------------
;;------------------------------------------------------------------

;  ----- ----- -----
; |  x  |  0  |     |             
;  ----- ----- -----
; |  0  |  0  |  x  |
;  ----- ----- -----
; |  x  |  x  |  0  |
;  ----- ----- -----

(defrule siguiente-movimiento-columna-final-IA
   
 ?h <- (siguiente-movimiento-columna-final-IA)
   (contador ?c)

   (celda (fila ?f1) (columna ?c1) (valor 1))
   (celda (fila ?f2&~?f1) (columna ?c1) (valor 0))
   (celda (fila ?f3&~?f2&~?f1) (columna ?c1) (valor 2))
  (no-activar ?a)
 =>
 (retract ?h)
      (if  (and (= ?c 4) (neq ?a 2))then
         (assert (movimiento-maquina-c ?f2 ?c1 0))
         (assert (movimiento-maquina))
      )      
)

;;------------------------------------------------------------------
;;------------------------------------------------------------------

(defrule finalizar-juego-gana-maquina

  (finalizar-juego-gana-maquina)
 
  =>
  (printout t crlf "--------------------GAME OVER--------------------"crlf)
  (printout t "------------------Ganan los '0'------------------" crlf )

  
  ;(halt)
  (assert (actualizar-tablero-finalizar))
  
)


;;------------------------------------------------------------------
;;------------------------------------------------------------------

;;Regla la cual me permite modificar una celda 

(defrule movimiento-maquina
  (declare (salience 88))
 ?h <- (movimiento-maquina)
 ?q <- (movimiento-maquina-c ?fila ?columna ?valor)
 ?m-maquina <- (celda (fila ?fila )(columna ?columna )(valor ?valor))
  ;(gana-IA ?i)
 =>
 (retract ?h)
  ;(if (neq ?i 1)then
    (modify ?m-maquina (valor 2))
    (assert (actualizar-tablero))
     
  ;else
   ;(assert (finalizar-juego-gana-maquina))
   
 ;)
 ;(retract ?m-maquina)
)



(defrule Tablero-completo-finalizar
  ;declare (salience 96))
  ?h <- (imprime ?i ?j&:(<= (* ?i ?j) 9))

  (Contenido-celda  (valor ?value)(parte1 ?p1)(parte2 ?p2)(parte3 ?p3)(parte4 ?p4))

  ?f <- (parteImprimir ?parteImprimir)

  ?g <- (actualizar-tablero-finalizar)

  

  (celda (fila ?i)(columna ?j)(valor ?value $?))
   =>
  
   (retract ?h)

    (if (and (= ?i 1) (= ?j 1) (= ?parteImprimir 1)) then
     (printout t crlf crlf" ------ ------ ------ "crlf)
    )

   
    (if (and (and (<= ?i 3) (< ?j 3)) (= ?parteImprimir 1))then
     (printout t "|" ?p1)
     (assert (imprime ?i (+ ?j 1)))
    )

    (if (and (and (<= ?i 3) (= ?j 3)) (= ?parteImprimir 1))then
      (printout t "|"?p1"|" crlf)
      (assert (imprime ?i 1))
      (assert (parteImprimir 2))
      (retract ?f)
    )

    (if (and (and (<= ?i 3) (< ?j 3)) (= ?parteImprimir 2))then
     (printout t "|" ?p2)
     (assert (imprime ?i (+ ?j 1)))
    )
     
    (if (and (and (<= ?i 3) (= ?j 3)) (= ?parteImprimir 2))then
      (printout t "|"?p2"|" crlf)
      (assert (imprime ?i 1))
      (assert (parteImprimir 3))
      (retract ?f)
    )
    
    (if (and (and (<= ?i 3) (< ?j 3)) (= ?parteImprimir 3))then
     (printout t "|" ?p3)
     (assert (imprime ?i (+ ?j 1)))
    )
    
    (if (and (and (<= ?i 3) (= ?j 3)) (= ?parteImprimir 3))then
      (printout t "|"?p3"| " ?i crlf)
      (assert (imprime ?i 1))
      (assert (parteImprimir 4))
      (retract ?f)
    )

    (if (and (and (<= ?i 3) (< ?j 3)) (= ?parteImprimir 4))then
     (printout t "|" ?p4)
     (assert (imprime ?i (+ ?j 1)))
    )

    (if (and (and (<= ?i 3) (= ?j 3)) (= ?parteImprimir 4))then
      (printout t "|"?p4"|"  crlf 
                  " ------ ------ ------ " crlf)
      (assert (imprime (+ ?i 1) 1))
      (assert (parteImprimir 1))
      (retract ?f)
    )

    (if (and (and (= ?i 3) (= ?j 3)) (= ?parteImprimir 4))then
     (printout t "    " (- ?i 2) "      " (- ?i 1) "      " ?i crlf crlf)
     (printout t crlf "Escribe el comando (Nuevo) para empezar una nueva partida..." crlf)
     (assert (parteImprimir 1))
     (retract ?g)
     (assert (imprime 1 1))
    ) 
  
)
 
(defrule finalizar-empate
    ?g <- (finalizar-empate)

 =>
 (retract ?g)
  (printout t crlf "--------------------EMPATE--------------------"crlf)
  (assert (actualizar-tablero-finalizar))
)

 
(defrule tablero-empate
    ?g <- (tablero-empate)
    (celda (fila ?f1) (columna ?c1) (valor 0))
    (celda (fila ?f1) (columna ?c1) (valor 0))
   
 =>
 (retract ?g)
  (printout t crlf "--------------------EMPATE--------------------"crlf)
  (assert (actualizar-tablero-finalizar))
  
  
)


 




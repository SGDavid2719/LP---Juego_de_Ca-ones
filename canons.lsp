; Genera el átomo escenari (x1-x2 x2-x3 x3-x4 y1 y2 y3 vent gravetat) -> Llama a (putprop) y a (random)
(defun generarescenari ()
	(putprop 'escenari (+ 100 (random (- 400 100))) 'x1-x2)
	(putprop 'escenari (+ 20 (random (- 150 20))) 'x2-x3)
	(putprop 'escenari (- 640 (+ (get 'escenari 'x1-x2)(get 'escenari 'x2-x3))) 'x3-x4)
	(putprop 'escenari (random 300) 'y2)
	(putprop 'escenari (+ (get 'escenari 'y2) (random (- 300 (get 'escenari 'y2)))) 'y1)
	(putprop 'escenari (random (get 'escenari 'y1)) 'y3)
	; Mediante un random decidimos el viento positivo o negativo
	(cond ((= (random 2) 0) (putprop 'escenari (- (random 5)) 'vent))
		(t (putprop 'escenari (random 5) 'vent))
	)
	; Hemos añadido una constante extra al escenario llamada gravetat
	(putprop 'escenari (- 0 9.8) 'gravetat)
)

; Genera cano y cano2 (ample alt boca posx posy angle)
; Las caracteristicas (ample, alt, boca) se podrían añadir a un átomo 'canogeneric para no repetir en todos los cañones
(defun generarcanons ()
	; cano
	(putprop 'cano '20 'ample)
	(putprop 'cano '10 'alt)
	(putprop 'cano '15 'boca)
	(putprop 'cano (+ (get 'cano 'ample) (random (- (get 'escenari 'x1-x2) (* 2 (get 'cano 'ample))))) 'posx)
	(putprop 'cano (get 'escenari 'y2) 'posy)
	(putprop 'cano (random 90) 'angle)
	; cano2
	(putprop 'cano2 '20 'ample)
	(putprop 'cano2 '10 'alt)
	(putprop 'cano2 '15 'boca)
	(putprop 'cano2 (+ (+ (get 'cano2 'ample) (+ (get 'escenari 'x1-x2)(get 'escenari 'x2-x3))) (random (- 640 (+ (* 2(get 'cano2 'ample)) (+ (get 'escenari 'x1-x2)(get 'escenari 'x2-x3)))))) 'posx)
	(putprop 'cano2 (get 'escenari 'y3) 'posy)
	(putprop 'cano2 (+ 90 (random (- 180 90))) 'angle)
)

; Pinta un rectágulo -> Llama a (drawrel) 
(defun rectangulo (xb y)
	(cond ((<= y 0) nil) 					; Altura
		(t (drawrel xb 0)					; Derecha (anchura)
			(drawrel 0 1)					; Sube
			(drawrel (- 0 xb) 0)			; Izquierda (- anchura)
			(drawrel 0 1)					; Sube
			(rectangulo xb (- y 2)))		; Llamada recursiva
	)
)

; Pintar las 3 montañas -> Llama a (rectangulo b h)
(defun pintamontanyes ()
	(move 0 0)
	; Verde - azul
	(color 0 156 23 0 115 202)																				
	(cls)
	(cleol)
	; Primer montículo
	(rectangulo (get 'escenari 'x1-x2) (get 'escenari 'y2))												
	(move (get 'escenari 'x1-x2) 0)
	; Segundo montículo
	(rectangulo (get 'escenari 'x2-x3) (get 'escenari 'y1))												
	(move (+ (get 'escenari 'x2-x3) (get 'escenari 'x1-x2)) 0)
	; Tercer montículo
	(rectangulo (get 'escenari 'x3-x4) (get 'escenari 'y3))												
)

; Pinta la boca de los cañones ->  Se coloca en el cañón pertinente, se pone a la mitad y sube igual a la altura. Llama a drawrel teniendo en cuenta el ángulo
; x = boca · sin α
; y = boca · cos α 
(defun pintaboca (canó)
	(move (+(get canó 'posx)(/(get canó 'ample) 2)) (+ (get canó 'posy)(get canó 'alt)))
	(drawrel (realpart (round (* (get canó 'boca) (cos (/ (* (get canó 'angle) pi) 180))))) (realpart (round (* (get canó 'boca) (sin (/ (* (get canó 'angle) pi) 180))))))
)

; Coloca el lápiz en la boca del cañón -> Son las mismas operaciones que en (pintaboca) pero sin llamar a (drawrel)
(defun colocalapiz (canó)
	(move (+(get canó 'posx)(/(get canó 'ample) 2)) (+ (get canó 'posy)(get canó 'alt)))
	(moverel (realpart (round (* (get canó 'boca) (cos (/ (* (get canó 'angle) pi) 180))))) (realpart (round (* (get canó 'boca) (sin (/ (* (get canó 'angle) pi) 180))))))
)

; Pinta los cañones -> Llama a (rectangulo) y a (pintaboca)
(defun pintacanons ()															
	; cano rojo - azul
	(color 156 38 0 0 115 202)	
	(move (get 'cano 'posx) (get 'cano 'posy))
	(rectangulo (get 'cano 'ample) (get 'cano 'alt))
	(pintaboca 'cano)
	; cano2 naranja - azul
	(color 255 168 51 0 115 202)	
	(move (get 'cano2 'posx) (get 'cano2 'posy))
	(rectangulo (get 'cano2 'ample) (get 'cano2 'alt))
	(pintaboca 'cano2)
)

; Pinta el número de líneas acorde al viento -> Llama a (pintavent) recursivamente
(defun pintavent (v x)
	(cond ((= v 0) nil)
		(t (cond ((< v 0) (move x 330) (drawrel 0 (- 0 5)) (pintavent (+ v 1) (- x 2)))
				(t (move x 330) (drawrel 0 (- 0 5)) (pintavent (- v 1) (+ x 2)))
			)
		)
	)
)

; Pinta la flecha del viento -> Pinta dependiendo del viento, si no hay no realiza ninguna operación. Llama a (pintavent)
(defun pintafletxa ()
	(cond ((= (get 'escenari 'vent) 0) nil)
		(t(color 255 255 255 0 115 202)
			(move 15 330)
			(drawrel 15 0)
			(cond ((< (get 'escenari 'vent) 0)
				(move 15 330)
				(drawrel 5 5)
				(moverel (- 0 5) (- 0 5))
				(drawrel 5 (- 0 5))
				(pintavent (get 'escenari 'vent) 30))
				(t (drawrel (- 0 5) 5)
					(moverel 5 (- 0 5))
					(drawrel (- 0 5) (- 0 5))
					(pintavent (get 'escenari 'vent) 15)
				)
			)	
		)
	)
)

; Pinta la explosión -> Se coloca en la posición del cañón explotado y pinta 
(defun pintaexplosion (canó)
	(cond ((equal canó 'cano) (move (get 'cano 'posx) (get 'cano 'posy)))
		(t (move (get 'cano2 'posx) (get 'cano2 'posy)))
	)
	(color 0 0 0 0 115 202)
	(moverel (- 0 8) 0)
	(drawrel 0 5)
	(drawrel 4 4)
	(drawrel 0 (- 0 5))
	(drawrel 4 (- 0 4))
	(color 128 41 159 0 115 202)
	(moverel 0 10)
	(drawrel 0 5)
	(drawrel 4 4)
	(drawrel 0 (- 0 5))
	(drawrel 4 (- 0 4))
	(color 0 0 0 0 115 202)
	(moverel 4 0)
	(drawrel 0 5)
	(drawrel 4 4)
	(drawrel 0 (- 0 5))
	(drawrel 4 (- 0 4))
	(color 128 41 159 0 115 202)
	(moverel 0 (- 0 10))
	(drawrel 0 5)
	(drawrel 4 4)
	(drawrel 0 (- 0 5))
	(drawrel 4 (- 0 4))
)

; Calcula la velocidad inicial -> Comprueba el eje primeramente
; v0x = v0 · cos α
; v0y = v0 · sin α
(defun velocidadinicial (canó velocitat eje)
	(cond ((equal eje 'x) (* velocitat (cos (/ (* (get canó 'angle) pi) 180))))
		(t (* velocitat (sin (/ (* (get canó 'angle) pi) 180))))
	)
)

; Calcula la velocidad en t (time) -> Es independiente al eje ya que varía la gravedad (a) y se pasa por parámetro
; vx = v0x + a·t (siendo "a" la velocidad del viento)
; vy = v0y + a·t (siendo "a" la gravedad)
(defun velocidad (canó velocitat eje time a)
	(+ (velocidadinicial canó velocitat eje) (* a time))
)

; Calcula la posición del proyectil -> Comprueba el eje primeramente
; x = x0 + vx·t
; y = y0 + vy·t + (1/2)·a·(t^2)
(defun posición (canó velocitat eje time)	
	(cond ((equal eje 'x) (+ (+ (+ (get canó 'posx)(/(get canó 'ample) 2)) (realpart (round (* (get canó 'boca) (cos (/ (* (get canó 'angle) pi) 180)))))) (* (velocidad canó velocitat eje time (get 'escenari 'vent)) time)))
		(t (+ (+ (+ (+ (get canó 'posy)(get canó 'alt)) (realpart (round (* (get canó 'boca) (sin (/ (* (get canó 'angle) pi) 180)))))) (* (velocidad canó velocitat eje time (get 'escenari 'gravetat)) time))(realpart (round (* (/ 1 2)(* (get 'escenari 'gravetat)(* time time)))))))
	)
)

; Función que pinta el proyectil -> Realiza llamadas recursivas, tiene en cuenta los límites (fin de la llamada recursiva), qué cañón realiza la acción y llama a (draw) 
(defun pintaproyectil (canó velocitat time)
	; x = 0 -> Límite por la izquierda
	(cond ((= (realpart (round (posición canó velocitat 'x time))) 0) nil)
		(t 
			; x < x2
			(cond ((<= (realpart (round (posición canó velocitat 'x time))) (get 'escenari 'x1-x2)) 
					; y > y2
					(cond ((> (realpart (round (posición canó velocitat 'y time))) (get 'escenari 'y2))
							; Pinta y llamada recursiva 
							(draw (realpart (round (posición canó velocitat 'x time))) (realpart (round (posición canó velocitat 'y time))))	
							(pintaproyectil canó velocitat (+ 0.05 time))
						)
						; y = y2
						(t
							; Pinta el proyectil
							(draw (realpart (round (posición canó velocitat 'x time))) (realpart (round (posición canó velocitat 'y time))))
							; cano2 disparando a cano
							(cond ((equal canó 'cano2) 
									; x = posx de cano
									(cond (( and (>= (realpart (round (posición canó velocitat 'x time))) (get 'cano 'posx))
												(<= (realpart (round (posición canó velocitat 'x time))) (+ (get 'cano 'posx) (get 'cano 'ample))))
												; Si el proyectil está sobre el área del cañón llama a (pintaexplosion)
												(cond ((<= (realpart (round (posición canó velocitat 'y time))) (+ (get 'cano 'posy) (get 'cano 'alt))) (pintaexplosion 'cano))
													(t nil)
												)
										)
										(t nil)
									)
								)
								(t nil)
							)
						)
					)
				)
				(t 
					; x <= x3
					(cond ((<= (realpart (round (posición canó velocitat 'x time))) (+ (get 'escenari 'x1-x2) (get 'escenari 'x2-x3)))
							; y <= y1
							(cond ((<= (realpart (round (posición canó velocitat 'y time))) (get 'escenari 'y1)) nil)
								; y > y1
								(t
									; Pinta y llamada recursiva 
									(draw (realpart (round (posición canó velocitat 'x time))) (realpart (round (posición canó velocitat 'y time))))	
									(pintaproyectil canó velocitat (+ 0.05 time))
								)
							)
						)
						(t
							; x = 640 -> Límite por la derecha
							(cond ((= (realpart (round (posición canó velocitat 'x time))) 640) nil)
								; x < 640
								(t 
									; y > y3
									(cond ((> (realpart (round (posición canó velocitat 'y time))) (get 'escenari 'y3))
											; Pinta y llamada recursiva 
											(draw (realpart (round (posición canó velocitat 'x time))) (realpart (round (posición canó velocitat 'y time))))	
											(pintaproyectil canó velocitat (+ 0.05 time))
										) 
										; y = y3 de cano
										(t
											; cano disparando a cano2
											(cond ((equal canó 'cano) 
													; x = posx de cano2
													(cond (( and (>= (realpart (round (posición canó velocitat 'x time))) (get 'cano2 'posx))
																(<= (realpart (round (posición canó velocitat 'x time))) (+ (get 'cano2 'posx) (get 'cano2 'ample))))
																; Si el proyectil está sobre el área del cañón llama a (pintaexplosion)
																(cond ((<= (realpart (round (posición canó velocitat 'y time))) (+ (get 'cano2 'posy) (get 'cano2 'alt))) (pintaexplosion 'cano2))
																	(t nil)
																)
														)
														(t nil)
													)
												)
												(t nil)
											)
										)
									)
								)
							)
						)
					)
				)
			)
		)
	)
)

; Se llama recursivamente y permite introducir los comandos en la misma línea
(defun repetir (x) (goto-xy 0 0) (cleol) (repetir (eval (read))))

; Función principal -> Genera el escenario y sus respectivos átomos
(defun pinta ()
	(generarescenari)
	(generarcanons)
	(pintamontanyes)
	(pintacanons)
	(pintafletxa)
	(repetir (eval (read)))
)

; Dispara -> Se coloca el lápiz sobre el cañón correspondiente, se evalua el cañón para el color y se llama a (pintaproyectil)
(defun simula (canó velocitat)
	(colocalapiz canó)
	(cond ((equal canó 'cano) (color 156 38 0 0 115 202))
		(t (color 255 168 51 0 115 202))
	)
	(pintaproyectil canó velocitat 0)
)

; Sube la boca del cañón -> Pinta la boca previa del color del fondo y se vuelve a pintar la boca modificando el ángulo previamente
(defun puja (canó graus)
	(color 0 115 202 0 115 202)
	(pintaboca canó)
	(cond ((equal canó 'cano2) (putprop canó (- (get canó 'angle) graus) 'angle) (color 255 168 51 0 115 202))
		(t (putprop canó (+ (get canó 'angle) graus) 'angle) (color 156 38 0 0 115 202))
	)
	(pintaboca canó)
)

; Baja la boca del cañón -> Pinta la boca previa del color del fondo y se vuelve a pintar la boca modificando el ángulo previamente
(defun baixa (canó graus)
	(color 0 115 202 0 115 202)
	(pintaboca canó)
	(cond ((equal canó 'cano2) (putprop canó (+ (get canó 'angle) graus) 'angle) (color 255 168 51 0 115 202))
		(t (putprop canó (- (get canó 'angle) graus) 'angle) (color 156 38 0 0 115 202))
	)
	(pintaboca canó)
)
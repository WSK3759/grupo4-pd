# README:

Un módulo es un fichero .hs que agrupa tipos y funciones. Así el proyecto se reparte en piezas pequeñas (Tipos, Objeto, Jugador, Nivel, Partida, Mundo, Render, Main) y cada una importa solo lo que necesita, sin ciclos: Tipos no depende de nadie y el Main está arriba del todo.

Una lista de exportación es la lista entre paréntesis tras module Nombre. Solo lo que aparece ahí es visible desde fuera, el resto es privado. Por eso en Nivel se exportan parsearFichero o buscarClave, pero no los auxiliares buscarOError, maybeAEither o separarCabecera, que son detalles internos.

Por qué ocultar el constructor de Jugador. El módulo Jugador exporta el tipo como Jugador y no como Jugador(..). Fuera de ese módulo nadie puede escribir Jugador (0,0) (0,0) (-5) 0. La única forma de obtener uno es crearJugador, que siempre empieza con 3 vidas, y perderVida nunca baja de 0. Así la regla "las vidas nunca son negativas" se cumple siempre porque solo se puede tocar un jugador a través de funciones que la respetan. 

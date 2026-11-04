# Organización del proyecto en módulos

Un módulo en Haskell permite organizar el código de un programa en distintos archivos, agrupando los tipos y las funciones relacionados. En nuestro proyecto hemos separado los datos básicos, los objetos, el jugador, los niveles, las partidas y la representación gráfica.

La lista de exportación de un módulo indica qué elementos pueden utilizar los demás módulos que lo importan. De esta manera, podemos mostrar únicamente las funciones y los tipos necesarios y mantener ocultos otros detalles internos.

En el módulo Jugador exportamos el tipo Jugador sin su constructor. Esto impide que otro módulo cree directamente un jugador con valores incorrectos, por ejemplo, un número negativo de vidas.

En su lugar, los jugadores deben crearse mediante crearJugador y modificarse usando las funciones proporcionadas, como perderVida, sumarPuntos y conVelocidad. Así protegemos las invariantes del tipo y mantenemos controlada la forma en que cambia su estado.
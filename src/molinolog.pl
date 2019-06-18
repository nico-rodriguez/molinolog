:- use_module(graficos).

% Este archivo se provee como una guía para facilitar la implementación y 
% entender el uso de graficos.pl
% El contenido de este archivo se puede modificar.

% El predicado minimax_depth/1 define la recursión máxima a utilizar en el algoritmo minimax
minimax_depth(2).

% molinolog/3 <- +JugadorNegro, +JugadorBlanco, +T
% JugadorNegro y JugadorBlanco pueden ser los átomos humano o maquina.
% T es el tamaño del tablero.
molinolog(JugadorNegro,JugadorBlanco,T) :-
	pce_image_directory(icons),
	gr_crear(Visual, T, [
		     boton('Reiniciar',reiniciar),
		     boton('Salir',salir)] % salir puede ser por el boton o por el click en la ventana
		 ),
    iniciar_juego(Visual,JugadorNegro,JugadorBlanco,T),
    !,
    gr_destruir(Visual).

% iniciar_juego/4 <- +Visual, +JugadorNegro, +JugadorBlanco, +T
iniciar_juego(Visual,JugadorNegro,JugadorBlanco,T):-
    iniciar_estado(T,Estado),
    fichas(Estado,Fichas),
    gr_dibujar_tablero(Visual,T,Fichas),
    loop(Visual,negro,JugadorNegro,JugadorBlanco,T, Estado).

contrincante(negro,blanco).
contrincante(blanco,negro).

% iniciar_estado/2 <- +T, ?E 
% T es el tamaño del tablero.
% E es el functor que representa un estado del juego.
% Tiene aridad 6 con la forma estado(Fase,Fichas,FichasColocarNegro,FichasColocarBlanco,FichasMoverNegro,FichasMoverBlanco).
% Fase puede ser los átomos colocar o mover.
% Fichas es una lista con todas las fichas en el tablero, cada elemento tiene la forma ficha(Turno, Dir, Dist).
% FichasColocarNegro es el numero de fichas que le quedan por colocar al jugador negro (comienza en 3*(T+1)).
% FichasColocarBlanco es el numero de fichas que le quedan por colocar al jugador blanco (comienza en 3*(T+1)).
% FichasMoverNegro es el numero de fichas que le quedan por mover al jugador negro (comienza en 3*(T+1)).
% FichasMoverBlanco es el numero de fichas que le quedan por mover al jugador blanco (comienza en 3*(T+1)).
iniciar_estado(T,E) :- 
    functor(E,estado,6),
    CantidadFichas is 3*(T+1),
    arg(1,E,colocar),
    arg(2,E,[]),
    arg(3,E,CantidadFichas),
    arg(4,E,CantidadFichas),
    arg(5,E,CantidadFichas),
    arg(6,E,CantidadFichas).

% fase/2 <- +Estado, ?Fase
fase(Estado,Fase) :- arg(1,Estado,Fase).

% fichas/2 <- +Estado, ?Fichas
fichas(Estado,Fichas) :- arg(2,Estado,Fichas).

% colocar_fichas_restantes/3 <- +Estado, ?Jugador, ?CantidadDeFichas
% Jugador puede ser los átomos humano o maquina.
colocar_fichas_restantes(Estado, negro, CantidadFichas) :- arg(3,Estado,CantidadFichas).
colocar_fichas_restantes(Estado, blanco, CantidadFichas) :- arg(4,Estado,CantidadFichas).

% mover_fichas_restantes/3 <- +Estado, ?Jugador, ?CantidadDeFichas
% Jugador puede ser los átomos humano o maquina.
mover_fichas_restantes(Estado, negro, CantidadFichas) :- arg(5,Estado,CantidadFichas).
mover_fichas_restantes(Estado, blanco, CantidadFichas) :- arg(6,Estado,CantidadFichas).

% colocar_ficha/5 <- +EstadoInicial, +Jugador, +Dir, +Dist, ?EstadoFinal
% EstadoFinal es el resultado de colocar una ficha en la posición (Dir,Dist) en el tablero representado por EstadoInicial para el jugador Jugador
colocar_ficha(EstadoInicial,negro,Dir,Dist,EstadoFinal) :- 
    fichas(EstadoInicial, Fichas),
    colocar_fichas_restantes(EstadoInicial, negro, CantidadFichasColocarNegro),
    CantidadFichasColocarNegroFinal is CantidadFichasColocarNegro-1, 
    colocar_fichas_restantes(EstadoInicial, blanco, CantidadFichasColocarBlanco),
    mover_fichas_restantes(EstadoInicial, negro, CantidadFichasMoverNegro),
    mover_fichas_restantes(EstadoInicial, blanco, CantidadFichasMoverBlanco),
	functor(EstadoFinal,estado,6),
    siguiente_fase(CantidadFichasColocarNegroFinal,CantidadFichasColocarBlanco,SiguienteFase),
    arg(1,EstadoFinal,SiguienteFase),
    arg(1,EstadoFinal,SiguienteFase),
    arg(2,EstadoFinal,[ficha(negro,Dir,Dist)|Fichas]),
    arg(3,EstadoFinal,CantidadFichasColocarNegroFinal),
    arg(4,EstadoFinal,CantidadFichasColocarBlanco),
    arg(5,EstadoFinal,CantidadFichasMoverNegro),
    arg(6,EstadoFinal,CantidadFichasMoverBlanco).
colocar_ficha(EstadoInicial,blanco,Dir,Dist,EstadoFinal) :- 
    fichas(EstadoInicial, Fichas),
    colocar_fichas_restantes(EstadoInicial, negro, CantidadFichasColocarNegro),
    colocar_fichas_restantes(EstadoInicial, blanco, CantidadFichasColocarBlanco),
    CantidadFichasColocarBlancoFinal is CantidadFichasColocarBlanco-1, 
    mover_fichas_restantes(EstadoInicial, negro, CantidadFichasMoverNegro),
    mover_fichas_restantes(EstadoInicial, blanco, CantidadFichasMoverBlanco),
	functor(EstadoFinal,estado,6),
    siguiente_fase(CantidadFichasColocarNegro,CantidadFichasColocarBlancoFinal,SiguienteFase),
    arg(1,EstadoFinal,SiguienteFase),
    arg(2,EstadoFinal,[ficha(blanco,Dir,Dist)|Fichas]),
    arg(3,EstadoFinal,CantidadFichasColocarNegro),
    arg(4,EstadoFinal,CantidadFichasColocarBlancoFinal),
    arg(5,EstadoFinal,CantidadFichasMoverNegro),
    arg(6,EstadoFinal,CantidadFichasMoverBlanco).

% siguiente_fase/3 <- +CantidadFichasColocarNegro, +CantidadFichasColocarBlanco, ?Fase
% Si no restan fichas por colocar, la siguiente fase es mover.
siguiente_fase(0,0,mover):-!.
siguiente_fase(_,_,colocar).

% colocar_ficha/5 <- +Estado, +Ficha
% evalua si Ficha existe en el tablero representado por Estado
existe_ficha(Estado,ficha(Turno,Dir,Dist)) :- fichas(Estado,Fichas), member(ficha(Turno,Dir,Dist),Fichas). 

% quitar_ficha/5 <- +EstadoInicial, +Jugador, +Dir, +Dist, ?EstadoFinal
% EstadoFinal es el resultado de quitar una ficha en la posición (Dir,Dist) en el tablero representado por EstadoInicial para el jugador Jugador
quitar_ficha(EstadoInicial,negro,Dir,Dist,EstadoFinal) :- 
    existe_ficha(EstadoInicial,ficha(negro,Dir,Dist)),
    fase(EstadoInicial,Fase),
    fichas(EstadoInicial, Fichas),
    delete(Fichas,ficha(negro,Dir,Dist), FichasFinal),
    colocar_fichas_restantes(EstadoInicial, negro, CantidadFichasColocarNegro),
    colocar_fichas_restantes(EstadoInicial, blanco, CantidadFichasColocarBlanco),
    mover_fichas_restantes(EstadoInicial, negro, CantidadFichasMoverNegro),
    CantidadFichasMoverNegroFinal is CantidadFichasMoverNegro-1,
    mover_fichas_restantes(EstadoInicial, blanco, CantidadFichasMoverBlanco),
	functor(EstadoFinal,estado,6),
    arg(1,EstadoFinal,Fase),
    arg(2,EstadoFinal,FichasFinal),
    arg(3,EstadoFinal,CantidadFichasColocarNegro),
    arg(4,EstadoFinal,CantidadFichasColocarBlanco),
    arg(5,EstadoFinal,CantidadFichasMoverNegroFinal),
    arg(6,EstadoFinal,CantidadFichasMoverBlanco).
quitar_ficha(EstadoInicial,blanco,Dir,Dist,EstadoFinal) :- 
    existe_ficha(EstadoInicial,ficha(blanco,Dir,Dist)),
    fase(EstadoInicial,Fase),
    fichas(EstadoInicial, Fichas),
    delete(Fichas,ficha(blanco,Dir,Dist), FichasFinal),
    colocar_fichas_restantes(EstadoInicial, negro, CantidadFichasColocarNegro),
    colocar_fichas_restantes(EstadoInicial, blanco, CantidadFichasColocarBlanco),
    mover_fichas_restantes(EstadoInicial, negro, CantidadFichasMoverNegro),
    mover_fichas_restantes(EstadoInicial, blanco, CantidadFichasMoverBlanco),
    CantidadFichasMoverBlancoFinal is CantidadFichasMoverBlanco-1,
	functor(EstadoFinal,estado,6),
    arg(1,EstadoFinal,Fase),
    arg(2,EstadoFinal,FichasFinal),
    arg(3,EstadoFinal,CantidadFichasColocarNegro),
    arg(4,EstadoFinal,CantidadFichasColocarBlanco),
    arg(5,EstadoFinal,CantidadFichasMoverNegro),
    arg(6,EstadoFinal,CantidadFichasMoverBlancoFinal).

% correr_ficha/7 <- +EstadoInicial, +Jugador, +Dir, +Dist, ?EstadoFinal
% EstadoFinal es el resultado de mover una ficha de la posición (Dir,Dist) a la posición (NewDir,NewDist) en el tablero representado por EstadoInicial para el jugador Jugador
correr_ficha(EstadoInicial,Turno,Dir,Dist,NewDir,NewDist,EstadoFinal) :- 
    existe_ficha(EstadoInicial,ficha(Turno,Dir,Dist)),
    fase(EstadoInicial,mover),
    fichas(EstadoInicial, Fichas),
    delete(Fichas,ficha(Turno,Dir,Dist), FichasFinal),
    colocar_fichas_restantes(EstadoInicial, negro, CantidadFichasColocarNegro),
    colocar_fichas_restantes(EstadoInicial, blanco, CantidadFichasColocarBlanco),
    mover_fichas_restantes(EstadoInicial, negro, CantidadFichasMoverNegro),
    mover_fichas_restantes(EstadoInicial, blanco, CantidadFichasMoverBlanco),
	functor(EstadoFinal,estado,6),
    arg(1,EstadoFinal,mover),
    arg(2,EstadoFinal,[ficha(Turno,NewDir,NewDist)|FichasFinal]),
    arg(3,EstadoFinal,CantidadFichasColocarNegro),
    arg(4,EstadoFinal,CantidadFichasColocarBlanco),
    arg(5,EstadoFinal,CantidadFichasMoverNegro),
    arg(6,EstadoFinal,CantidadFichasMoverBlanco).

% evaluar_tablero/3 <- ?Jugador, +Estado, ?Valor
% Valor es el resultado de evaluar el tablero representado por Estado para el jugador Jugador.
% La evaluación es fichas restantes del jugador menos las fichas restantes del contrincante
evaluar_tablero(negro,Estado,Valor) :- 
    mover_fichas_restantes(Estado, negro, CantidadFichasMoverNegro),
    mover_fichas_restantes(Estado, blanco, CantidadFichasMoverBlanco),
    Valor is CantidadFichasMoverNegro-CantidadFichasMoverBlanco.
evaluar_tablero(blanco,Estado,Valor) :- 
    mover_fichas_restantes(Estado, negro, CantidadFichasMoverNegro),
    mover_fichas_restantes(Estado, blanco, CantidadFichasMoverBlanco),
    Valor is CantidadFichasMoverBlanco-CantidadFichasMoverNegro.

% fichas_jugador/3 <- +Turno, +Estado, ?FichasJugador
% FichasJugador es la lista de fichas de Estado filtradas por el jugador Turno
fichas_jugador(Turno,Estado,FichasJugador) :- 
    fichas(Estado,Fichas),
    fichas_jugador_Aux(Turno,Fichas,FichasJugador).
fichas_jugador_Aux(_,[],[]).
fichas_jugador_Aux(Turno,[C|R],[C|Y]) :- arg(1,C,Turno), fichas_jugador_Aux(Turno,R,Y),!.
fichas_jugador_Aux(Turno,[_|R],FichasJugador) :- fichas_jugador_Aux(Turno,R,FichasJugador).

% --------------------------
% Loop principal
% --------------------------

% loop/6 <- +Visual, +Turno, +JugadorNegro, +JugadorBlanco, +T, +Estado
loop(Visual,Turno,JugadorNegro,JugadorBlanco,T, Estado) :-
    verificar_fin(Estado,Visual,JugadorNegro,JugadorBlanco,T),
    mensaje(Visual,Turno,Estado),
    ejecutar_turno(Visual,Turno,JugadorNegro,JugadorBlanco,T,Estado).

% ejecutar_turno/6 <- +Visual, +Turno, +JugadorNegro, +JugadorBlanco, +T, +Estado
ejecutar_turno(Visual,negro,humano,JugadorBlanco,T,Estado) :-
    gr_evento(Visual,Evento),
    evento(Evento,Visual,negro,humano,JugadorBlanco,T,Estado).
ejecutar_turno(Visual,blanco,JugadorNegro,humano,T,Estado) :-
    gr_evento(Visual,Evento),
    evento(Evento,Visual,blanco,JugadorNegro,humano,T,Estado).
ejecutar_turno(Visual,negro,maquina,JugadorBlanco,T,Estado) :-
    elegir_movimiento(Estado, EstadoFinal,T,negro),
    fichas(EstadoFinal,Fichas),
    gr_dibujar_tablero(Visual,T,Fichas),
    loop(Visual,blanco,maquina,JugadorBlanco,T,EstadoFinal).
ejecutar_turno(Visual,blanco,JugadorNegro,maquina,T,Estado) :-
    elegir_movimiento(Estado, EstadoFinal,T,blanco),
    fichas(EstadoFinal,Fichas),
    gr_dibujar_tablero(Visual,T,Fichas),
    loop(Visual,negro,JugadorNegro,maquina,T,EstadoFinal).

% mensaje/3 <- +Visual, +Turno, +Estado
mensaje(Visual,Turno,Estado) :-
    fase(Estado,colocar),
    colocar_fichas_restantes(Estado,Turno,Cantidad),
    sformat(Msg, 'Jugador ~w, colocar ~D fichas', [Turno,Cantidad]),
    gr_estado(Visual, Msg).
mensaje(Visual,Turno,Estado) :-
    fase(Estado,mover),
    sformat(Msg, 'Jugador ~w, mover', [Turno]),
    gr_estado(Visual, Msg).

% evento/7 <- +Evento, +Visual, +Turno, +JugadorNegro, +JugadorBlanco, +T, +Estado
evento(click(Dir,Dist),Visual,Turno,JugadorNegro,JugadorBlanco,T,Estado) :-
    fase(Estado,colocar),
    posicion_libre(Estado,Dir,Dist),
    !,
    gr_ficha(Visual,T,Dir,Dist,Turno),
    colocar_ficha(Estado,Turno,Dir,Dist,EstadoIntermedio),
    chequear_molino(Dir,Dist,Visual,Turno,T,EstadoIntermedio,EstadoFinal),
    contrincante(Turno,SiguienteTurno),
    loop(Visual,SiguienteTurno,JugadorNegro,JugadorBlanco,T,EstadoFinal).
evento(click(Dir,Dist),Visual,Turno,JugadorNegro,JugadorBlanco,T,Estado) :-
    fase(Estado,colocar),
    \+ posicion_libre(Estado,Dir,Dist),
    !,
    gr_mensaje(Visual,'Posicion ocupada, seleccione otra.'),
    loop(Visual,Turno,JugadorNegro,JugadorBlanco,T,Estado).
evento(click(Dir,Dist),Visual,Turno,JugadorNegro,JugadorBlanco,T,Estado) :-
    fase(Estado,mover),
    existe_ficha(Estado,ficha(Turno,Dir,Dist)),
    !,
    (
        mover_ficha(Visual,T,Estado,Turno,Dir,Dist,EstadoFinal) -> true ; 
        fichas(Estado,Fichas),
        gr_dibujar_tablero(Visual,T,Fichas),
        loop(Visual,Turno,JugadorNegro,JugadorBlanco,T,Estado)
    ),
    contrincante(Turno,SiguienteTurno),
    loop(Visual,SiguienteTurno,JugadorNegro,JugadorBlanco,T,EstadoFinal).
evento(click(Dir,Dist),Visual,Turno,JugadorNegro,JugadorBlanco,T,Estado) :-
    fase(Estado,mover),
    \+ existe_ficha(Estado,ficha(Turno,Dir,Dist)),
    !,
    sformat(Msg, 'Debes elegir una ficha color ~w.', [Turno]),
    gr_mensaje(Visual,Msg),
    loop(Visual,Turno,JugadorNegro,JugadorBlanco,T,Estado).
evento(salir,Visual,Turno,JugadorNegro,JugadorBlanco,T,Estado) :-
    (   gr_opciones(Visual, '¿Seguro?', ['Sí', 'No'], 'Sí')
	->  true
	;   loop(Visual,Turno,JugadorNegro,JugadorBlanco,T,Estado)
	).
evento(reiniciar,Visual,Turno,JugadorNegro,JugadorBlanco,T,Estado) :-
    (   gr_opciones(Visual, '¿Seguro?', ['Sí', 'No'], 'Sí')
	->  % reiniciar el juego
        iniciar_juego(Visual,JugadorNegro,JugadorBlanco,T)
	;   loop(Visual,Turno,JugadorNegro,JugadorBlanco,T,Estado)
	).

% posicion_libre/3 <- +Estado, +Dir, +Dist
% indica si la posición (Dir,Dist) esta libre en el tablero representado por Estado
posicion_libre(Estado, Dir, Dist) :- fichas(Estado,Fichas), posicion_libre_aux(Fichas,Dir,Dist).
posicion_libre_aux([],_,_).
posicion_libre_aux([C|R],Dir,Dist) :- 
    arg(2,C,Dir1),
    arg(3,C,Dist1),
    posiciones_distintas((Dir1,Dist1),(Dir,Dist)),
    posicion_libre_aux(R,Dir,Dist).

% posiciones_distintas/2 <- +Pos1, +Pos2
% indica si Pos1 es distinta a Pos2
posiciones_distintas((Dir1,Dist1),(Dir2,Dist2)) :- Dir1 \= Dir2, Dist1 \= Dist2.
posiciones_distintas((Dir1,Dist),(Dir2,Dist)) :- Dir1 \= Dir2.
posiciones_distintas((Dir,Dist1),(Dir,Dist2)) :- Dist1 \= Dist2.

% chequear_molino/7 <- +Dir, +Dist, +Visual, +Turno, +T, +EstadoIncial, ?EstadoFinal
% chequea si la colocación de una ficha en la posición (Dir,Dist) generó un nuevo molino, y en caso afirmativo delega al usuario el seleccionar que ficha del oponente capturar
% EstadoFinal es instanciado con el resultado de la selección del usuario de que ficha del oponente capturar, en caso de que haya molinos nuevos, y sino es igual a EstadoInicial
chequear_molino(Dir,Dist,Visual,Turno,T,EstadoInicial,EstadoFinal) :-
    findall([ficha(Turno,Dir1,Dist1),ficha(Turno,Dir2,Dist2),ficha(Turno,Dir3,Dist3)],
            (member(ficha(Turno,Dir,Dist),[ficha(Turno,Dir1,Dist1),ficha(Turno,Dir2,Dist2),ficha(Turno,Dir3,Dist3)]),molino(EstadoInicial,ficha(Turno,Dir1,Dist1),ficha(Turno,Dir2,Dist2),ficha(Turno,Dir3,Dist3),T)),
            MolinosNuevos),
    length(MolinosNuevos,N),
    N>0,
    [[ficha(_,Dir11,Dist11),ficha(_,Dir12,Dist12),ficha(_,Dir13,Dist13)]|_] = MolinosNuevos,
    gr_ficha(Visual,T,Dir11,Dist11,'seleccion'),
    gr_ficha(Visual,T,Dir12,Dist12,'seleccion'),
    gr_ficha(Visual,T,Dir13,Dist13,'seleccion'),
    capturar_ficha(Visual,Turno,T,EstadoInicial,EstadoFinal).
chequear_molino(Dir,Dist,_,Turno,T,Estado,Estado) :-
    findall([ficha(Turno,Dir1,Dist1),ficha(Turno,Dir2,Dist2),ficha(Turno,Dir3,Dist3)],
            (member(ficha(Turno,Dir,Dist),[ficha(Turno,Dir1,Dist1),ficha(Turno,Dir2,Dist2),ficha(Turno,Dir3,Dist3)]),molino(Estado,ficha(Turno,Dir1,Dist1),ficha(Turno,Dir2,Dist2),ficha(Turno,Dir3,Dist3),T)),
            MolinosNuevos),
    length(MolinosNuevos,N),
    N=0.

% molino/5 <- +Estado, +Ficha1, +Ficha2, +Ficha3, +T
% Molinos horizontales
molino(Estado,ficha(Turno,nw,N),ficha(Turno,n,N),ficha(Turno,ne,N),_) :-
    existe_ficha(Estado,ficha(Turno,nw,N)),
    existe_ficha(Estado,ficha(Turno,n,N)),
    existe_ficha(Estado,ficha(Turno,ne,N)).
molino(Estado,ficha(Turno,w,Dist1),ficha(Turno,w,Dist2),ficha(Turno,w,Dist3),T) :-
    T1 is T - 1,
    between(1,T1,Dist3),
    Dist2 is Dist3 + 1, Dist1 is Dist2 + 1,
    existe_ficha(Estado,ficha(Turno,w,Dist1)),
    existe_ficha(Estado,ficha(Turno,w,Dist2)),
    existe_ficha(Estado,ficha(Turno,w,Dist3)).
molino(Estado,ficha(Turno,e,Dist1),ficha(Turno,e,Dist2),ficha(Turno,e,Dist3),T) :-
    T1 is T - 1,
    between(1,T1,Dist1),
    Dist2 is Dist1 + 1, Dist3 is Dist2 + 1,
    existe_ficha(Estado,ficha(Turno,e,Dist1)),
    existe_ficha(Estado,ficha(Turno,e,Dist2)),
    existe_ficha(Estado,ficha(Turno,e,Dist3)).
molino(Estado,ficha(Turno,sw,N),ficha(Turno,s,N),ficha(Turno,se,N),_) :-
    existe_ficha(Estado,ficha(Turno,sw,N)),
    existe_ficha(Estado,ficha(Turno,s,N)),
    existe_ficha(Estado,ficha(Turno,se,N)).
% Molinos verticales
molino(Estado,ficha(Turno,nw,N),ficha(Turno,w,N),ficha(Turno,sw,N),_) :-
    existe_ficha(Estado,ficha(Turno,nw,N)),
    existe_ficha(Estado,ficha(Turno,w,N)),
    existe_ficha(Estado,ficha(Turno,sw,N)).
molino(Estado,ficha(Turno,n,Dist1),ficha(Turno,n,Dist2),ficha(Turno,n,Dist3),T) :-
    T1 is T - 1,
    between(1,T1,Dist3),
    Dist2 is Dist3 + 1, Dist1 is Dist2 + 1,
    existe_ficha(Estado,ficha(Turno,n,Dist1)),
    existe_ficha(Estado,ficha(Turno,n,Dist2)),
    existe_ficha(Estado,ficha(Turno,n,Dist3)).
molino(Estado,ficha(Turno,s,Dist1),ficha(Turno,s,Dist2),ficha(Turno,s,Dist3),T) :-
    T1 is T - 1,
    between(1,T1,Dist1),
    Dist2 is Dist1 + 1, Dist3 is Dist2 + 1,
    existe_ficha(Estado,ficha(Turno,s,Dist1)),
    existe_ficha(Estado,ficha(Turno,s,Dist2)),
    existe_ficha(Estado,ficha(Turno,s,Dist3)).
molino(Estado,ficha(Turno,ne,N),ficha(Turno,e,N),ficha(Turno,se,N),_) :-
    existe_ficha(Estado,ficha(Turno,ne,N)),
    existe_ficha(Estado,ficha(Turno,e,N)),
    existe_ficha(Estado,ficha(Turno,se,N)).

% capturar_ficha/5 <- +Visual, +Turno, +T, +EstadoIncial, ?EstadoFinal
% permite al usuario capturar una ficha del oponente en caso de que formo un molino
% se chequea que la posición seleccionada tenga una ficha del oponente
capturar_ficha(Visual,Turno,T,EstadoInicial,EstadoFinal) :-
    sformat(Msg,'Jugador ~w, capturar',[Turno]),
    gr_estado(Visual,Msg),
    gr_evento(Visual,click(Dir,Dist)),
    contrincante(Turno,Contrincante),
    (existe_ficha(EstadoInicial,ficha(Contrincante,Dir,Dist))
      -> true
      ; sformat(Msg2,'Debes seleccionar una ficha color ~w.',[Contrincante]),
	    gr_mensaje(Visual,Msg2),
	    false
    ),
    !,
    quitar_ficha(EstadoInicial,Contrincante,Dir,Dist,EstadoFinal),
    fichas(EstadoFinal,Fichas),
    gr_dibujar_tablero(Visual,T,Fichas).
capturar_ficha(Visual,Turno,T,EstadoInicial,EstadoFinal) :-
    capturar_ficha(Visual,Turno,T,EstadoInicial,EstadoFinal).

% capturar_ficha/5 <- +Visual, +Turno, +T, +EstadoIncial, ?EstadoFinal
% verifica si Estado representa un juego que ha finalizado. En caso de que sí, se muestra una notificación indicando quien gano y se pregunta si se quiere repetir la partida
verificar_fin(Estado,Visual,JugadorNegro,JugadorBlanco,T):-
    mover_fichas_restantes(Estado, negro, CantidadFichas),
    CantidadFichas < 3,
    (   gr_opciones(Visual, 'Ha ganado el jugador blanco, jugar nuevamente?', ['Sí', 'No'], 'Sí')
	->  % reiniciar el juego
        iniciar_juego(Visual,JugadorNegro,JugadorBlanco,T)
	; halt
    ).
verificar_fin(Estado,Visual,JugadorNegro,JugadorBlanco,T):-
    mover_fichas_restantes(Estado, blanco, CantidadFichas),
    CantidadFichas < 3,
    (   gr_opciones(Visual, 'Ha ganado el jugador negro, jugar nuevamente?', ['Sí', 'No'], 'Sí')
	->  % reiniciar el juego
        iniciar_juego(Visual,JugadorNegro,JugadorBlanco,T)
	; halt
    ).
verificar_fin(_,_,_,_,_).

% posicion_adyacente/5 <- +Dir, +Dist, +DirAdy, +DistAdy, +T
posicion_adyacente(Dir,Dist,Dir,AdyDist,T) :-
    member(Dir,[w,e,n,s]),
    Dist is T+1,
    AdyDist is Dist-1.
posicion_adyacente(Dir,1,Dir,AdyDist,_) :-
    member(Dir,[w,e,n,s]),
    AdyDist is 2.
posicion_adyacente(Dir,Dist,Dir,AdyDist,T) :-
    member(Dir,[w,e,n,s]),
    Dist > 1, Dist < T+1,
    AdyDist is Dist-1.
posicion_adyacente(Dir,Dist,Dir,AdyDist,T) :-
    member(Dir,[w,e,n,s]),
    Dist > 1, Dist < T+1,
    AdyDist is Dist+1.
posicion_adyacente(w,Dist,nw,Dist,_).
posicion_adyacente(w,Dist,sw,Dist,_).
posicion_adyacente(n,Dist,nw,Dist,_).
posicion_adyacente(n,Dist,ne,Dist,_).
posicion_adyacente(e,Dist,ne,Dist,_).
posicion_adyacente(e,Dist,se,Dist,_).
posicion_adyacente(s,Dist,sw,Dist,_).
posicion_adyacente(s,Dist,se,Dist,_).
posicion_adyacente(nw,Dist,w,Dist,_).
posicion_adyacente(nw,Dist,n,Dist,_).
posicion_adyacente(ne,Dist,n,Dist,_).
posicion_adyacente(ne,Dist,e,Dist,_).
posicion_adyacente(se,Dist,s,Dist,_).
posicion_adyacente(se,Dist,e,Dist,_).
posicion_adyacente(sw,Dist,w,Dist,_).
posicion_adyacente(sw,Dist,s,Dist,_).

% mover_ficha/5 <- +Visual, +T, +EstadoIncial, +Turno, +Dir, +Dist, ?EstadoFinal
% permite al usuario mover una ficha una posición adyacente
% se chequea que la posición seleccionada origen tenga una ficha del jugador y que la posicion destino sea adyacente y este libre
% tambien se chequea si el movimiento genero un molino, y en caso afirmativo se le permite al usuario elegir una ficha del oponente para capturar
mover_ficha(Visual,T,EstadoInicial,Turno,Dir,Dist,EstadoFinal) :-
    gr_ficha(Visual,T,Dir,Dist,'seleccion'),
    gr_evento(Visual,click(NewDir,NewDist)),
    ( posicion_adyacente(Dir,Dist,NewDir,NewDist,T)
      -> true
      ; gr_mensaje(Visual,'Debes seleccionar una posicion adyacente.'),
	false
    ),
    ( posicion_libre(EstadoInicial,NewDir,NewDist)
      -> true
      ; gr_mensaje(Visual,'Debes seleccionar una posicion que este libre.'),
	false
    ),
    !,
    correr_ficha(EstadoInicial,Turno,Dir,Dist,NewDir,NewDist,EstadoIntermedio),
    fichas(EstadoIntermedio,Fichas),
    gr_dibujar_tablero(Visual,T,Fichas),
    chequear_molino(NewDir,NewDist,Visual,Turno,T,EstadoIntermedio,EstadoFinal).

% elegir_movimiento/4 <- +EstadoIncial, ?EstadoFinal, +Turno, +T
% se instancia en EstadoFinal el resultado de elegir una jugada aplicando minimax para todos los posibles movimientos de la maquina
elegir_movimiento(EstadoInicial, EstadoFinal,T,Turno) :-
    posibles_estados(EstadoInicial,T,Turno,PosiblesEstados),
    minimax_depth(Depth),
    elegir_movimiento_aux(PosiblesEstados,Turno,T,Depth,EstadoFinal,_).
elegir_movimiento_aux([C],Turno,T,Depth,C,Max) :- 
    contrincante(Turno,Contrincante),
    minimax(C,Depth,min,Max,Turno,Contrincante,T).
elegir_movimiento_aux([C|R],Turno,T,Depth,EstadoFinal,Max) :- 
    elegir_movimiento_aux(R,Turno,T,Depth,EstadoCandidato,Max1),
    contrincante(Turno,Contrincante),
    minimax(C,Depth,min,Max2,Turno,Contrincante,T),
    (
        Max1>Max2 -> 
            EstadoFinal = EstadoCandidato, Max = Max1;
            EstadoFinal = C, Max = Max2
    ).

% minimax/7 <- +Estado, +Depth, +Fase (min o max), +Jugador, +Turno, +T
minimax(Estado,0,_,Valor,Jugador,_,_) :- evaluar_tablero(Jugador,Estado,Valor).
minimax(Estado,_,_,Valor,Jugador,_,_) :-
    mover_fichas_restantes(Estado, blanco, CantidadFichas),
    CantidadFichas < 3,
    evaluar_tablero(Jugador,Estado,Valor).
minimax(Estado,_,_,Valor,Jugador,_,_) :-
    mover_fichas_restantes(Estado, negro, CantidadFichas),
    CantidadFichas < 3,
    evaluar_tablero(Jugador,Estado,Valor).
minimax(Estado,Depth,max,Max,Jugador,Turno,T) :- 
    Depth>0,
    posibles_estados(Estado,T,Turno,PosiblesEstados),
    Depth1 is Depth-1,
    max_posibles_Estados(PosiblesEstados,Depth1,Jugador,Turno,T,Max).
minimax(Estado,Depth,min,Min,Jugador,Turno,T) :- 
    Depth>0,
    posibles_estados(Estado,T,Turno,PosiblesEstados),
    Depth1 is Depth-1,
    min_posibles_Estados(PosiblesEstados,Depth1,Jugador,Turno,T,Min).

% max_posibles_Estados/6 <- +EstadosPosibles, +Depth, +Jugador, +Turno, +T, ?ValorMaximo
% Instancia en ValorMaximo el maximo valor de la evaluación de todos los posibles estados
max_posibles_Estados([C],Depth,Jugador,Turno,T,Max) :- 
    contrincante(Turno,Contrincante),
    minimax(C,Depth,min,Max,Jugador,Contrincante,T).
max_posibles_Estados([C|R],Depth,Jugador,Turno,T,Max) :-
    max_posibles_Estados(R,Depth,Jugador,Turno,T,Max1),
    contrincante(Turno,Contrincante),
    minimax(C,Depth,min,Max2,Jugador,Contrincante,T),
    (Max1>Max2 -> 
    	Max = Max1;
    	Max = Max2
    ).

% min_posibles_Estados/6 <- +EstadosPosibles, +Depth, +Jugador, +Turno, +T, ?ValorMinimo
% Instancia en ValorMinimo el minimo valor de la evaluación de todos los posibles estados
min_posibles_Estados([C],Depth,Jugador,Turno,T,Min) :- 
    contrincante(Turno,Contrincante),
    minimax(C,Depth,max,Min,Jugador,Contrincante,T).
min_posibles_Estados([C|R],Depth,Jugador,Turno,T,Min) :-
    min_posibles_Estados(R,Depth,Jugador,Turno,T,Min1),
    contrincante(Turno,Contrincante),
    minimax(C,Depth,max,Min2,Contrincante,Turno,T),
    (Min1<Min2 -> 
    	Min = Min1;
    	Min = Min2
    ).

% min_posibles_Estados/6 <- +Estado, +T, +Turno, ?PosiblesEstados
% Instancia en PosiblesEstados una lista de todos los estados posibles a partir de todos los movimientos posibles para el jugador Turno dado el Estado
% Se tiene en cuenta que si se forman molinos, la captura de una pieza del rival. Cada opción de captura agrega un estado más a PosiblesEstados
posibles_estados(Estado,T,Turno,PosiblesEstados) :-
    fase(Estado,colocar),
    posiciones_libres(T,Estado,Pos),
    random_permutation(Pos,PosRdm), % Para que en caso de haber 2 jugadas que evaluando el tablero den el mismo valor, no siempre se elija la misma.
    posibles_estados_colocar(Estado,PosRdm,T,Turno,PosiblesEstados).
posibles_estados(Estado,T,Turno,PosiblesEstados) :-
    fase(Estado,mover),
  	fichas_jugador_mover(T,Turno,Estado,FichasJugador),
    random_permutation(FichasJugador,FichasJugadorRdm), % Para que en caso de haber 2 jugadas que evaluando el tablero den el mismo valor, no siempre se elija la misma.
    posibles_estados_mover(Estado,FichasJugadorRdm,T,Turno,PosiblesEstados).

posibles_estados_colocar(_,[],_,_,[]).
posibles_estados_colocar(Estado,[C|R],T,Turno,[Ec|Er]) :-
    arg(1,C,Dir),
    arg(2,C,Dist),
    colocar_ficha(Estado,Turno,Dir,Dist,Ec),
    chequear_molino_maquina(Ec,Dir,Dist,Turno,T,N),
    N=0,
    posibles_estados_colocar(Estado,R,T,Turno,Er).
posibles_estados_colocar(Estado,[C|R],T,Turno,EstadosPosibles) :-
    arg(1,C,Dir),
    arg(2,C,Dist),
    colocar_ficha(Estado,Turno,Dir,Dist,EstadoResultante),
    chequear_molino_maquina(EstadoResultante,Dir,Dist,Turno,T,N),
    N>0,
    contrincante(Turno,Contrincante),
    fichas_jugador(Contrincante,EstadoResultante,FichasJugador),
    posibles_estados_molino(EstadoResultante,Contrincante,FichasJugador,Em),
    posibles_estados_colocar(Estado,R,T,Turno,Er),
    append(Em, Er, EstadosPosibles).

posibles_estados_molino(_,_,[],[]).
posibles_estados_molino(Estado,Contrincante,[C|R],[EstadoResultante|Er]) :- 
    arg(2,C,DirNew),
    arg(3,C,DistNew),
    quitar_ficha(Estado,Contrincante,DirNew,DistNew,EstadoResultante),
    posibles_estados_molino(Estado,Contrincante,R,Er).

posibles_estados_mover(_,[],_,_,[]).
posibles_estados_mover(Estado,[C|R],T,Turno,EstadosPosibles) :-
    arg(2,C,Dir),
    arg(3,C,Dist),
    posiciones_adyacentes_libres(T,Estado,Dir,Dist,Pos),
    random_permutation(Pos,PosRdm), % Para que en caso de haber 2 jugadas que evaluando el tablero den el mismo valor, no siempre se elija la misma.
    posibles_estados_mover_adyacentes(Estado,Dir,Dist,PosRdm,T,Turno,Ema),
    posibles_estados_mover(Estado,R,T,Turno,Er),
    append(Ema, Er, EstadosPosibles).

posibles_estados_mover_adyacentes(_,_,_,[],_,_,[]).
posibles_estados_mover_adyacentes(Estado,Dir,Dist,[C|R],T,Turno,[EstadoResultante|Er]) :-
    arg(1,C,NewDir),
    arg(2,C,NewDist),
    correr_ficha(Estado,Turno,Dir,Dist,NewDir,NewDist,EstadoResultante),
    chequear_molino_maquina(EstadoResultante,NewDir,NewDist,Turno,T,N),
    N=0,
    posibles_estados_mover_adyacentes(Estado,Dir,Dist,R,T,Turno,Er).

posibles_estados_mover_adyacentes(Estado,Dir,Dist,[C|R],T,Turno,EstadosPosibles) :-
    arg(1,C,NewDir),
    arg(2,C,NewDist),
    correr_ficha(Estado,Turno,Dir,Dist,NewDir,NewDist,EstadoResultante),
    chequear_molino_maquina(EstadoResultante,NewDir,NewDist,Turno,T,N),
    N>0,
    contrincante(Turno,Contrincante),
    fichas_jugador(Contrincante,EstadoResultante,FichasJugador),
    posibles_estados_molino(EstadoResultante,Contrincante,FichasJugador,Em),
    posibles_estados_mover_adyacentes(Estado,Dir,Dist,R,T,Turno,Er),
    append(Em, Er, EstadosPosibles).

% posiciones_libres/3 <- +T, +Fichas, ?Res
% Res es la lista con las posiciones libres en el tablero de tamaño T dada una lista de fichas colocadas F
posiciones_libres(T,Estado,R) :- findall((Dir,Dist),(posicion_valida(T,Dir,Dist), posicion_libre(Estado, Dir, Dist)),R).

% posicion_valida/3 <- +T, ?Dir, ?Dist
% La direccion Dir es valida y la distancia Dist son validos para un tablero de tamaño T
posicion_valida(T,Dir,Dist) :- T>0, dist_valida(T,Dist), dir_valida(Dir).

% dist_valida/2 <- +T, ?Dist
% La distancia Dist es posible para un tablero de tamaño T
dist_valida(T,Dist) :- H is T+1, dist_valida_aux(Dist, 1, H).
dist_valida_aux(D, D, _).
dist_valida_aux(Dist, D, H) :- D1 is D+1, D1 =< H, dist_valida_aux(Dist, D1, H).

% dist_valida/1 <- ?Dir
% La direccion Dir es valida
dir_valida(Dir) :- member(Dir,[nw,n,ne,w,e,sw,s,se]).

% La distancia Dist es posible para un tablero de tamaño T
chequear_molino_maquina(Estado,Dir,Dist,Turno,T,N) :-
    findall([ficha(Turno,Dir1,Dist1),ficha(Turno,Dir2,Dist2),ficha(Turno,Dir3,Dist3)],
            (member(ficha(Turno,Dir,Dist),[ficha(Turno,Dir1,Dist1),ficha(Turno,Dir2,Dist2),ficha(Turno,Dir3,Dist3)]),molino(Estado,ficha(Turno,Dir1,Dist1),ficha(Turno,Dir2,Dist2),ficha(Turno,Dir3,Dist3),T)),
            MolinosNuevos),
    length(MolinosNuevos,N).

% posiciones_adyacentes_libres/4 <- +T, +Estado, +Dir, +Dist, ?Res
% Instancia en Res todas las posiciones adyacentes a (Dir,Dist) que están libres en el tablero representado por Estado
posiciones_adyacentes_libres(T,Estado,Dir,Dist,Res) :- findall((DirR,DistR),(posicion_adyacente(Dir,Dist,DirR,DistR,T),posicion_libre(Estado, DirR, DistR)),Res).

% fichas_jugador_mover/4 <- +T, +Turno, +Estado, ?FichasJugadorMover
% Instancia en FichasJugadorMover todas las fichas que el jugaodr Turno puede mover
% No puede mover sus fichas que no tengan posiciones adyacentes libres
fichas_jugador_mover(T,Turno,Estado,FichasJugadorMover) :- 
    fichas_jugador(Turno,Estado,FichasJugador),
    fichas_jugador_mover_aux(T,Estado,FichasJugador,FichasJugadorMover).
   
fichas_jugador_mover_aux(_,_,[],[]).
fichas_jugador_mover_aux(T,Estado,[C|R],[C|FR]) :-
    arg(2,C,Dir),
    arg(3,C,Dist),
    posiciones_adyacentes_libres(T,Estado,Dir,Dist,Pos),
    length(Pos,N),
    N>0,
    fichas_jugador_mover_aux(T,Estado,R,FR).
fichas_jugador_mover_aux(T,Estado,[C|R],FichasJugador) :-
    arg(2,C,Dir),
    arg(3,C,Dist),
    posiciones_adyacentes_libres(T,Estado,Dir,Dist,Pos),
    length(Pos,N),
    N=0,
    fichas_jugador_mover_aux(T,Estado,R,FichasJugador).
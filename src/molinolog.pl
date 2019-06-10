:- use_module(graficos).

% Este archivo se provee como una guía para facilitar la implementación y 
% entender el uso de graficos.pl
% El contenido de este archivo se puede modificar.

% El predicado minimax_depth/1 define la recursión máxima a utilizar en el algoritmo minimax
minimax_depth(3).

% Variables globales:
% fase/1, +Fase.
% Fase solo puede ser colocar o mover.
% fichas/3, +Turno,+Dir,+Dist.
% Indica que hay una ficha del jugador Turno en la posición (Dir,Dist).
% colocar_fichas_restantes/2, +Turno,+Fichas.
% Indica que al jugador Turno le resta Fichas fichas por colocar.
% mover_fichas_restantes/2, +Turno,+Fichas.
% Indica que al jugador Turno le quedan Fichas fichas para jugar.

% molinolog(+JugadorNegro,+JugadorBlanco,+T)
% JugadorNegtro y JugadorBlanco pueden ser los átomos humano o maquina.
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

borrar_variables_globales() :-
    retractall(fase(_)), % <- Variable global con la fase actual del juego
    retractall(ficha(_,_,_)),
    retractall(colocar_fichas_restantes(_,_)),
    retractall(mover_fichas_restantes(_,_)).
% borrar_variables_globales().

inicializar_variables_globales(T) :-
    assert(fase(colocar)),
    Fichas is 3*(T+1),
    % Variables globales con las fichas que cada jugador les resta colocar
    assert(colocar_fichas_restantes(negro,Fichas)),
    assert(colocar_fichas_restantes(blanco,Fichas)),
    assert(mover_fichas_restantes(negro,Fichas)),
    assert(mover_fichas_restantes(blanco,Fichas)).

iniciar_juego(Visual,JugadorNegro,JugadorBlanco,T):-
    borrar_variables_globales(),
    inicializar_variables_globales(T),
    gr_dibujar_tablero(Visual,T,[]),
    loop(Visual,negro,JugadorNegro,JugadorBlanco,T).

contrincante(negro,blanco).
contrincante(blanco,negro).

% --------------------------
% Loop principal
% --------------------------

loop(Visual,Turno,JugadorNegro,JugadorBlanco,T) :-
    mensaje(Visual,Turno),
    jugar_turno(Visual,Turno,JugadorNegro,JugadorBlanco,T),
    contrincante(Turno,SiguienteTurno),
    loop(Visual,SiguienteTurno,JugadorNegro,JugadorBlanco,T).

jugar_turno(Visual,negro,humano,JugadorBlanco,T) :-
    gr_evento(Visual,E),
    evento(E,Visual,negro,humano,JugadorBlanco,T).
jugar_turno(Visual,blanco,JugadorNegro,humano,T) :-
    gr_evento(Visual,E),
    evento(E,Visual,blanco,JugadorNegro,humano,T).

evento(click(Dir,Dist),Visual,Turno,JugadorNegro,JugadorBlanco,T) :-
    fase(colocar),
    posicion_libre(Dir,Dist),
    !,
    colocar_ficha(Dir,Dist,Visual,Turno,JugadorNegro,JugadorBlanco,T).
evento(click(Dir,Dist),Visual,Turno,JugadorNegro,JugadorBlanco,T) :-
    fase(colocar),
    \+ posicion_libre(Dir,Dist),
    !,
    gr_mensaje(Visual,'Posicion ocupada, seleccione otra.'),
    loop(Visual,Turno,JugadorNegro,JugadorBlanco,T).
evento(click(Dir,Dist),Visual,Turno,JugadorNegro,JugadorBlanco,T) :-
    fase(mover),
    posicion_ocupada(Turno,Dir,Dist),
    !,
    mover_ficha(Dir,Dist,Visual,Turno,JugadorNegro,JugadorBlanco,T).
evento(click(Dir,Dist),Visual,Turno,JugadorNegro,JugadorBlanco,T) :-
    fase(mover),
    \+ posicion_ocupada(Turno,Dir,Dist),
    !,
    sformat(Msg, 'Debes elegir una ficha color ~w.', [Turno]),
    gr_mensaje(Visual,Msg),
    loop(Visual,Turno,JugadorNegro,JugadorBlanco,T).
evento(salir,Visual,Turno,JugadorNegro,JugadorBlanco,T) :-
    (   gr_opciones(Visual, '¿Seguro?', ['Sí', 'No'], 'Sí')
	->  halt
	;   loop(Visual,Turno,JugadorNegro,JugadorBlanco,T)
	).
evento(reiniciar,Visual,Turno,JugadorNegro,JugadorBlanco,T) :-
    (   gr_opciones(Visual, '¿Seguro?', ['Sí', 'No'], 'Sí')
	->  % reiniciar el juego
        iniciar_juego(Visual,JugadorNegro,JugadorBlanco,T)
	;   loop(Visual,Turno,JugadorNegro,JugadorBlanco,T)
	).

mensaje(Visual,Turno) :-
    fase(colocar),
    colocar_fichas_restantes(Turno,Fichas),
    sformat(Msg, 'Jugador ~w, colocar ~D fichas', [Turno,Fichas]),
    gr_estado(Visual, Msg).
mensaje(Visual,Turno) :-
    fase(mover),
    sformat(Msg, 'Jugador ~w, mover', [Turno]),
    gr_estado(Visual, Msg).

colocar_ficha(Dir,Dist,Visual,Turno,JugadorNegro,JugadorBlanco,T) :-
    assert(ficha(Turno,Dir,Dist)),
    retract(colocar_fichas_restantes(Turno,Fichas)),
    Fichas1 is Fichas-1,
    assert(colocar_fichas_restantes(Turno,Fichas1)),
    contrincante(Turno,Contrincante),
    clause(colocar_fichas_restantes(Contrincante,FichasContrincante),true),
    (   (Fichas1 == 0,FichasContrincante == 0)
    -> (retract(fase(colocar)),assert(fase(mover)),retractall(colocar_fichas_restantes(_,_)))
    ;   true
    ),
    gr_ficha(Visual,T,Dir,Dist,Turno),
    chequear_molino(Dir,Dist,Visual,Turno,JugadorNegro,JugadorBlanco,T).

chequear_molino(Dir,Dist,Visual,Turno,JugadorNegro,JugadorBlanco,T) :-
    findall([ficha(Turno,Dir1,Dist1),ficha(Turno,Dir2,Dist2),ficha(Turno,Dir3,Dist3)],
            (member(ficha(Turno,Dir,Dist),[ficha(Turno,Dir1,Dist1),ficha(Turno,Dir2,Dist2),ficha(Turno,Dir3,Dist3)]),molino(ficha(Turno,Dir1,Dist1),ficha(Turno,Dir2,Dist2),ficha(Turno,Dir3,Dist3),T)),
            MolinosNuevos),
    length(MolinosNuevos,N),
    (   N > 0
    ->  [[ficha(_,Dir11,Dist11),ficha(_,Dir12,Dist12),ficha(_,Dir13,Dist13)]|_] = MolinosNuevos,
        gr_ficha(Visual,T,Dir11,Dist11,'seleccion'),
        gr_ficha(Visual,T,Dir12,Dist12,'seleccion'),
        gr_ficha(Visual,T,Dir13,Dist13,'seleccion'),
        capturar_ficha(Visual,Turno,JugadorNegro,JugadorBlanco,T)
    ;   true
    ).

% Molinos horizontales
molino(ficha(Turno,nw,N),ficha(Turno,n,N),ficha(Turno,ne,N),_) :-
    clause(ficha(Turno,nw,N),true),
    clause(ficha(Turno,n,N),true),
    clause(ficha(Turno,ne,N),true).
molino(ficha(Turno,w,Dist1),ficha(Turno,w,Dist2),ficha(Turno,w,Dist3),T) :-
    T1 is T - 1,
    between(1,T1,Dist3),
    Dist2 is Dist3 + 1, Dist1 is Dist2 + 1,
    clause(ficha(Turno,w,Dist1),true),
    clause(ficha(Turno,w,Dist2),true),
    clause(ficha(Turno,w,Dist3),true).
molino(ficha(Turno,e,Dist1),ficha(Turno,e,Dist2),ficha(Turno,e,Dist3),T) :-
    T1 is T - 1,
    between(1,T1,Dist1),
    Dist2 is Dist1 + 1, Dist3 is Dist2 + 1,
    clause(ficha(Turno,e,Dist1),true),
    clause(ficha(Turno,e,Dist2),true),
    clause(ficha(Turno,e,Dist3),true).
molino(ficha(Turno,sw,N),ficha(Turno,s,N),ficha(Turno,se,N),_) :-
    clause(ficha(Turno,sw,N),true),
    clause(ficha(Turno,s,N),true),
    clause(ficha(Turno,se,N),true).
% Molinos verticales
molino(ficha(Turno,nw,N),ficha(Turno,w,N),ficha(Turno,sw,N),_) :-
    clause(ficha(Turno,nw,N),true),
    clause(ficha(Turno,w,N),true),
    clause(ficha(Turno,sw,N),true).
molino(ficha(Turno,n,Dist1),ficha(Turno,n,Dist2),ficha(Turno,n,Dist3),T) :-
    T1 is T - 1,
    between(1,T1,Dist3),
    Dist2 is Dist3 + 1, Dist1 is Dist2 + 1,
    clause(ficha(Turno,n,Dist1),true),
    clause(ficha(Turno,n,Dist2),true),
    clause(ficha(Turno,n,Dist3),true).
molino(ficha(Turno,s,Dist1),ficha(Turno,s,Dist2),ficha(Turno,s,Dist3),T) :-
    T1 is T - 1,
    between(1,T1,Dist1),
    Dist2 is Dist1 + 1, Dist3 is Dist2 + 1,
    clause(ficha(Turno,s,Dist1),true),
    clause(ficha(Turno,s,Dist2),true),
    clause(ficha(Turno,s,Dist3),true).
molino(ficha(Turno,ne,N),ficha(Turno,e,N),ficha(Turno,se,N),_) :-
    clause(ficha(Turno,ne,N),true),
    clause(ficha(Turno,e,N),true),
    clause(ficha(Turno,se,N),true).

verificar_fin(2,Turno,Visual,JugadorNegro,JugadorBlanco,T):-
    sformat(Msg, 'Ha ganado el jugador ~w, jugar nuevamente?', [Turno]),
    (   gr_opciones(Visual, Msg, ['Sí', 'No'], 'Sí')
	->  % reiniciar el juego
        iniciar_juego(Visual,JugadorNegro,JugadorBlanco,T)
	; halt
    ).
verificar_fin(_,_,_,_,_,_).

capturar_ficha(Visual,Turno,JugadorNegro,JugadorBlanco,T) :-
    sformat(Msg,'Jugador ~w, capturar',[Turno]),
    gr_estado(Visual,Msg),
    gr_evento(Visual,click(Dir,Dist)),
    contrincante(Turno,Contrincante),
    ( posicion_ocupada(Contrincante,Dir,Dist)
      -> true
      ; sformat(Msg2,'Debes seleccionar una ficha color ~w.',[Contrincante]),
	gr_mensaje(Visual,Msg2),
	false
    ),
    !,
    retract(mover_fichas_restantes(Contrincante,Fichas)),
    Fichas1 is Fichas-1,
    assert(mover_fichas_restantes(Contrincante,Fichas1)),
    retract(ficha(Contrincante,Dir,Dist)),
    findall(ficha(Color1,Dir1,Dist1),clause(ficha(Color1,Dir1,Dist1),true),ListaFichas),
    gr_dibujar_tablero(Visual,T,ListaFichas),
    verificar_fin(Fichas1,Turno,Visual,JugadorNegro,JugadorBlanco,T).
capturar_ficha(Visual,Turno,JugadorNegro,JugadorBlanco,T) :-
    capturar_ficha(Visual,Turno,JugadorNegro,JugadorBlanco,T).

mover_ficha(Dir,Dist,Visual,Turno,JugadorNegro,JugadorBlanco,T) :-
    gr_ficha(Visual,T,Dir,Dist,'seleccion'),
    gr_evento(Visual,click(NewDir,NewDist)),
    ( posicion_adyacente(Dir,Dist,NewDir,NewDist,T)
      -> true
      ; gr_mensaje(Visual,'Debes seleccionar una posicion adyacente.'),
	false
    ),
    ( posicion_libre(NewDir,NewDist)
      -> true
      ; gr_mensaje(Visual,'Debes seleccionar una posicion que este libre.'),
	false
    ),
    !,
    retract(ficha(Turno,Dir,Dist)),
    assert(ficha(Turno,NewDir,NewDist)),
    findall(ficha(Color1,Dir1,Dist1),clause(ficha(Color1,Dir1,Dist1),true),ListaFichas),
    gr_dibujar_tablero(Visual,T,ListaFichas),
    chequear_molino(NewDir,NewDist,Visual,Turno,JugadorNegro,JugadorBlanco,T).
mover_ficha(Dir,Dist,Visual,Turno,JugadorNegro,JugadorBlanco,T) :-
    mover_ficha(Dir,Dist,Visual,Turno,JugadorNegro,JugadorBlanco,T).

posicion_libre(Dir,Dist) :-
    \+ clause(ficha(_,Dir,Dist),true).

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

posicion_ocupada(Jugador,Dir,Dist) :-
    clause(ficha(Jugador,Dir,Dist),true).

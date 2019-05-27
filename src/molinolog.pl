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
% colocar_fichas/2, +Turno,+Fichas.
% Indica que al jugador Turno le resta Fichas fichas por colocar.

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
    retractall(colocar_fichas(_,_)).
borrar_variables_globales().

inicializar_variables_globales(T) :-
    assert(fase(colocar)),
    Fichas is 3*(T+1),
    % Variables globales con las fichas que cada jugador les resta colocar
    assert(colocar_fichas(negro,Fichas)),
    assert(colocar_fichas(blanco,Fichas)).

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
    gr_evento(Visual,E),
    evento(E,Visual,Turno,JugadorNegro,JugadorBlanco,T).

evento(click(Dir,Dist),Visual,Turno,JugadorNegro,JugadorBlanco,T) :-
    handle_click(Dir,Dist,Visual,Turno,JugadorNegro,JugadorBlanco,T),
    contrincante(Turno,SiguienteTurno),
    loop(Visual,SiguienteTurno,JugadorNegro,JugadorBlanco,T).
evento(salir,Visual,Turno,JugadorNegro,JugadorBlanco,T) :-
    (   gr_opciones(Visual, '¿Seguro?', ['Sí', 'No'], 'Sí')
	->  true
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
    colocar_fichas(Turno,Fichas),
    sformat(Msg, 'Jugador ~w, colocar ~D fichas', [Turno,Fichas]),
    gr_estado(Visual, Msg).

handle_click(Dir,Dist,Visual,Turno,_,_,T) :-
    fase(colocar),
    posicion_libre(Dir,Dist),
    assert(ficha(Turno,Dir,Dist)),
    retract(colocar_fichas(Turno,Fichas)),
    Fichas1 is Fichas-1,
    assert(colocar_fichas(Turno,Fichas1)),
    gr_ficha(Visual,T,Dir,Dist,Turno),
    chequear_molino(Dir,Dist,Visual,Turno,JugadorNegro,JugadorBlanco,T).
handle_click(_,_,Visual,Turno,JugadorNegro,JugadorBlanco,T) :-
    loop(Visual,Turno,JugadorNegro,JugadorBlanco,T).

chequear_molino(Dir,Dist,Visual,Turno,JugadorNegro,JugadorBlanco,T) :-
    findall([ficha(Turno,Dir1,Dist1),ficha(Turno,Dir2,Dist2),ficha(Turno,Dir3,Dist3)],
            (member(ficha(Turno,Dir,Dist),[ficha(Turno,Dir1,Dist1),ficha(Turno,Dir2,Dist2),ficha(Turno,Dir3,Dist3)]),molino(ficha(Turno,Dir1,Dist1),ficha(Turno,Dir2,Dist2),ficha(Turno,Dir3,Dist3))),
            MolinosNuevos),
    length(MolinosNuevos,N),
    (   N > 0
    ->  capturar_ficha(Visual,Turno,JugadorNegro,JugadorBlanco,T)
    ;   true
    ).

% Molinos horizontales
molino(ficha(Turno,nw,N),ficha(Turno,n,N),ficha(Turno,ne,N)) :-
    clause(ficha(Turno,nw,N),true),
    clause(ficha(Turno,n,N),true),
    clause(ficha(Turno,ne,N),true),
    !.
molino(ficha(Turno,w,3),ficha(Turno,w,2),ficha(Turno,w,1)) :-
    clause(ficha(Turno,w,3),true),
    clause(ficha(Turno,w,2),true),
    clause(ficha(Turno,w,1),true),
    !.
molino(ficha(Turno,e,1),ficha(Turno,e,2),ficha(Turno,e,3)) :-
    clause(ficha(Turno,e,1),true),
    clause(ficha(Turno,e,2),true),
    clause(ficha(Turno,e,3),true),
    !.
molino(ficha(Turno,sw,N),ficha(Turno,s,N),ficha(Turno,se,N)) :-
    clause(ficha(Turno,sw,N),true),
    clause(ficha(Turno,s,N),true),
    clause(ficha(Turno,se,N),true),
    !.
% Molinos verticales
molino(ficha(Turno,nw,N),ficha(Turno,w,N),ficha(Turno,sw,N)) :-
    clause(ficha(Turno,nw,N),true),
    clause(ficha(Turno,w,N),true),
    clause(ficha(Turno,sw,N),true),
    !.
molino(ficha(Turno,n,3),ficha(Turno,n,2),ficha(Turno,n,1)) :-
    clause(ficha(Turno,n,3),true),
    clause(ficha(Turno,n,2),true),
    clause(ficha(Turno,n,1),true),
    !.
molino(ficha(Turno,s,1),ficha(Turno,s,2),ficha(Turno,s,3)) :-
    clause(ficha(Turno,s,1),true),
    clause(ficha(Turno,s,2),true),
    clause(ficha(Turno,s,3),true),
    !.
molino(ficha(Turno,ne,N),ficha(Turno,e,N),ficha(Turno,se,N)) :-
    clause(ficha(Turno,ne,N),true),
    clause(ficha(Turno,e,N),true),
    clause(ficha(Turno,se,N),true),
    !.

% capturar_ficha(Visual,Turno,JugadorNegro,JugadorBlanco,T) :-
%     sformat(Msg, 'Jugador ~w, capturar', [Turno]),
%     gr_estado(Visual, Msg),


posicion_libre(Dir,Dist) :-
    \+ clause(ficha(_,Dir,Dist),true).

posicion_ocupada(Jugador,Dir,Dist) :-
    clause(ficha(Jugador,Dir,Dist),true).

:- use_module(graficos).

% Este archivo se provee como una guía para facilitar la implementación y 
% entender el uso de graficos.pl
% El contenido de este archivo se puede modificar.

% El predicado minimax_depth/1 define la recursión máxima a utilizar en el algoritmo minimax
minimax_depth(3).

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

iniciar_juego(Visual,JugadorNegro,JugadorBlanco,T):-
    iniciar_estado(T,Estado),
    fichas(Estado,Fichas),
    gr_dibujar_tablero(Visual,T,Fichas),
    loop(Visual,negro,JugadorNegro,JugadorBlanco,T, Estado).

contrincante(negro,blanco).
contrincante(blanco,negro).

% inicializar_variables(+T,?E)/2
% T es el tamaño del tablero.
% E es el functor que representa un estado del juego.
% Tiene aridad 6 con la forma estado(Fase,Fichas,FichasColocarNegro,FichasColocarBlanco,FichasMoverNegro,FichasMoverBlanco).
% Fase en la que se esta (colocar o mover).
% Fichas en el tablero. Lista con elementos de la forma (Turno, Dir, Dist).
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

fase(Estado,Fase) :- arg(1,Estado,Fase).
fichas(Estado,Fichas) :- arg(2,Estado,Fichas).
colocar_fichas_restantes(Estado, negro, CantidadFichas) :- arg(3,Estado,CantidadFichas).
colocar_fichas_restantes(Estado, blanco, CantidadFichas) :- arg(4,Estado,CantidadFichas).
mover_fichas_restantes(Estado, negro, CantidadFichas) :- arg(5,Estado,CantidadFichas).
mover_fichas_restantes(Estado, blanco, CantidadFichas) :- arg(6,Estado,CantidadFichas).

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

siguiente_fase(0,0,mover):-!.
siguiente_fase(_,_,colocar).

existe_ficha(Estado,ficha(Turno,Dir,Dist)) :- fichas(Estado,Fichas), member(ficha(Turno,Dir,Dist),Fichas). 

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

% --------------------------
% Loop principal
% --------------------------

loop(Visual,Turno,JugadorNegro,JugadorBlanco,T, Estado) :-
    mensaje(Visual,Turno,Estado),
    gr_evento(Visual,Evento),
    evento(Evento,Visual,Turno,JugadorNegro,JugadorBlanco,T,Estado).

mensaje(Visual,Turno,Estado) :-
    fase(Estado,colocar),
    colocar_fichas_restantes(Estado,Turno,Cantidad),
    sformat(Msg, 'Jugador ~w, colocar ~D fichas', [Turno,Cantidad]),
    gr_estado(Visual, Msg).
mensaje(Visual,Turno,Estado) :-
    fase(Estado,mover),
    sformat(Msg, 'Jugador ~w, mover', [Turno]),
    gr_estado(Visual, Msg).

evento(click(Dir,Dist),Visual,Turno,JugadorNegro,JugadorBlanco,T,Estado) :-
    fase(Estado,colocar),
    posicion_libre(Estado,Dir,Dist),
    !,
    gr_ficha(Visual,T,Dir,Dist,Turno),
    colocar_ficha(Estado,Turno,Dir,Dist,EstadoIntermedio),
    chequear_molino(Dir,Dist,Visual,Turno,JugadorNegro,JugadorBlanco,T,EstadoIntermedio,EstadoFinal),
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
        mover_ficha(Visual,T,JugadorNegro,JugadorBlanco,Estado,Turno,Dir,Dist,EstadoFinal) -> true ; 
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
    gr_mensaje(Visual,'Evento salir'),
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

posicion_libre(Estado, Dir, Dist) :- fichas(Estado,Fichas), posicion_libre_aux(Fichas,Dir,Dist).
posicion_libre_aux([],_,_).
posicion_libre_aux([C|R],Dir,Dist) :- 
    arg(2,C,Dir1),
    arg(3,C,Dist1),
    posiciones_distintas((Dir1,Dist1),(Dir,Dist)),
    posicion_libre_aux(R,Dir,Dist).

posiciones_distintas((Dir1,Dist1),(Dir2,Dist2)) :- Dir1 \= Dir2, Dist1 \= Dist2.
posiciones_distintas((Dir1,Dist),(Dir2,Dist)) :- Dir1 \= Dir2.
posiciones_distintas((Dir,Dist1),(Dir,Dist2)) :- Dist1 \= Dist2.

chequear_molino(Dir,Dist,Visual,Turno,JugadorNegro,JugadorBlanco,T,EstadoInicial,EstadoFinal) :-
    findall([ficha(Turno,Dir1,Dist1),ficha(Turno,Dir2,Dist2),ficha(Turno,Dir3,Dist3)],
            (member(ficha(Turno,Dir,Dist),[ficha(Turno,Dir1,Dist1),ficha(Turno,Dir2,Dist2),ficha(Turno,Dir3,Dist3)]),molino(EstadoInicial,ficha(Turno,Dir1,Dist1),ficha(Turno,Dir2,Dist2),ficha(Turno,Dir3,Dist3),T)),
            MolinosNuevos),
    length(MolinosNuevos,N),
    N>0,
    [[ficha(_,Dir11,Dist11),ficha(_,Dir12,Dist12),ficha(_,Dir13,Dist13)]|_] = MolinosNuevos,
    gr_ficha(Visual,T,Dir11,Dist11,'seleccion'),
    gr_ficha(Visual,T,Dir12,Dist12,'seleccion'),
    gr_ficha(Visual,T,Dir13,Dist13,'seleccion'),
    capturar_ficha(Visual,Turno,JugadorNegro,JugadorBlanco,T,EstadoInicial,EstadoFinal).

chequear_molino(Dir,Dist,_,Turno,_,_,T,Estado,Estado) :-
    findall([ficha(Turno,Dir1,Dist1),ficha(Turno,Dir2,Dist2),ficha(Turno,Dir3,Dist3)],
            (member(ficha(Turno,Dir,Dist),[ficha(Turno,Dir1,Dist1),ficha(Turno,Dir2,Dist2),ficha(Turno,Dir3,Dist3)]),molino(Estado,ficha(Turno,Dir1,Dist1),ficha(Turno,Dir2,Dist2),ficha(Turno,Dir3,Dist3),T)),
            MolinosNuevos),
    length(MolinosNuevos,N),
    N=0.

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

capturar_ficha(Visual,Turno,JugadorNegro,JugadorBlanco,T,EstadoInicial,EstadoFinal) :-
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
    gr_dibujar_tablero(Visual,T,Fichas),
    mover_fichas_restantes(EstadoFinal, Contrincante, CantidadFichas),
    verificar_fin(CantidadFichas,Turno,Visual,JugadorNegro,JugadorBlanco,T).

capturar_ficha(Visual,Turno,JugadorNegro,JugadorBlanco,T,EstadoInicial,EstadoFinal) :-
    capturar_ficha(Visual,Turno,JugadorNegro,JugadorBlanco,T,EstadoInicial,EstadoFinal).

verificar_fin(2,Turno,Visual,JugadorNegro,JugadorBlanco,T):-
    sformat(Msg, 'Ha ganado el jugador ~w, jugar nuevamente?', [Turno]),
    (   gr_opciones(Visual, Msg, ['Sí', 'No'], 'Sí')
	->  % reiniciar el juego
        iniciar_juego(Visual,JugadorNegro,JugadorBlanco,T)
	; halt
    ).
verificar_fin(_,_,_,_,_,_).

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

mover_ficha(Visual,T,JugadorNegro,JugadorBlanco,EstadoInicial,Turno,Dir,Dist,EstadoFinal) :-
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
    chequear_molino(NewDir,NewDist,Visual,Turno,JugadorNegro,JugadorBlanco,T,EstadoIntermedio,EstadoFinal).
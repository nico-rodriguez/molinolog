% Predicados gráficos para la segunda tarea de 
% Programación Lógica 2019 - Molinolog
% 
% Este archivo no es entregable, así que no debe ser modificado, salvo
% para ayudar a depurar. A medida que se encuentren errores y se
% sugieran mejoras van a haber nuevas versiones.
%
%


:- module(graficos,
[
 gr_crear/3, % -Ventana +T +Botones
 % Devuelve un handle a la Ventana creada.
 % T es el tamaño del tablero.
 % En botones viene una lista de boton(Nombre, Evento)
 % indicando los botones a crear.
 % Las dimensiones iniciales son Filas*Columnas.
 
 gr_destruir/1, % +Ventana
 % Cierra la ventana e invalida el handle.
 
 gr_dibujar_tablero/3, % +Ventana +T +Fichas
 % Redibuja el tablero con una nueva lista de fichas.
 % T es el tamaño del tablero.
 % Fichas es una lista de elementos ficha(Tipo,Dir,Dist)
 
 gr_dimensiones/2, % +Ventana +T
 % Cambia el tamaño del tablero de la ventana Ventana,
 % poniendo todas las casillas vacías.
 % T es el tamaño del tablero.

 gr_ficha/5, % +Ventana +T +Dir +Dist +Ficha
 % Pinta la ficha Ficha en la casilla (Dir, Dist) de la ventana Ventana.
 % Ficha puede ser 'blanco' o 'negro' para colocar una ficha de ese color
 % o 'seleccion' para dibujar el marcador de selección.
 % T es el tamaño del tablero.
 
 gr_evento/2, % +Ventana ?Evento
 % Devuelve en Evento la acción del usuario, que puede ser 
 % click(Dir, Dist), salir o
 % el del boton accionado.

 gr_mensaje/2, % +Ventana +String
 % Muestra String en una ventanita auxiliar,
 % quedando la aplicación a la espera de que ésta se cierre.

 gr_pregunta/3, % +Ventana +Pregunta ?Respuesta
 % Muestra una ventanita conteniendo Pregunta
 % y un espacio para que el usuario ingrese Respuesta.
 % Se regresa del predicado cuando el usuario selecciona el botón.
 % El predicado falla si se cierra el dialogo.
 
 gr_opciones/4, % +Ventana +Pregunta +Opciones ?Respuesta
 % Muestra una ventanita conteniendo Pregunta
 % y un botón por cada elemento de Opciones,
 % para que elija el usuario.
 % Se regresa del predicado cuando el usuario selecciona un botón,
 % devolviendo el elegido en Respuesta
 % El predicado falla si se cierra el dialogo.

 gr_estado/2 , % +Ventana +NuevoEstado
 % Muestra el NuevoEstado de la partida en la parte inferior 
 % de la pantalla.

 gr_purgar/0
 % Cierra todas las ventanas que pueden haber quedado abiertas
 % por fallos del programa. Hack, usar solo en desarrollo.
]).


:- use_module(library(pce)).
:- use_module(library(tabular)).
:- use_module(library(autowin)).
:- use_module(library(dragdrop)).

%La clase root.
:- pce_begin_class(my_frame, frame).

variable(queue, any, both).
variable(image, image, both).
variable(dialog, dialog, both).

%Desactivamos que el usuario la cierre.
%En su lugar, mandamos un mensaje salir.
wm_delete(Self) :->
	get(Self, queue, Queue),
	thread_send_message(Queue, salir).

:- pce_end_class.

click_tablero(Q, Punto, T) :-
	get(Punto, y, Y),
	get(Punto, x, X),
    C is 1 + (X + 2) // 60,
    F is 1 + (Y + 2) // 60,
    coords(F,C,T,Dir,Dist),
    valid_coords(F,C,T),
	thread_send_message(Q, click(Dir, Dist)).

:- send(class(my_frame), record_instances).
gr_purgar :-
	get(class(my_frame), instances, I),
	send(I, for_all, message(@arg1, destroy)).

gr_crear(Frame, T, Botones) :-
	message_queue_create(Q),
	new(Frame, my_frame('Molinolog')),
	new(W, auto_sized_dialog),
	send(Frame, can_resize,	@off),
	forall(member(boton(Txt, Val), Botones),
	       send(W, append, 
		    button(Txt, 
			   message(@prolog,
				   thread_send_message,
				   prolog(Q),
				   prolog(Val))))),
	send(W, max_size, size(1000, 1200)),
	new(I, image(kind := pixmap)),
	new(B, bitmap(I)),
	send(B, recogniser, 
	     click_gesture(left, '', single, 
			      message(@prolog,
				      click_tablero,
				      prolog(Q),
                      @event?position,
                      T))),
	send(W, append, B),
	send(W, append, label(reporter)),
	send(Frame, append, W),
	send(Frame, queue, prolog(Q)),
	send(Frame, image, I),
	send(Frame, dialog, W),
	gr_dimensiones(Frame,T),
	send(Frame, open).

gr_destruir(Ventana) :-
	get(Ventana, queue, Q),
	message_queue_destroy(Q),
	send(Ventana, destroy).

gr_dimensiones(Ventana,T) :-
	Tam is 120*(T+1)+60,
	get(Ventana, image, I),
	send(I, resize, Tam, Tam),
	get(Ventana, dialog, W),
	send(W, redraw),
	!.

gr_dibujar(Ventana, X, Y, Imagen):-
    atom_concat(Imagen, '.gif', Arch),
	new(ImgFicha,image(Arch)),
	get(Ventana, image, I),
	send(I, draw_in, bitmap(ImgFicha), point(X, Y)),
	send(Ventana, flush),
	!.

gr_dibujar_tablero(Ventana, T, Fichas):-
    gr_dibujar_base(Ventana,T),
    gr_dibujar_fichas(Ventana,T,Fichas),
	!.

gr_dibujar_base(Ventana,T):-
    Max is 2*(T+1) + 1,
    Mid is T + 2,
    between(1,Max,F),
    between(1,Max,C),
    tipo_casillero(F,C,Mid,Max,Tipo),
    dibujar_casillero(Ventana,F,C,Tipo),
    fail.
gr_dibujar_base(_,_).

% filas superiores
tipo_casillero(F,C,Mid,_,i_v):-
    F < Mid, C < F,!.
tipo_casillero(F,F,Mid,_,l_s_e):-
    F < Mid,!.
tipo_casillero(F,C,Mid,_,i_h):-
    F < Mid, F < C, C < Mid,!.
tipo_casillero(1,Mid,Mid,_,t_s):-!.
tipo_casillero(F,Mid,Mid,_,cruz):-
    F < Mid-1,!.
tipo_casillero(F,Mid,Mid,_,t_n):-
    F is Mid-1,!.
tipo_casillero(F,C,Mid,Max,i_h):-
    F < Mid, Mid < C, C < Max - F + 1,!.
tipo_casillero(F,C,Mid,Max,l_s_w):-
    F < Mid, C is Max - F + 1,!.
tipo_casillero(F,C,Mid,Max,i_v):-
    F < Mid, C > Max - F + 1,!.

% fila del centro
tipo_casillero(Mid,1,Mid,_,t_e):-!.
tipo_casillero(Mid,C,Mid,_,cruz):-
    1 < C, C < Mid-1, !.
tipo_casillero(Mid,C,Mid,_,t_w):-
    C is Mid-1, !.
tipo_casillero(Mid,C,Mid,_,t_e):-
    C is Mid+1, !.
tipo_casillero(Mid,C,Mid,Max,cruz):-
    C > Mid+1, C < Max,!.
tipo_casillero(Mid,Max,Mid,Max,t_w):-!.

% filas inferiores
tipo_casillero(F,C,Mid,Max,i_v):-
    F > Mid, C < Max - F + 1,!.
tipo_casillero(F,C,Mid,Max,l_n_e):-
    F > Mid, C is Max - F + 1,!.
tipo_casillero(F,C,Mid,_,i_h):-
    F > Mid, C < Mid,!.
tipo_casillero(F,Mid,Mid,_,t_s):-
    F is Mid+1,!.
tipo_casillero(F,Mid,Mid,Max,cruz):-
    F > Mid+1, F < Max, !.
tipo_casillero(Max,Mid,Mid,Max,t_n):-!.
tipo_casillero(F,C,Mid,_,i_h):-
    F > Mid, C > Mid, C < F, !.
tipo_casillero(F,F,Mid,_,l_n_w):-
    F > Mid,!.
tipo_casillero(F,C,Mid,_,i_v):-
    F > Mid, C > F,!.

tipo_casillero(_,_,_,_,nada).

dibujar_casillero(Ventana,F,C,Tipo):-
    atom_concat('tablero_',Tipo,NomArch),
    X is (C-1) * 60,
    Y is (F-1) * 60,
    gr_dibujar(Ventana, X, Y, NomArch).

gr_dibujar_fichas(_,_,[]).
gr_dibujar_fichas(Ventana,T,[ficha(Tipo,Dir,Dist)|Resto]):-
    gr_ficha(Ventana, T, Dir, Dist, Tipo),
    gr_dibujar_fichas(Ventana,T,Resto).

gr_ficha(Ventana, T, Dir, Dist, Ficha) :-
    coords(F,C,T,Dir,Dist),
    X is (C-1) * 60,
    Y is (F-1) * 60,
    gr_dibujar(Ventana, X, Y, Ficha),
	!.

gr_evento(Ventana, Input) :-
	get(Ventana, queue, Q),
	thread_get_message(Q, Aux),
	!,
	Input = Aux.

gr_mensaje(V, Texto) :-
	new(D, dialog('Mensaje')),
	send(D, transient_for, V),
	send(D, append, label(lab, Texto)),
	send(D, append, button(ok,
			       message(D, return, @nil))),
	send(D, default_button, ok), % Ok: default button
	(   get(D, confirm, _Answer) % This blocks!
	->  send(D, destroy)
	;   true
	).

gr_pregunta(V, Preg, Resp) :-
	new(D, dialog('Pregunta')),
	send(D, transient_for, V),
        send(D, append,
             label(lab, Preg)),
	send(D, append,
             new(TI, text_item('', ''))),
        send(D, append,
             button(ok, message(D, return,
                                TI?selection))),
        send(D, default_button, ok), % Ok: default button
        get(D, confirm, Answer),     % This blocks!
        send(D, destroy),
	Answer = Resp.

gr_opciones(V, Texto, Opciones, Resp) :-
	new(D, dialog('Opciones')),
	send(D, transient_for, V),
	send(D, append, label(lab, Texto)),
	forall(member(O, Opciones),
	       send(D, append, button(O,
			       message(D, return, O)))),
	get(D, confirm,Answer),
	send(D, destroy),
	Resp = Answer.

gr_estado(MV,NuevoEstado) :-
	send(MV, report, progress,'%s',NuevoEstado).

% --------------------------
% Manejo de coordenadas
% --------------------------

coords(F,C,T,Dir,Dist):-
    nonvar(F),nonvar(C),!,
    NS is F-T-2,
    EW is C-T-2,
    dir(NS,EW,Dir),
    Dist is max(abs(NS),abs(EW)).
coords(F,C,T,Dir,Dist):-
    dcoords(Dir,DNS,DEW),
    F is T + Dist*DNS + 2,
    C is T + Dist*DEW + 2.

valid_coords(T2,T2,T):-
    T2 is T + 2,!,fail.
valid_coords(_,T2,T):-
    T2 is T + 2,!.
valid_coords(T2,_,T):-
    T2 is T + 2,!.
valid_coords(FC,FC,_):-
    !.
valid_coords(F,C,T):-
    F is 2*(T+2)-C.

dir(0,EW,e):-
    EW > 0,!.
dir(0,EW,w):-
    EW < 0,!.
dir(NS,0,n):-
    NS < 0,!.
dir(NS,0,s):-
    NS > 0,!.
dir(NSEW,NSEW,se):-
    NSEW > 0,!.
dir(NSEW,NSEW,nw):-
    NSEW < 0,!.
dir(NS,EW,sw):-
    NS \= EW, NS > 0,!.
dir(NS,EW,ne):-
    NS \= EW, NS < 0.

dcoords(n,-1,0).
dcoords(ne,-1,1).
dcoords(e,0,1).
dcoords(se,1,1).
dcoords(s,1,0).
dcoords(sw,1,-1).
dcoords(w,0,-1).
dcoords(nw,-1,-1).

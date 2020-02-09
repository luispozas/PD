%LUIS POZAS PALOMO - SESION 7

% EJERCICIO 1: ----------------------------------

elimina1([ ],X,[ ]).
elimina1([X|R],Y,NR) :- Y == X, elimina1(R,Y, NR).
elimina1([X|R],Y,[X|NR]) :- Y\== X, elimina1(R,Y,NR).


elimina2([ ],X,[ ]).
elimina2([X|R],Y,NR) :- Y = X, elimina2(R,Y, NR).
elimina2([X|R],Y,[X|NR]) :- Y\= X, elimina2(R,Y,NR).


elimina3([ ],X,[ ]).
elimina3([X|R],X,NR) :- elimina3(R,X,NR).
elimina3([X|R],Y,[X|NR]) :- Y\== X, elimina3(R,Y,NR).

% En el (a) y en el (b) cuando probamos la segunda consulta vemos como no salen los datos esperados porque
%  en el (a) no hay igualdad sintactica nunca entre la variable Y y el termino X y en el (b) solo unifica con el primer
% elemento de la lista y a partir de ahi no puede probar los demas terminos de la lista.

% EJERCICIO 2: ------------------------------------

arbol(E, I, D).

simetrico(void, void).
simetrico(arbol(E, I1, D1), arbol(E, I2, D2)) :-
	simetrico(I1,D2),
	simetrico(D1,I2).

suma_tree(void, 0).
suma_tree(arbol(E,I,D), S) :-
	suma_tree(I, S1),
	suma_tree(D, S2),
	S is E + S1 + S2.


maxveces1(void, []).
maxveces1(arbol(E, I, D), E, N, L) :-
	maxveces1(I, L1),
	maxveces1(D, L2).

% EJERCICIO 3: -------------------------------

subtermino(S, S).
subtermino(S,T) :- \+atomic(T), arg(_, T, S1), subtermino(S, S1).

% EJERCICIO 4: -------------------------------

hanoi(1,A,B,C,[A -> B]).
hanoi(N,A,B,C,L):- N>1, N1 is N-1, hanoi(N1,A,C,B,L1), hanoi(N1,C,B,A,L2), append(L1,[A -> B|L2],L).




   
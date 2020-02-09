%LUIS POZAS PALOMO - SESION 6

%------------------------ EJERCICIO 1 -----------------------------:
% ?- profesion(X,Y), quizas_tenga(Y,taladro).
% X = roberto,
% Y = carpintero.

% Busca una persona en la que en su profesion quizas tenga un taladro.
% Encuentra a Roberto que es carpintero y utiliza un taladro

% ?-  sospechoso(X,Y,relacion_sentimental).
% X = alfonso,
% Y = filomena.

% Busca un asesino X que mato a Y por una relacion sentimental,
% puedo observar ademas que en el predicado busca a la victima
%  sin importar el utensilio utilizado

% ?- asesinado_con(X,Y), sospechoso(Z,Y,_).
% false.

% No existe nadie (X) que sea asesinado con un utensilio (Y) y que ademas 
% sea utlizado por un sospechoso (Z) para matar a alguien

% ?-  profesion(X,Y), quizas_tenga(Y,Z), es(Z,arma_blanca).
% X = alfonso,
% Y = carnicero,
% Z = cuchillo ;

% X = roberto,
% Y = carpintero,
% Z = sierra ;

% Busca una persona X que trabaja en Y en la que sus utensilios de trabajo
% sean considerados arma blanca Z

% ?- tiene_pasado_turbio(X), sospechoso(X,_,_).
% X = juan ;

% X = juan ;

% Busca a una persona X que tenga pasado turbio y que sea sospechoso
% de algo. (puede ser a cualquier persona por cualquier razon).

% A CONTINUACION he añadido nuevos hechos y he visto como encuentra mas soluciones
% como por ejemplo añadiendo el siguiente:

% es(portatil, arma_blanca).

% y consultando este predicado,
% ?- profesion(X,Y), quizas_tenga(Y,Z), es(Z,arma_blanca).

% da como resultado:
% X = juan,
% Y = informatico,
% Z = portatil ;

% -------------------  EJERCICIO 2 ----------------------------

mezcla([], _, []).
mezcla(_, [], []).
mezcla([X|Xs], [Y|Ys], [X,Y|Z]) :-
	mezcla(Xs, Ys, Z).

% -------------------- EJERCICIO 3 ----------------------------
% ------APARTADO_1:

sublista(L1, L2) :-
	append(L3, L4, L2),
	append(L1, L5, L4).

sublista_1([], _).
sublista_1([X|Xs], [X,Ys]) :- sublista_1(Xs, Ys).
sublista_1([X|Xs], [_|Ys]) :- sublista_1([X|Xs], Ys).

% ------APARTADO_2:

contenida([], _).
contenida([L|L1], L2):- 
    sublista([L], L2),
	contenida(L1, L2).
	
% ------------------- EJERCICIO 4 -----------------------------

nodo(void, 0).
nodo(arbol(X, I, D), 1+Z1+Z2) :-
	nodo(I, Z1),
	nodo(D, Z2).

% El razonamiento que he seguido es que un arbol tiene la suma
% de los nodos del hijo derecho, mas la del izquierdo, y el suyo (+1).
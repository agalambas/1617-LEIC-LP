%		86372 Alexandre Galambas
%		Logica para Programacao
%		2016/2017, 1 Ano, 2 Semestre
%-------------------------------------------------------------------------


:- include('SUDOKU').

%-------------------------------------------------------------------------
%		3.1. Predicados para propagacao de mudancas
%-------------------------------------------------------------------------


/* tira_num_aux/4
% N_Puz e' o puzzle resultante de tirar o numero Num da posicao Pos
de Puz */

% se Num pertencer a Cont, retira-se
tira_num_aux( Num, Puz, Pos, N_Puz) :-
		puzzle_ref( Puz, Pos, Cont),
		member( Num, Cont),!,
		subtract( Cont, [Num], N_Cont),
		puzzle_muda_propaga( Puz, Pos, N_Cont, N_Puz).

% se Num nao pertencer a Cont, o puzzle mantem-se
tira_num_aux(_, Puz, _, Puz).


/* tira_num/4
N_Puz e o puzzle resultante de tirar o numero Num de todas as
posicoes em Posicoes de Puz */

tira_num( Num, Puz, Posicoes, N_Puz) :-
		percorre_muda_Puz( Puz, tira_num_aux( Num), Posicoes, N_Puz).


/* puzzle_muda_propaga/4
N_Puz e o puzzle resultante de tirar o conteudo da posicao Pos por Cont
No caso de Cont ser uma lista unitaria, propaga a mudanca */

% se Cont for uma lista unitaria, troca-se o conteudo de Pos por Cont
% tira-se o El de Cont das posicoes relacionadas
puzzle_muda_propaga( Puz, Pos, Cont, N_Puz) :-
		length( Cont, 1),!,
		puzzle_muda( Puz, Pos, Cont, Aux_Puz),
		posicoes_relacionadas( Pos, Posicoes),
		nth1( 1, Cont, El),
		tira_num( El, Aux_Puz, Posicoes, N_Puz).

% se Cont for uma lista vazia, o puzzle mantem-se
puzzle_muda_propaga( Puz, _, [], Puz) :- !.

% se Cont tiver mais que um elemento, troca-se o conteudo de Pos por Cont
puzzle_muda_propaga( Puz, Pos, Cont, N_Puz) :-
		puzzle_muda( Puz, Pos, Cont, N_Puz).

%-------------------------------------------------------------------------
%		3.2. Predicados para inicializacao de puzzles
%-------------------------------------------------------------------------


/* poss_unitaria/2
Constroi uma lista de numeros que pertencem a listas unitarias */

% quando ja nao houver mais listas parar verificar, termina
poss_unitaria( [], []).

% se a lista P for unitaria, guarda-se o seu conteudo
poss_unitaria( [P|R], Cont) :-
		\+length( P, 1),!,
		poss_unitaria( R, Cont).

% se a lista P nao for unitaria, passa-se para a proxima
poss_unitaria( [P|R], [P|R1]) :-
		poss_unitaria( R, R1).

/* possibilidades/3
Poss e' a lista de numeros possivieis para a posicao Pos de Puzz */

% se Cont_i nao for uma lista unitaria
% Poss contem os numeros que nao perencem a listas unitarias
possibilidades( Pos, Puz, Poss) :-
		puzzle_ref( Puz, Pos, Cont_i),
		\+length( Cont_i, 1),!,
		posicoes_relacionadas( Pos, Posicoes),
		conteudos_posicoes( Puz, Posicoes, Conteudos_aux),
		poss_unitaria( Conteudos_aux, Conteudos),
		append( Conteudos, Cont),
		numeros( L),
		subtract( L, Cont, Poss).

% se Cont_i for uma lista unitaria, Poss e' o proprio numero
possibilidades( Pos, Puz, Poss) :-
		puzzle_ref( Puz, Pos, Poss).


/* inicializa_aux/3
N_Puz e' o puzzle resultante de colocar na posicao Pos de Puz
a lista com os numeros possiveis para essa posicao */

inicializa_aux( Puz, Pos, N_Puz) :-
		possibilidades( Pos, Puz, Poss),
		puzzle_muda_propaga(Puz, Pos, Poss, N_Puz).


/* inicializa/2
N_Puz e' o puzzle resultante de inicializar o puzzle Puz */

inicializa( Puz, N_Puz) :-
		todas_posicoes(Posicoes),
		percorre_muda_Puz( Puz, inicializa_aux, Posicoes, N_Puz).

%-------------------------------------------------------------------------
%		3.3. Predicados para inspeccao de puzzles
%-------------------------------------------------------------------------


/* pos_numero/4
se o numero Num so aparecer uma vez, descobre-se a sua posicao Pos_Num */

pos_numero( [P|R], Num, [_|R1], Pos_Num) :-
		\+member( Num, P),!,
		pos_numero( R, Num, R1, Pos_Num).

pos_numero( _, _, [P1|_], P1).


/* so_aparece_uma_vez/4
Pos_Num e' a unica posicao de Posicoes onde o numero Num aparece */

so_aparece_uma_vez( Puz, Num, Posicoes, Pos_Num) :-
		conteudos_posicoes( Puz, Posicoes, Conteudos),
		append( Conteudos, Cont),
		length( Cont, Comp),
		subtract( Cont, [Num], N_Cont),
		length( N_Cont, N_Comp),
		Rep is Comp - N_Comp,
		Rep =:= 1,
		pos_numero( Conteudos, Num, Posicoes, Pos_Num).


/* inspecciona_num/4
Inspeccionar o grupo Posicoes de Puz para o numero Num */

% se o numero Num so aparece uma vez
% muda-se o conteudo da posicao Pos_Num para Num
inspecciona_num( Posicoes, Puz, Num, N_Puz) :-
		so_aparece_uma_vez( Puz, Num, Posicoes, Pos_Num),!,
		puzzle_muda_propaga( Puz, Pos_Num, [Num], N_Puz).

% se o numero Num aparecer mais que uma vez, o puzzle mantem-se
inspecciona_num( _, Puz, _, Puz).


/* inspecciona_grupo/4
executa o predicado inspecciona_num, numero a numero */

inspecciona_grupo( _, Puz, [], Puz) :- !.

inspecciona_grupo( Gr, Puz, [P|R], N_Puz) :-
		inspecciona_num( Gr, Puz, P, Aux_Puz),
		inspecciona_grupo( Gr, Aux_Puz, R, N_Puz).


/* inspecciona_grupo/3
inspecionar o grupo Gr de de Puz para todos os numeros possiveis */

inspecciona_grupo( Puz, Gr, N_Puz) :-
		numeros( L),
		inspecciona_grupo( Gr, Puz, L, N_Puz).


/* inspecciona/2
inspecciona todos os grupos do Puz para todos os numeros possiveis */

inspecciona( Puz, N_Puz) :-
		grupos( Gr),
		percorre_muda_Puz( Puz, inspecciona_grupo, Gr, N_Puz).

%-------------------------------------------------------------------------
%		3.4. Predicados para verificacao de solucoes
%-------------------------------------------------------------------------


/* gr_unitaria/1
verifica se as listas de Gr sao todas unitarias */

gr_unitaria( []) :- !.

gr_unitaria( [P|R]) :-
		length( P, 1),
		gr_unitaria( R).


/* grupo_correcto/3
o grupo Gr de Puz contem todos os numeros de Nums sem repeticoes */

grupo_correcto( Puz, Nums, Gr) :-
		conteudos_posicoes( Puz, Gr, Conteudos),
		gr_unitaria( Conteudos),
		append( Conteudos, Cont),
		msort( Cont, Nums).


/* solucao/3
verifica que cada grupo tem todos os numeros possivieis sem repeticoes */

solucao( _, _, []) :- !.

solucao( Puz, L, [P|R]) :-
		grupo_correcto( Puz, L, P),
		solucao( Puz, L, R).


/* solucao/1
significa que Puz e' uma solucao */

solucao( Puz) :-
		grupos( Gr),
		numeros( L),
		solucao( Puz, L, Gr).

%-------------------------------------------------------------------------
%		3.5. Predicado para resolucao de puzzles
%-------------------------------------------------------------------------


/* resolve_pos/3
Pos e' a primeira posicao nao unitaria do puzzle */

resolve_pos( Puz, [P|R], Pos) :-
		puzzle_ref( Puz, P, Cont),
		length( Cont, 1),!,
		resolve_pos( Puz, R, Pos).

resolve_pos( _, [P|_], P).


/* resolve_aux/2
testa os numeros possiveis nas posicoes nao unitarias
ate chegar a uma solucao */

resolve_aux( Puz, Sol) :-
		Sol = Puz,
		solucao( Sol),!.

resolve_aux( Puz, Sol) :-
		todas_posicoes( Posicoes),
		resolve_pos( Puz, Posicoes, Pos),
		puzzle_ref( Puz, Pos, Cont),
		member( El, Cont),
		puzzle_muda_propaga( Puz, Pos, [El], N_Puz),
		resolve_aux( N_Puz, Sol).


/* resolve/2
Sol e' uma solucao de Puz */

% preenche todas as posicoes do puzzle
resolve( Puz, Sol) :-
		inicializa( Puz, Aux_Puz),
		inspecciona( Aux_Puz, N_Puz),
		resolve_aux( N_Puz, Sol).

% ---------------------------------------------------------
% Módulo responsável pela manipulação do tabuleiro do jogo.
%
% Funções principais:
% - Gerar bugs aleatórios
% - Criar linhas e tabuleiro inicial
% - Acessar células do tabuleiro
% - Alterar valores do tabuleiro
% - Realizar troca de peças
% ---------------------------------------------------------

:- module(board, [random_bug/1, generate_row/2, generate_board/2, init_board/1, cell/4, valid_pos/2, replace/4, set_cell/5, swap/6]).
:- use_module(library(random)).
:- use_module('config.pl').

% ---------------------------------------------------------
% random_bug/1
% Seleciona aleatoriamente um tipo de bug definido em
% config.pl através do predicado bug_types.

random_bug(Bug) :-
    bug_types(Types),
    random_member(Bug, Types).

% ---------------------------------------------------------
% generate_row/2
% Gera uma linha do tabuleiro contendo N bugs aleatórios.

generate_row(N, Row) :-
    length(Row, N),
    maplist({}/[X]>>random_bug(X), Row).

% ---------------------------------------------------------
% generate_board/2
% Gera o tabuleiro completo com N linhas e N colunas.
% Cada linha é gerada usando generate_row.

generate_board(N, Board) :-
    length(Board, N),
    maplist(generate_row(N), Board).

% ---------------------------------------------------------
% init_board/1
% Inicializa o tabuleiro do jogo utilizando o tamanho
% definido em config.pl através do predicado size.

init_board(Board) :-
    size(N),
    generate_board(N, Board).

% ---------------------------------------------------------
% valid_pos/2
% Verifica se uma posição (R,C) está dentro dos limites
% válidos do tabuleiro.

valid_pos(R,C) :-
    size(N),
    R>=1, R=<N,
    C>=1, C=<N.

% ---------------------------------------------------------
% cell/4
% Obtém o valor armazenado em uma posição específica
% (R,C) do tabuleiro.

cell(Board,R,C,Val) :-
    valid_pos(R,C),
    nth1(R,Board,Row),
    nth1(C,Row,Val).

% ---------------------------------------------------------
% replace/4
% Substitui um elemento em uma lista na posição indicada
% pelo índice.

replace([_|T],1,X,[X|T]).
replace([H|T],I,X,[H|R]) :-
    I>1,
    I1 is I-1,
    replace(T,I1,X,R).

% ---------------------------------------------------------
% set_cell/5
% Atualiza o valor de uma célula específica do tabuleiro
% retornando um novo tabuleiro com a alteração.

set_cell(Board,R,C,Val,NewBoard) :-
    nth1(R,Board,Row),
    replace(Row,C,Val,NewRow),
    replace(Board,R,NewRow,NewBoard).

% ---------------------------------------------------------
% swap/6
% Realiza a troca de duas posições do tabuleiro.
% Usado quando o jogador move duas peças adjacentes.

swap(Board,R1,C1,R2,C2,NewBoard) :-
    cell(Board,R1,C1,V1),
    cell(Board,R2,C2,V2),
    set_cell(Board,R1,C1,V2,B1),
    set_cell(B1,R2,C2,V1,NewBoard).
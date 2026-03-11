% ---------------------------------------------------------
% Módulo responsável pela renderização do tabuleiro.
%
% Funções principais:
% - Desenhar o tabuleiro na tela
% - Mostrar números de linhas e colunas
% - Exibir cada célula com sua cor/símbolo
% ---------------------------------------------------------

:- module(render, [render/1]).
:- use_module('config.pl').

% ---------------------------------------------------------
% draw_board/1
% Percorre todas as linhas do tabuleiro e chama
% o predicado responsável por desenhá-las.

draw_board(Board) :-
    draw_rows(Board, 1).

% ---------------------------------------------------------
% draw_rows/2
% Percorre recursivamente as linhas do tabuleiro
% exibindo cada uma com seu número correspondente.

draw_rows([], _).
draw_rows([Row|Rest], I) :-
    format("~d ", [I]),
    draw_row(Row),
    nl,
    I2 is I+1,
    draw_rows(Rest, I2).

% ---------------------------------------------------------
% draw_row/1
% Percorre cada célula da linha e imprime
% seu símbolo colorido correspondente.

draw_row([]).
draw_row([Cell|Rest]) :-
    ansi_color(Cell, Symbol),
    write(Symbol), write(" "),
    draw_row(Rest).

% ---------------------------------------------------------
% draw_header/0
% Desenha o cabeçalho do tabuleiro mostrando
% os números das colunas.

draw_header :-
    size(N),
    write("  "),
    forall(between(1,N,C), format("~d ", [C])),
    nl.

% ---------------------------------------------------------
% render/1
% Predicado principal de renderização.
% Exibe primeiro o cabeçalho e depois o tabuleiro.

render(Board) :-
    draw_header,
    draw_board(Board).
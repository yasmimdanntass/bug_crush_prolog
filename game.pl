:- use_module(library(random)).

% ===== CONFIG =====

size(8). % Tamanho

bug_types([r,g,b,y,p]).  % red, green, blue, yellow, purple

ansi_color(r, "\e[31m●\e[0m").
ansi_color(g, "\e[32m●\e[0m").
ansi_color(b, "\e[34m●\e[0m").
ansi_color(y, "\e[33m●\e[0m").
ansi_color(p, "\e[35m●\e[0m").

% ===== GERAÇÃO DO TABULEIRO =====

random_bug(Bug) :-
    bug_types(Types),
    random_member(Bug, Types).

generate_row(N, Row) :-
    length(Row, N),
    maplist({}/[X]>>random_bug(X), Row).

generate_board(N, Board) :-
    length(Board, N),
    maplist(generate_row(N), Board).

init_board(Board) :-
    size(N),
    generate_board(N, Board).

% ===== MOSTRAR O TABULEIRO =====

draw_board(Board) :-
    draw_rows(Board, 1).

draw_rows([], _).
draw_rows([Row|Rest], I) :-
    format("~d ", [I]),
    draw_row(Row),
    nl,
    I2 is I+1,
    draw_rows(Rest, I2).

draw_row([]).
draw_row([Cell|Rest]) :-
    ansi_color(Cell, Symbol),
    write(Symbol), write(" "),
    draw_row(Rest).

draw_header :-
    size(N),
    write("  "),
    forall(between(1,N,C), format("~d ", [C])),
    nl.

render(Board) :-
    draw_header,
    draw_board(Board).

% ===== ATUALIZAR AS PEÇAS  =====

cell(Board,R,C,Val) :-
    nth1(R,Board,Row),
    nth1(C,Row,Val).

replace([_|T],1,X,[X|T]).
replace([H|T],I,X,[H|R]) :-
    I>1,
    I1 is I-1,
    replace(T,I1,X,R).

set_cell(Board,R,C,Val,NewBoard) :-
    nth1(R,Board,Row),
    replace(Row,C,Val,NewRow),
    replace(Board,R,NewRow,NewBoard).

swap(Board,R1,C1,R2,C2,NewBoard) :-
    cell(Board,R1,C1,V1),
    cell(Board,R2,C2,V2),
    set_cell(Board,R1,C1,V2,B1),
    set_cell(B1,R2,C2,V1,NewBoard).

% ===== MOVER PEÇAS =====

direction_delta(w,-1,0).
direction_delta(s, 1,0).
direction_delta(a,0,-1).
direction_delta(d,0, 1).

valid_pos(R,C) :-
    size(N),
    R>=1, R=<N,
    C>=1, C=<N.

make_move(Board,R,C,Dir,NewBoard) :-
    direction_delta(Dir,DR,DC),
    R2 is R+DR,
    C2 is C+DC,
    valid_pos(R2,C2),
    swap(Board,R,C,R2,C2,NewBoard).

% ===== ENTRADA =====

read_move(R,C,D) :-
    write("Linha: "), read(R),
    write("Coluna: "), read(C),
    write("Dir (w/a/s/d): "), read(D),
    nl.

% ===== GAME LOOP =====

game_loop(Board) :-
    render(Board),
    read_move(R,C,D),
    (
        make_move(Board,R,C,D,Next)
    ->
        game_loop(Next)
    ;
        write("Movimento inválido!"), nl,
        game_loop(Board)
    ).

start :-
    init_board(Board),
    game_loop(Board).
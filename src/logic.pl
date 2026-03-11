:- module(logic, [make_move/5, resolve_board/3, resolve_silent/2]).
:- use_module(library(clpfd)).
:- use_module('config.pl').
:- use_module('board.pl').
:- use_module('render.pl').

direction_delta(w,-1,0).
direction_delta(s, 1,0).
direction_delta(a,0,-1).
direction_delta(d,0, 1).

make_move(Board,R,C,Dir,NewBoard) :-
    direction_delta(Dir,DR,DC),
    R2 is R+DR,
    C2 is C+DC,
    valid_pos(R2,C2),
    swap(Board,R,C,R2,C2,NewBoard).

is_match(Board, R, C) :-
    cell(Board, R, C, V), V \== nil,
    (
        (C1 is C-1, C2 is C-2, cell(Board, R, C1, V), cell(Board, R, C2, V)) ;
        (C1 is C-1, C2 is C+1, cell(Board, R, C1, V), cell(Board, R, C2, V)) ;
        (C1 is C+1, C2 is C+2, cell(Board, R, C1, V), cell(Board, R, C2, V)) ;
        (R1 is R-1, R2 is R-2, cell(Board, R1, C, V), cell(Board, R2, C, V)) ;
        (R1 is R-1, R2 is R+1, cell(Board, R1, C, V), cell(Board, R2, C, V)) ;
        (R1 is R+1, R2 is R+2, cell(Board, R1, C, V), cell(Board, R2, C, V))
    ).

clear_board(Board, NewBoard) :-
    size(N),
    findall(Row, (
        between(1, N, R),
        findall(Cell, (
            between(1, N, C),
            (is_match(Board, R, C) -> Cell = nil ; cell(Board, R, C, Cell))
        ), Row)
    ), NewBoard).

% === CONTAGEM DE PONTOS ===

match_coord(Board, R, C) :-
    size(N),
    between(1,N,R),
    between(1,N,C),
    is_match(Board,R,C).

all_matches(Board, Coords) :-
    findall((R,C), match_coord(Board,R,C), Raw),
    sort(Raw, Coords).

points_group(Size, Points) :-
    ( Size =:= 3 -> Points = 10
    ; Size =:= 4 -> Points = 20
    ; Size =:= 5 -> Points = 50
    ; Size =:= 6 -> Points = 100
    ; Size >= 7  -> Points = 500
    ; Points = 0 ).

group_points(Coords, Points) :-
    length(Coords, Size),
    points_group(Size, Points).

points_from_groups(Groups, Points) :-
    maplist(group_points, Groups, List),
    sum_list(List, Points).

detect_groups(Board, Groups) :-
    all_matches(Board, Coords),
    group_by_lines(Coords, HGroups),
    group_by_columns(Coords, VGroups),
    append(HGroups, VGroups, Groups).

group_by_lines(Coords, Groups) :-
    size(N),
    findall(GroupCoords,
        (
            between(1,N,R),
            findall(C, member((R,C),Coords), Cols0),
            sort(Cols0, Cols),
            consecutive_segments(Cols, Blocks),
            member(Block, Blocks),
            length(Block, L),
            L >= 3,
            findall((R,C), member(C, Block), GroupCoords)
        ),
    Groups).

group_by_columns(Coords, Groups) :-
    size(N),
    findall(GroupCoords,
        (
            between(1,N,C),
            findall(R, member((R,C),Coords), Rows0),
            sort(Rows0, Rows),
            consecutive_segments(Rows, Blocks),
            member(Block, Blocks),
            length(Block, L),
            L >= 3,
            findall((R,C), member(R, Block), GroupCoords)
        ),
    Groups).

consecutive_segments([], []).

consecutive_segments([H|T], [Segment|Rest]) :-
    build_segment(H, T, Segment, Remaining),
    consecutive_segments(Remaining, Rest).

build_segment(X, [], [X], []).

build_segment(X, [Y|T], [X|Segment], Remaining) :-
    Y =:= X + 1,
    build_segment(Y, T, Segment, Remaining).

build_segment(X, [Y|T], [X], [Y|T]) :-
    Y =\= X + 1.

% === ANIMAÇÃO E TERMINAL ===

clear_screen :-
    format('~c[2J~c[3J~c[999;1H', [27, 27, 27]),
    flush_output.

% === GRAVIDADE ===

fall_col([X, nil | T], [nil, X | T]) :- X \== nil.
fall_col([H | T], [H | T1]) :- fall_col(T, T1).
fall_col([nil | T], [Bug | T]) :- random_bug(Bug).
fall_step(Col, NextCol) :- fall_col(Col, NextCol), !.
fall_step(Col, Col).

apply_gravity_step(Board, NextBoard) :-
    transpose(Board, Cols),
    maplist(fall_step, Cols, NextCols),
    transpose(NextCols, NextBoard).

animate_gravity(Board, FinalBoard) :-
    apply_gravity_step(Board, NextBoard),
    ( Board \== NextBoard ->
        clear_screen,            % <--- Limpa a tela antes do render
        render(NextBoard), 
        sleep(0.3),
        animate_gravity(NextBoard, FinalBoard)
    ; FinalBoard = Board ).

resolve_board(Board, FinalBoard, Points) :-
    resolve_board(Board, FinalBoard, 0, Points).

resolve_board(Board, FinalBoard, Combo, Points) :-
    detect_groups(Board, Groups),

    ( Groups \== [] ->

        points_from_groups(Groups, BasePoints),

        Bonus is 0.5 * Combo,
        Multiplier is 1 + Bonus,
        RoundPoints is round(BasePoints * Multiplier),

        clear_board(Board, Cleared),

        clear_screen,            % <--- Limpa a tela antes de mostrar a explosão
        render(Cleared),
        write("Explosao! Pontos: "), write(RoundPoints), nl,
        sleep(0.8),

        animate_gravity(Cleared, Fallen),

        NextCombo is Combo + 1,

        resolve_board(Fallen, FinalBoard, NextCombo, CascadePoints),

        Points is RoundPoints + CascadePoints

    ;
        FinalBoard = Board,
        Points = 0
    ).

% === CONFIGURANDO A EXIBIÇÃO PRA SÓ ACONTECER QUANDO O TABULEIRO ESTIVER PRONTO ===

gravity_col_instant(Col, NewCol) :-
    include(\==(nil), Col, Solid),
    length(Col, N),
    length(Solid, S),
    Missing is N - S,
    length(Pad, Missing),
    maplist({}/[X]>>random_bug(X), Pad),
    append(Pad, Solid, NewCol).

% Resolve cascatas silenciosamente
resolve_silent(Board, FinalBoard) :-
    clear_board(Board, Cleared),
    ( Board \== Cleared ->
        transpose(Cleared, Cols),
        maplist(gravity_col_instant, Cols, NewCols),
        transpose(NewCols, Fallen),
        resolve_silent(Fallen, FinalBoard)
    ; FinalBoard = Board ).
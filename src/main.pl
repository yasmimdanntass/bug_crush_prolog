:- module(main, [start/0]).
:- use_module('board.pl').
:- use_module('render.pl').
:- use_module('logic.pl').

read_move(R,C,D) :-
    write("Linha: "), read(R),
    write("Coluna: "), read(C),
    write("Dir (w/a/s/d): "), read(D),
    nl.

game_loop(Board) :-
    render(Board),
    read_move(R,C,D),
    (
        make_move(Board,R,C,D,Next)
    ->
        % Mostra o tabuleiro logo apos a troca manual do jogador
        render(Next), write("Trocando pecas..."), nl, sleep(0.8),
        
        % Inicia a verificacao de matches e quedas
        resolve_board(Next, ResolvedBoard, Points),
        write("Pontos ganhos: "), write(Points), nl,

        game_loop(ResolvedBoard)
    ;
        write("Movimento invalido!"), nl,
        game_loop(Board)
    ).

start :-
    write("=== BEM-VINDO AO BUG CRUSH ==="), nl,
    init_board(Board),
    
    % Resolve os matches da geracao inicial de forma 100% silenciosa
    resolve_silent(Board, ReadyBoard),
    
    game_loop(ReadyBoard).
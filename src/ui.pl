% ---------------------------------------------------------
% Módulo responsável pela interface do usuário (UI).
%
% Este modulo controla:
% - Telas do jogo (inicial, menu, regras, instruções)
% - Entrada de dados do jogador
% - Exibição do HUD (informações do jogador)
% - Tela de fim de jogo
% - Limpeza da tela
% ---------------------------------------------------------

:- module(ui, [
    clear_screen/0,
    initial_screen/0,
    wait_for_m/0,
    main_menu/1,
    login_screen/1,
    rules_screen/0,
    instructions_screen/0,
    render_hud/3,
    game_over_screen/2
]).

:- encoding(utf8).
:- use_module(library(readutil)).

% ---------------------------------------------------------
% Definição de cores ANSI usadas no terminal
% ---------------------------------------------------------
green('\e[32m').
reset('\e[0m').

% ---------------------------------------------------------
% clear_screen/0
% Simula limpeza da tela imprimindo várias linhas vazias.
% ---------------------------------------------------------
clear_screen :-
    print_newlines(40).

% ---------------------------------------------------------
% print_newlines/1
% Imprime N quebras de linha recursivamente.
% ---------------------------------------------------------
print_newlines(0) :- !. 
print_newlines(N) :-
    N > 0,
    nl,
    N1 is N - 1,
    print_newlines(N1).

% ---------------------------------------------------------
% initial_screen/0
% Exibe a tela inicial do jogo com o título em ASCII
% e aguarda o jogador pressionar 'M' para continuar.
% ---------------------------------------------------------
initial_screen :-
    clear_screen,
    green(G), reset(R),
    write(G), writeln('                                              '), write(R),
    write(G), writeln('  ____  _    _  _____      _____  _____  _    _  _____ _    _ '), write(R),
    write(G), writeln(' |  _ \\| |  | |/ ____|    / ____ |  __ \\| |  | |/ ____| |  | |'), write(R),
    write(G), writeln(' | |_) | |  | | |  __     | |    | |__) | |  | | (___ | |__| |'), write(R),
    write(G), writeln(' |  _ <| |  | | | |_ |    | |    |  _  /| |  | |\\___ \\|  __  |'), write(R),
    write(G), writeln(' | |_) | |__| | |__| |    | |____| | \\ \\| |__| |____) | |  | |'), write(R),
    write(G), writeln(' |____/ \\____/ \\_____|     \\_____|_|  \\_\\\\____/|_____/|_|  |_|'), write(R),
    nl,
    write(G), writeln(' [ Pressione a tecla \'M\' para ir ao Menu Inicial ]'), write(R),
    wait_for_m.

% ---------------------------------------------------------
% wait_for_m/0
% Aguarda o jogador digitar 'M' ou 'm' para continuar.
% Caso contrário, solicita novamente.
% ---------------------------------------------------------
wait_for_m :-
    write('> '), flush_output(current_output),
    read_line_to_string(user_input, Input),
    (( Input == "m" ; Input == "M") 
    -> true
    ;  writeln('Entrada inválida. Aperte \'M\' para continuar.'),
       wait_for_m
    ).

% ---------------------------------------------------------
% main_menu/1
% Exibe o menu principal do jogo e lê a opção escolhida.
% ---------------------------------------------------------
main_menu(Opcao) :-
    clear_screen,
    writeln('===================='),
    writeln('      BUG CRUSH     '),
    writeln('===================='),
    writeln('1-Iniciar Jogo'),
    writeln('2-Regras'),
    writeln('3-Instruções'),
    writeln('4-Sair'),
    write('Escolha uma opção: '), flush_output(current_output),
    
    read_line_to_string(user_input, Input),
    (  number_string(OpcaoNum, Input),
       member(OpcaoNum, [1, 2, 3, 4]) 
    -> Opcao = OpcaoNum
    ;  writeln('Opção inválida. Pressione [ENTER] e tente novamente.'),
       read_line_to_string(user_input,  _),
       main_menu(Opcao)
    ).

% ---------------------------------------------------------
% login_screen/1
% Solicita o nome do jogador e exibe mensagem de boas-vindas.
% ---------------------------------------------------------
login_screen(Name) :-
    clear_screen,
    writeln('===== LOGIN =====\n'),
    write('Digite o seu nome: '), flush_output(current_output),
    read_line_to_string(user_input, Name),
    nl,
    format('Bem vindo(a), ~w!~n', [Name]), 
    writeln('Pressione [ENTER] para iniciar o jogo...'),
    read_line_to_string(user_input, _).

% ---------------------------------------------------------
% rules_screen/0
% Exibe as regras básicas do jogo.
% ---------------------------------------------------------
rules_screen :-
    clear_screen,
    writeln('===== REGRAS ===== '),
    writeln('1-Troque duas peças vizinhas na horizontal ou na vertical.'),
    writeln('2-Forme combinações de 3 ou mais peças iguais.'),
    writeln('3-Cada troca consome um movimento.'),
    writeln('4-A fase termina quando os movimentos acabam ou quando o jogador decide sair.'),
    writeln('5-O jogador vence quando atinge 500 pontos.\n'),
    writeln('Pressione [ENTER] para retornar ao Menu Inicial'),
    read_line_to_string(user_input, _).

% ---------------------------------------------------------
% instructions_screen/0
% Exibe instruções de como jogar e os comandos de movimento.
% ---------------------------------------------------------
instructions_screen :-
    clear_screen,
    writeln('===== INSTRUÇÕES ====='),
    writeln('Digite as coordenadas das peças e a direção do movimento para trocar suas posições.\n'),
    writeln('Os comandos para as direções funcionam da seguinte maneira:'),
    writeln(' - w: mover a peça para cima'),
    writeln(' - a: mover a peça para a esquerda'),
    writeln(' - s: mover a peça para baixo'),
    writeln(' - d: mover a peça para a direita\n'),
    writeln('Formato: \n Linha: X \n Coluna: Y \n Direção(w/a/s/d): Z \n'),
    writeln('Pressione [ENTER] para retornar ao Menu Inicial'),
    read_line_to_string(user_input, _).

% ---------------------------------------------------------
% print_chars/2
% Imprime um caractere C repetido N vezes.
% Usado para construir bordas de caixas no HUD.
% ---------------------------------------------------------
print_chars(0, _) :- !.
print_chars(N, C) :- 
    N > 0, write(C), N1 is N - 1, print_chars(N1, C).

% ---------------------------------------------------------
% box_width/1
% Define a largura padrão das caixas exibidas no HUD.
% ---------------------------------------------------------
box_width(40).

% ---------------------------------------------------------
% format_line/2
% Formata uma linha dentro da caixa do HUD com rótulo
% e valor alinhados corretamente.
% ---------------------------------------------------------
format_line(Label, Value) :-
    box_width(BoxWidth),
    string_length(Label, LenL),
    string_length(Value, LenV),
    PaddingLen is BoxWidth - (LenL + LenV),
    write('║ '), write(Label), write(Value), print_chars(PaddingLen, ' '), writeln(' ║').

% ---------------------------------------------------------
% render_hud/3
% Exibe o HUD do jogo contendo:
% - Nome do jogador
% - Pontuação
% - Movimentos restantes
% ---------------------------------------------------------
render_hud(Name, Points, Movements) :-
    box_width(BoxWidth),
    BorderWidth is BoxWidth + 2,
    nl, nl,
    write('╔'), print_chars(BorderWidth, '═'), writeln('╗'),
    format_line("Jogador: ", Name),
    
    number_string(Points, PStr),
    format_line("Pontos:  ", PStr),
    
    number_string(Movements, MStr),
    format_line("Movimentos Restantes: ", MStr),
    
    write('╚'), print_chars(BorderWidth, '═'), writeln('╝'), nl.

% ---------------------------------------------------------
% game_over_screen/2
% Exibe a tela de fim de jogo com o resultado final
% e informa se o jogador venceu ou perdeu.
% ---------------------------------------------------------
game_over_screen(Name, Points) :-
    clear_screen,
    writeln('╔═══════════════════════════════════╗'),
    writeln('║             FIM DE JOGO           ║'),
    writeln('╚═══════════════════════════════════╝'),
    format(' Jogador: ~w~n', [Name]),
    format(' Pontuação final: ~w~n~n', [Points]),
    
    % Lógica de vitória ou derrota
    (Points >= 500
    ->  writeln('      PARABÉNS! VOCÊ VENCEU!    ')
    ;   writeln('      QUE PENA! VOCÊ PERDEU.    ')
    ),
    
    nl, writeln(' Pressione [ENTER] para voltar ao menu'),
    read_line_to_string(user_input, _).
% ---------------------------------------------------------
% Responsável por armazenar constantes utilizadas
% em todo o sistema, como:
% - Tamanho do tabuleiro
% - Tipos de bugs disponíveis
% - Cores ANSI usadas na renderização do tabuleiro
% ---------------------------------------------------------

:- module(config, [size/1, bug_types/1, ansi_color/2]).

size(8). 

bug_types([r,g,b,y,p]).  % red, green, blue, yellow, purple

ansi_color(r, "\e[31mo\e[0m"). % Red
ansi_color(g, "\e[32mo\e[0m"). % Green
ansi_color(b, "\e[34mo\e[0m"). % Blue
ansi_color(y, "\e[33mo\e[0m"). % Yellow
ansi_color(p, "\e[35mo\e[0m"). % Purple
ansi_color(nil, " ").
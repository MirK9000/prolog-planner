
%%% ================================================================
%%% FILE: layout_spiral_tests.pl
%%% Purpose: Minimal tests for spiral placement
%%% ================================================================
:- module(layout_spiral_tests, []).
:- use_module(library(plunit)).
:- use_module(layout_spiral).
:- use_module(rects).
:- use_module(zones).

:- begin_tests(layout_spiral, [setup(setup_rt), cleanup(cleanup_rt)]).

setup_rt :-
  asserta(rects:room_size(6000, 4000)),
  % default policies
  asserta(zones:policy(min_aisle_width_mm, 600)),
  asserta(zones:policy(window_clear_depth_mm, 1000)).

cleanup_rt :-
  retractall(rects:room_size(_,_)),
  retractall(zones:policy(_,_)),
  retractall(zones:forbidden_zone(_,_,_)).

% No obstacles: should place greedily

test('greedy places N=6 tiles 1400x800, grid derived from aisle=600') :-
  layout_spiral:layout_tiles_spiral_opts(6, 1400,800, _{mode:greedy}, Rects, Oris, Meta),
  length(Rects, 6), length(Oris, 6), Meta.grid_mm > 0.

% With an obstacle strip blocking some cells

test('bt fallback can skip a blocked cell and still place N=3') :-
  asserta(zones:forbidden_zone(obs, struct, rect(1400,0, 1400,4000))), % block the 2nd column
  layout_spiral:layout_tiles_spiral_opts(3, 1400,800, _{mode:auto}, Rects, _Oris, _Meta),
  length(Rects, 3).

:- end_tests(layout_spiral).

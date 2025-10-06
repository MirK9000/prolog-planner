:- module(tile_barrier, []).

room(size(10000, 7000)).

% Колонны по бокам
static_object('col-3', column, rect(0, 4200, 1400, 1400), []).
static_object('col-4', column, rect(8400, 4200, 1600, 1200), []).

% Дверь на юге
static_object('door-1', door, rect(1500, 6800, 1200, 200), []).

task(13, size(1400, 800)).

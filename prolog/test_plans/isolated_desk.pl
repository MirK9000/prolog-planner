:- module(isolated_desk, []).

room(size(10000, 7000)).

% Колонны окружают клетку (0,2)
static_object('col-3', column, rect(0, 1800, 2400, 1000), []).
static_object('col-4', column, rect(1400, 2800, 1000, 1400), []).
static_object('col-5', column, rect(0, 4200, 2400, 1000), []).

static_object('door-1', door, rect(1500, 6800, 1200, 200), []).

task(10, size(1400, 800)).

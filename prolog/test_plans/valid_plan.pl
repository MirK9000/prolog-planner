:- module(valid_plan, []).

room(size(10000, 7000)).

static_object('col-1', column, rect(2300, 1800, 1000, 1000), []).
static_object('col-2', column, rect(6700, 1800, 1000, 1000), []).
static_object('door-1', door, rect(1500, 6800, 1200, 200), []).

task(6, size(1400, 800)).

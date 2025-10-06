


:- module(plan, []).

room(size(10000, 7000)).
static_object('cab-1', cabinet, rect(3900, 6400, 1400, 600), []).
static_object('cab-2', cabinet, rect(5500, 6400, 1300, 600), []).
static_object('col-1', column, rect(2300, 1800, 1000, 1000), []).
static_object('col-2', column, rect(6700, 1800, 1000, 1000), []).
static_object('door-1', door, rect(1500, 6800, 1200, 200), []).
static_object('esh-1', electrical_shield, rect(7300, 6800, 800, 200), []).
static_object('fa-1', fire_alarm, rect(900, 6800, 200, 200), []).
static_object('fe-2', fire_extinguisher, rect(200, 6500, 300, 300), []).
static_object('fe-3', fire_extinguisher, rect(9500, 6600, 300, 300), []).
static_object('win-1', window, rect(1600, 0, 2000, 200), []).
static_object('win-2', window, rect(4100, 0, 2000, 200), []).
static_object('win-3', window, rect(6600, 0, 2000, 200), []).
static_object('col-3', column, rect(1500, 3500, 8500, 2300), []).

% task spec
task(6, size(1400, 800)).
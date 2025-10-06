:- begin_tests(connectivity).
:- use_module(connectivity).
:- use_module(plan_loader_spiral).
:- use_module(library(assoc)).
:- use_module(library(ordsets)).

test('isolated_cell - all neighbors blocked') :-
    list_to_ord_set([(0,1),(2,1),(1,0),(1,2)], Blocked),
    connectivity:isolated_cell(Blocked, 3, 3, (1,1)).

test('isolated_cell - has free neighbor', [fail]) :-
    list_to_ord_set([(0,1),(2,1),(1,2)], Blocked),
    connectivity:isolated_cell(Blocked, 3, 3, (1,1)).

test('build_grid_graph - simple case') :-
    Free = [(1,1),(1,2),(2,1)],
    list_to_ord_set([], Blocked),
    connectivity:build_grid_graph(Free, Blocked, 4,4, Graph),
    get_assoc((1,1), Graph, Neigh),
    sort(Neigh, Sorted),
    assertion(Sorted == [(1,2),(2,1)]).

test('bfs_reachable - all connected') :-
    empty_assoc(G0),
    put_assoc((1,1), G0, [(1,2)], G1),
    put_assoc((1,2), G1, [(1,1),(2,2)], G2),
    put_assoc((2,2), G2, [(1,2)], Graph),
    connectivity:bfs_reachable([(1,1)], Graph, Reachable),
    assertion(Reachable == [(1,1),(1,2),(2,2)]).

test('bfs_reachable - has unreachable') :-
    empty_assoc(G0),
    put_assoc((1,1), G0, [(1,2)], G1),
    put_assoc((1,2), G1, [(1,1)], Graph),
    connectivity:bfs_reachable([(1,1)], Graph, Reachable),
    assertion(\+ ord_memberchk((3,3), Reachable)).

% ------------------------------------------------------------------
% Integration tests with sample plans
% ------------------------------------------------------------------
test('valid_plan should succeed',
     [setup(cleanup_plan), cleanup(cleanup_plan)]) :-
    plan_loader_spiral:load_plan('test_plans/valid_plan.pl'),
    plan_loader_spiral:solve_plan_spiral_oriented(Rects, _Oris, _Grid),
    length(Rects, Count),
    assertion(Count >= 6).

test('isolated_desk plan should fail',
     [setup(cleanup_plan), cleanup(cleanup_plan), fail]) :-
    plan_loader_spiral:load_plan('test_plans/isolated_desk.pl'),
    plan_loader_spiral:solve_plan_spiral_oriented(_Rects, _Oris, _Grid).

test('tile_barrier plan should fail',
     [setup(cleanup_plan), cleanup(cleanup_plan), fail]) :-
    plan_loader_spiral:load_plan('test_plans/tile_barrier.pl'),
    plan_loader_spiral:solve_plan_spiral_oriented(_Rects, _Oris, _Grid).

cleanup_plan :-
    catch(plan_loader_spiral:clear_plan, _, true),
    retractall(plan_loader_spiral:current_plan_module(_)).

:- end_tests(connectivity).

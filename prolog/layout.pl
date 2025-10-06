:- module(layout,
    [ layout_desks/5,              % layout_desks(N, Wmm,Hmm, Grid, RectsMM)
      layout_desks_with_orient/6,  % layout_desks_with_orient(N, Wmm,Hmm, Grid, RectsMM, Oris)
      layout_desks_opts/7,         % layout_desks_opts(N,W,H,Grid, Opts, RectsMM, Oris) ; Opts supports no_zones
      run_benchmark/1,
      run_all_benchmarks/0
    ]).

:- use_module(rects).
:- use_module(zones).
:- use_module(paths).     
:- use_module(library(pairs)).
:- use_module(library(lists)).
:- use_module(library(debug)).

:- multifile progress_hook/1.

emit_progress(E) :- ignore(catch(progress_hook(E), _, true)).

/* ----------------------------------------------------------------------
   FRONTIER-АЛГОРИТМ (жадный с откатами)
   - сетка (GridUnits)
   - стартовые точки вдоль левой и верхней стен (шаг = min(Wg,Hg))
   - из точки пробуем обе ориентации (0: W×H, 1: H×W)
   - при успехе добавляем (X+W,Y) и (X,Y+H), чистим фронт (in-room, no-occupied, no-zones, недоминируемые)
   - НОВОЕ: при каждом успешном кандидате инкрементально проверяем эвакуацию
---------------------------------------------------------------------- */

%% Публичные обёртки

layout_desks(N, Wmm,Hmm, Grid, RectsMM) :-
    layout_desks_opts(N, Wmm,Hmm, Grid, [], RectsMM, _).

layout_desks_with_orient(N, Wmm,Hmm, Grid, RectsMM, Oris) :-
    layout_desks_opts(N, Wmm,Hmm, Grid, [], RectsMM, Oris).

layout_desks_opts(N, Wmm,Hmm, Grid, Opts, RectsMM, Oris) :-
    % 0) Комната (мм) — берём САМУЮ БОЛЬШУЮ из всех room_size/2
    room_rect_mm_max(rect(0,0,WrMM,HrMM)),
    must_be_multiple(WrMM, Grid),
    must_be_multiple(HrMM, Grid),
    must_be_multiple(Wmm,  Grid),
    must_be_multiple(Hmm,  Grid),

    % 1) Перевод в клетки
    Wr  is WrMM div Grid,  Hr  is HrMM div Grid,
    Wg  is Wmm  div Grid,  Hg  is Hmm  div Grid,

    % 2) Зоны (в мм -> клетки), с поддержкой опции no_zones
    zones_for_layout(Opts, Grid, ZonesG),

    % 3) Стартовый фронт: вдоль левой И верхней стен (шаг min(Wg,Hg))
    seed_frontier_left_top(Wr,Hr, Wg,Hg, Frontier0),

    % СТАРТ ПОИСКА 
    emit_progress(start(N, WrMM, HrMM, Grid)),

    % 4) Укладка N прямоугольников + эвакуация
    place_frontier(N, Wg,Hg, Wr,Hr, WrMM,HrMM, Grid, ZonesG, [], Frontier0, RectsG, Oris),

    % 5) Назад в мм
    maplist(scale_rect_up(Grid), RectsG, RectsMM).

/* ============================
   FRONTIER core
   ============================ */

% place_frontier(+N, +Wg,+Hg, +Wr,+Hr, +WrMM,+HrMM, +Grid, +Zones, +PlacedIn, +FrontierIn, -PlacedOut, -Oris)
place_frontier(0, _Wg,_Hg, _Wr,_Hr, _WrMM,_HrMM, _Grid, _Zones, Placed, _Frontier, Placed, []) :- !,
    length(Placed, K),
    emit_progress(finish(ok, K)).
place_frontier(N, Wg,Hg, Wr,Hr, WrMM,HrMM, Grid, Zones, Placed0, Frontier0, Placed, [Ori|Oris]) :-
    N > 0,
    sort_frontier_yx(Frontier0, FrontierSorted),
    select(XY, FrontierSorted, FrontierRest),         % backtracking по точкам фронта
    XY = (X,Y),
    orientation_choice(Wg,Hg, Wr, X, Ori),            % две ветки: Ori=0 ; Ori=1
    dims_by_ori(Wg,Hg, Ori, W,H),
    Rect = rect(X,Y,W,H),
    emit_progress(try_place(_{xy:XY, ori:Ori, rect:Rect,
                              placed_so_far:Placed0})),
    (   fits_room(Wr,Hr, Rect) -> true
    ;   emit_progress(reject(Rect, room_bounds)), fail
    ),
    (   no_overlap_with_list(Rect, Placed0) -> true
    ;   emit_progress(reject(Rect, overlap_desks)), fail
    ),
    (   no_overlap_with_list(Rect, Zones) -> true
    ;   emit_progress(reject(Rect, overlap_zones)), fail
    ),

    gather_doors(Doors),
    ( Doors == [] -> true
    ; get_min_aisle_width(MinW),
      maplist(scale_rect_up(Grid), [Rect|Placed0], DesksMM),
      (   paths:check_evac_all(WrMM,HrMM, Grid, Doors, MinW, DesksMM)
      ->  true
      ;   emit_progress(reject(Rect, evac_blocked)), fail
      )
    ),

    % дальше как было
    extend_frontier(Rect, FrontierRest, Wr,Hr, [Rect|Placed0], Zones, Frontier1),
    length(Frontier1, FLen),
    length(Placed0, P0),
    Total is N + P0,           % всего надо поставить
    PlacedNow is P0 + 1,       % уже поставлено
    emit_progress(placed(PlacedNow, Total, Rect, FLen)),
    N1 is N - 1,
    place_frontier(N1, Wg,Hg, Wr,Hr, WrMM,HrMM, Grid, Zones, [Rect|Placed0], Frontier1, Placed, Oris).

gather_doors(Doors) :-
    findall(door(Id,side(S),offset(Off),width(W)),
            zones:door(Id, side(S), offset(Off), width(W)),
            Doors).

% локальный хелпер: получить минимальную ширину прохода (мм)
get_min_aisle_width(MinW) :-
    ( zones:policy(min_aisle_width_mm, MinW0) -> MinW = MinW0 ; MinW = 1200 ).

/* ---------- стартовые фронты ---------- */

seed_frontier_left_top(Wr,Hr, Wg,Hg, Frontier) :-
    Step is min(Wg,Hg),
    ( Step =< 0 -> Frontier = []
    ; Ymax is Hr - 1, Xmax is Wr - 1,
      findall((0,Y),
              ( Ymax >= 0, between(0, Ymax, Y), 0 is Y mod Step ),
              Left),
      findall((X,0),
              ( Xmax >= 0, between(0, Xmax, X), 0 is X mod Step ),
              Top),
      append(Left, Top, F0),
      sort(F0, Frontier)  % убрать дубликаты (0,0)
    ).

/* ---------- эвристики выбора ---------- */

orientation_choice(Wg,Hg, Wr, X, Ori) :-
    Rem is Wr - X,
    ( Rem < min(Wg,Hg) ->
        ( Hg =< Wg -> (Ori = 1 ; Ori = 0)
                    ; (Ori = 0 ; Ori = 1) )
    ;   ( Wg =< Hg -> (Ori = 0 ; Ori = 1)
                    ; (Ori = 1 ; Ori = 0) )
    ).

dims_by_ori(Wg,Hg, 0, Wg,Hg).
dims_by_ori(Wg,Hg, 1, Hg,Wg).

/* ---------- проверки ---------- */

fits_room(Wr,Hr, rect(X,Y,W,H)) :-
    X >= 0, Y >= 0, W > 0, H > 0,
    X + W =< Wr,
    Y + H =< Hr.

no_overlap_with_list(_Rect, []).
no_overlap_with_list(Rect, [R|Rs]) :-
    rects:rect_no_overlap(Rect, R),
    no_overlap_with_list(Rect, Rs).

/* ---------- работа с фронтом ---------- */

sort_frontier_yx(Frontier, Sorted) :-
    map_list_to_pairs(yx_key, Frontier, Pairs),
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, Sorted).
yx_key((X,Y), (Y,X)).

extend_frontier(rect(X,Y,W,H), FrontierIn, Wr,Hr, PlacedNow, Zones, FrontierOut) :-
    Xr is X + W,  Yr is Y,         P1 = (Xr,Yr),
    Xd is X,      Yd is Y + H,     P2 = (Xd,Yd),
    append([P1,P2], FrontierIn, Front0),
    filter_frontier(Wr,Hr, PlacedNow, Zones, Front0, Front1),
    nondominated(Front1, Front2),
    sort(Front2, FrontierOut).

filter_frontier(Wr,Hr, Placed, Zones, In, Out) :-
    include(point_inside_room(Wr,Hr), In, T0),
    exclude(point_in_any_rect(Placed), T0, T1),
    exclude(point_in_any_rect(Zones),  T1, Out).

point_inside_room(Wr,Hr, (X,Y)) :- X >= 0, Y >= 0, X < Wr, Y < Hr.

point_in_any_rect(Rects, (X,Y)) :-
    member(Rect, Rects),
    point_in_rect(X,Y, Rect), !.

point_in_rect(X,Y, rect(RX,RY,W,H)) :-
    X >= RX, X < RX + W,
    Y >= RY, Y < RY + H.

nondominated(Points, ND) :-
    exclude(is_dominated_by(Points), Points, ND).

is_dominated_by(Points, P) :-
    member(Q, Points),
    dominates(Q, P),
    Q \= P.

dominates((X1,Y1), (X2,Y2)) :-
    X1 =< X2, Y1 =< Y2,
    (X1 < X2 ; Y1 < Y2).

/* ---------- утилиты масштаба и кратности ---------- */

must_be_multiple(Value, Step) :- 0 is Value mod Step.

scale_rect_down(Grid, rect(X,Y,W,H), rect(Xg,Yg,Wg,Hg)) :-
    Xg is X div Grid, Yg is Y div Grid, Wg is W div Grid, Hg is H div Grid.
scale_rect_up(Grid, rect(Xg,Yg,Wg,Hg), rect(X,Y,W,H)) :-
    X is Xg*Grid, Y is Yg*Grid, W is Wg*Grid, H is Hg*Grid.

/* ---------- выбор комнаты (детерминированно, крупнейшая) ---------- */

room_rect_mm_max(rect(0,0,W,H)) :-
    findall(Wi-Hi, rects:room_size(Wi,Hi), Pairs),
    Pairs \= [],
    max_by_area(Pairs, W-H).

max_by_area([W-H], W-H) :- !.
max_by_area([W1-H1,W2-H2|T], Max) :-
    A1 is W1*H1, A2 is W2*H2,
    ( A2 > A1 -> max_by_area([W2-H2|T], Max)
    ;               max_by_area([W1-H1|T], Max)
    ).

/* ---------- опции зон ---------- */

zones_for_layout(Opts, Grid, ZonesG) :-
    ( member(no_zones, Opts) ->
        ZonesG = []
    ;   once(zones:collect_zones(ZonesMM)),
        maplist(scale_rect_down(Grid), ZonesMM, ZonesG)
    ).

/* ======================================================================
   БЕНЧМАРКИ (устойчивые; если нет решения — печатаем NO SOLUTION)
====================================================================== */

with_default_room(W,H, Goal) :-
    ( clause(rects:room_size(_,_), true) ->
        Goal
    ;   setup_call_cleanup(
            asserta(rects:room_size(W,H)),
            Goal,
            retract(rects:room_size(W,H))
        )
    ).

run_benchmark(N) :-
    W = 1400, H = 700, Grid = 100,
    format('--- BENCHMARK for N = ~w desks ---~n', [N]),
    with_default_room(6000,4000,
      (   time( ( once(layout_desks(N, W, H, Grid, _Rects)) -> Res = ok ; Res = no_solution ) ),
          ( Res == ok -> writeln('result: OK') ; writeln('result: NO SOLUTION') )
      )
    ),
    format('--- END BENCHMARK for N = ~w ---~n~n', [N]).

run_all_benchmarks :-
    writeln('Starting benchmark suite...'),
    Ns = [5, 10, 15, 20, 25, 30, 40, 50],
    maplist(run_benchmark, Ns),
    writeln('Benchmark suite finished.').

/* ======================================================================
   ТЕСТЫ
   - «пустая» сцена: выключаем зоны через опцию [no_zones]
   - валидность: внутри комнаты и без пересечений
   - доп. тесты на выбор комнаты и стартовый фронт
====================================================================== */
:- begin_tests(layout_frontier_tests, [setup(load_layout_facts), cleanup(unload_layout_facts)]).
:- use_module(layout).
:- use_module(rects).

load_layout_facts :-
    % эти два факта нужны ТОЛЬКО для тестов
    asserta(rects:room_size(100, 80)),
    asserta(rects:room_size(6000, 4000)).

unload_layout_facts :-
    retractall(rects:room_size(100, 80)),
    retractall(rects:room_size(6000, 4000)).

test('room_rect_mm_max picks largest deterministically') :-
    room_rect_mm_max(rect(0,0,W,H)),
    W == 6000, H == 4000.

test('seed frontier non-empty & has (0,0) on 60x40 grid with step 7') :-
    seed_frontier_left_top(60,40, 14,7, F),
    F \= [],
    memberchk((0,0), F).  % убрали choicepoint

test('empty/no_zones: 5 desks fit quickly & valid') :-
    once((
        layout_desks_opts(5, 1400,700, 100, [no_zones], Rects, _),
        rects:all_inside_room(rect(0,0,6000,4000), Rects),
        rects:all_no_overlap(Rects)
    )).

test('empty/no_zones: 10 desks fit') :-
    once((
        layout_desks_opts(10, 1400,700, 100, [no_zones], Rects, _),
        rects:all_inside_room(rect(0,0,6000,4000), Rects),
        rects:all_no_overlap(Rects)
    )).

test('empty/no_zones: 20 desks fit') :-
    once((
        layout_desks_opts(20, 1400,700, 100, [no_zones], Rects, _),
        rects:all_inside_room(rect(0,0,6000,4000), Rects),
        rects:all_no_overlap(Rects)
    )).

:- end_tests(layout_frontier_tests).

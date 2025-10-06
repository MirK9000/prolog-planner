:- module(paths,
  [ check_evac_all/6    % +WrMM,+HrMM,+Grid,+Doors,+MinAisleMM,+DesksMM
  ]).

:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(ordsets)).


emit_progress(E) :- ignore(catch(layout:progress_hook(E), _, true)).


% ------------------------------------------------------------------------------
% check_evac_all/6
%   Построить карту достижимости от дверей на "эродированном" свободном
%   пространстве (мин. ширина прохода MinAisleMM), и убедиться, что у КАЖДОГО
%   стола есть хотя бы одна стартовая клетка на периметре, достижимая от двери.
% ------------------------------------------------------------------------------
check_evac_all(WrMM,HrMM, Grid, Doors, MinAisleMM, DesksMM) :-
    Wr is WrMM // Grid, Hr is HrMM // Grid,
    Wr >= 1, Hr >= 1,
    % радиус эрозии (в клетках) для минимальной ширины
    MinCells is (MinAisleMM + Grid - 1) // Grid,
    R is max(0, (MinCells-1)//2), % для 12 → R=6 → порог 13 клеток = 1300 мм (чуть строже)

    emit_progress(evac(start, _{min_cells:MinCells, r:R,
                                doors:Doors, desks:DesksMM})),

    % 0) ЦЕЛИ (клетки дверей на границе)
    door_goal_cells(Wr,Hr, Grid, Doors, GoalRaw),

    % 1) ЖЁСТКИЕ препятствия: столы и структурные зоны (с дилатацией)
    desks_to_cells(Grid, DesksMM, DeskCells),
    struct_obstacles_mm(StructRects),
    rects_to_cells(Grid, StructRects, StructCells),
    dilate_cells(Wr,Hr, R, DeskCells,   DeskDil),
    dilate_cells(Wr,Hr, R, StructCells, StructDil),
    ord_union(DeskDil, StructDil, HardObs),

    % 2) МЯГКИЕ препятствия: буфер у стен минус коридоры в области дверей
    wall_buffer_cells(Wr,Hr, R, WallBuf),
    door_corridor_cells(Wr,Hr, Grid, Doors, R, Corr),
    ord_subtract(WallBuf, Corr, SoftBuf),

    % 3) Совокупные препятствия
    ord_union(HardObs, SoftBuf, Obstacles),

    % 4) Свободные клетки
    all_cells(Wr,Hr, All),
    ord_subtract(All, Obstacles, Free0),

    % 5) Источники BFS — только те, что реально свободны
    ord_intersection(Free0, GoalRaw, Goals),
    Goals \= [],

    % 6) BFS и проверка периметров столов
    bfs_multisource(Wr,Hr, Free0, Goals, DistAssoc),
    (   maplist(desk_has_reachable_start(Wr,Hr,R, Grid, Free0, DistAssoc), DesksMM)
    ->  emit_progress(evac(done, ok))
    ;   emit_progress(evac(done, fail)),
        fail
    ).


% ---------- сбор структурных прямоугольников из зон (колонны/тумбы и т.п.) ----------
% Используем forbidden_zone/3 как «структурные препятствия для ходьбы».
% Если понадобится фильтровать по типам — добавь проверку Type ∈ {column,struct,...}
struct_obstacles_mm(Rects) :-
    findall(R,
      ( zones:forbidden_zone(_Id,Type,R),
        blocks_walk_type(Type)
      ),
      Rects).

blocks_walk_type(struct).
blocks_walk_type(column).
blocks_walk_type(cabinet).
blocks_walk_type(obstacle).

% ---------- перевод прямоугольников в клетки ----------
rects_to_cells(_Grid, [], []).
rects_to_cells(Grid, [R|Rs], Cells) :-
    rect_to_cells(Grid, R, C1),
    rects_to_cells(Grid, Rs, C2),
    ord_union(C1, C2, Cells).

desks_to_cells(Grid, DesksMM, Cells) :- rects_to_cells(Grid, DesksMM, Cells).

rect_to_cells(Grid, rect(Xmm,Ymm,Wmm,Hmm), CellsOrd) :-
    X0 is Xmm // Grid,        Y0 is Ymm // Grid,
    X1 is (Xmm+Wmm-1) // Grid, Y1 is (Ymm+Hmm-1) // Grid, % включительно
    findall((X,Y),
            ( between(X0, X1, X),
              between(Y0, Y1, Y)
            ),
            Cells),
    sort(Cells, CellsOrd).

% ---------- дилатация препятствий квадратом Чебышёва на радиус R ----------
dilate_cells(Wr,Hr, R, Cells, Dil) :-
    ( R =< 0 -> sort(Cells, Dil)
    ; findall((X2,Y2),
        ( member((X,Y), Cells),
          DX is -R,  RX is R,  DY is -R, RY is R,
          between(DX, RX, OffX),
          between(DY, RY, OffY),
          X2 is X+OffX, Y2 is Y+OffY,
          X2 >= 0, X2 < Wr,
          Y2 >= 0, Y2 < Hr
        ),
        Raw),
      sort(Raw, Dil)
    ).


% ---------- полоса-буфер у стен шириной R ----------
wall_buffer_cells(Wr,Hr, R, Buf) :-
    ( R =< 0 -> Buf = []
    ; Xmax is Wr - 1,
      Ymax is Hr - 1,
      XR   is Wr - R,
      YR   is Hr - R,
      findall((X,Y),
        ( between(0, Xmax, X),
          between(0, Ymax, Y),
          ( X < R ; Y < R ; X >= XR ; Y >= YR )
        ),
        Raw),
      sort(Raw, Buf)
    ).



% ---------- все клетки комнаты ----------
all_cells(Wr,Hr, AllOrd) :-
    Xmax is Wr - 1,
    Ymax is Hr - 1,
    findall((X,Y), (between(0,Xmax,X), between(0,Ymax,Y)), All),
    sort(All, AllOrd).

% ---------- клетки дверей (на границе комнаты) ----------
door_goal_cells(Wr,Hr, Grid, Doors, GoalsOrd) :-
    Xmax is Wr - 1,
    Ymax is Hr - 1,
    findall((X,Y),
      ( member(door(_Id,side(S),offset(OffMM),width(Wmm)), Doors),
        Off is OffMM // Grid,
        Wc  is max(1, Wmm // Grid),
        ( S == north ->
            Y is 0,
            X0 is Off,
            T  is Off + Wc - 1,
            X1 is min(Xmax, T),
            between(X0,X1,X)
        ; S == south ->
            Y is Ymax,
            X0 is Off,
            T  is Off + Wc - 1,
            X1 is min(Xmax, T),
            between(X0,X1,X)
        ; S == west  ->
            X is 0,
            Y0 is Off,
            T  is Off + Wc - 1,
            Y1 is min(Ymax, T),
            between(Y0,Y1,Y)
        ; S == east  ->
            X is Xmax,
            Y0 is Off,
            T  is Off + Wc - 1,
            Y1 is min(Ymax, T),
            between(Y0,Y1,Y)
        )
      ),
      Raw),
    sort(Raw, GoalsOrd).

% Клетки коридоров: на ширину R внутрь от дверей, по ширине проёма
door_corridor_cells(Wr,Hr, Grid, Doors, R, CorrOrd) :-
    Xmax is Wr - 1,
    Ymax is Hr - 1,
    findall((X,Y),
      ( member(door(_Id,side(S),offset(OffMM),width(Wmm)), Doors),
        Off is OffMM // Grid,
        Wc  is max(1, Wmm // Grid),
        ( S == west ->
            X0 is 0,           X1 is min(R, Xmax),
            Y0 is Off,         T  is Off + Wc - 1,
            Y1 is min(Ymax, T),
            between(X0,X1,X), between(Y0,Y1,Y)
        ; S == east ->
            X0 is max(0, Wr-1-R), X1 is Xmax,
            Y0 is Off,            T  is Off + Wc - 1,
            Y1 is min(Ymax, T),
            between(X0,X1,X), between(Y0,Y1,Y)
        ; S == north ->
            Y0 is 0,           Y1 is min(R, Ymax),
            X0 is Off,         T  is Off + Wc - 1,
            X1 is min(Xmax, T),
            between(X0,X1,X), between(Y0,Y1,Y)
        ; S == south ->
            Y0 is max(0, Hr-1-R), Y1 is Ymax,
            X0 is Off,            T  is Off + Wc - 1,
            X1 is min(Xmax, T),
            between(X0,X1,X), between(Y0,Y1,Y)
        )
      ),
      Raw),
    sort(Raw, CorrOrd).


% ---------- BFS по 4-соседям ----------
bfs_multisource(_Wr,_Hr, Free, Goals, DistAssoc) :-
    empty_assoc(Empty),
    seed_goals(Goals, 0, Empty, Dist0, Queue0),
    bfs_loop(Free, Queue0, Dist0, DistAssoc).

seed_goals([], _, Dist, Dist, []).
seed_goals([G|Gs], D, DistIn, DistOut, [G|Q]) :-
    put_assoc(G, DistIn, D, Dist1),
    seed_goals(Gs, D, Dist1, DistOut, Q).

bfs_loop(_Free, [], Dist, Dist).
bfs_loop(Free, [C|Qs], DistIn, DistOut) :-
    get_assoc(C, DistIn, D), !,
    neighbors4(C, Ns),
    include({Free}/[N]>>ord_memberchk(N, Free), Ns, NsFree),
    findall(N,
      ( member(N, NsFree),
        \+ get_assoc(N, DistIn, _)
      ),
      ToAdd),
    D1 is D+1,
    queue_add(ToAdd, Qs, Qs1),
    assoc_bulk_put(ToAdd, D1, DistIn, Dist1),
    bfs_loop(Free, Qs1, Dist1, DistOut).
bfs_loop(Free, [_|Qs], DistIn, DistOut) :-
    bfs_loop(Free, Qs, DistIn, DistOut).



neighbors4((X,Y), [(X1,Y),(X2,Y),(X,Y1),(X,Y2)]) :-
    X1 is X-1, X2 is X+1, Y1 is Y-1, Y2 is Y+1.

queue_add([], Q, Q).
queue_add([A|As], Q, Qout) :- append(Q, [A|As], Qout).

assoc_bulk_put([], _, A, A).
assoc_bulk_put([K|Ks], D, A0, A) :-
    put_assoc(K, A0, D, A1),
    assoc_bulk_put(Ks, D, A1, A).

% ---------- стартовые клетки вокруг периметра стола ----------
desk_has_reachable_start(Wr,Hr,R, Grid, Free, DistAssoc, RectMM) :-
    % 1) клетки стола
    rect_to_cells(Grid, RectMM, DeskCells),
    % 2) дилатация стола на радиус R (зона «запрета» вокруг стола)
    dilate_cells(Wr,Hr, R, DeskCells, Dil),
    % 3) внешнее кольцо вокруг дилатации — 4-соседями
    perimeter_ring(Dil, Ring),
    % 4) найдётся точка кольца, которая свободна и достижима от дверей
    member(C, Ring),
    ord_memberchk(C, Free),
    get_assoc(C, DistAssoc, _),  % факт достижимости (значение неважно)
    !.

perimeter_ring(Cells, RingOrd) :-
    findall((X2,Y2),
      ( member((X,Y), Cells),
        member((Dx,Dy), [(-1,0),(1,0),(0,-1),(0,1)]),
        X2 is X+Dx, Y2 is Y+Dy
      ),
      Raw),
    sort(Raw, RingOrd).

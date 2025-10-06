%%% ================================================================
%%% FILE: orient_spiral.pl
%%% Purpose: Stage 2 for layout_spiral — orientation solver (graph-of-sides)
%%% Exports: orient_tiles/5
%%% ================================================================
:- module(orient_spiral,
  [ orient_tiles/5    % +RectsMM,+Meta,+Doors,-Oris,-Proof
  ]).

:- use_module(rects).
:- use_module(tiles).
:- use_module(connectivity).
:- use_module(library(ordsets)).
:- use_module(library(assoc)).
:- use_module(library(yall)).
:- use_module(library(lists)). 


%% public -----------------------------------------------------------

% RectsMM: [rect(X,Y,S,S)|...], from layout_spiral
% Meta:    _{grid_mm:S, safe:_{x0:X0,y0:Y0}, cells:_{x:Cx,y:Cy}, room:_{w:Wr,h:Hr}}
% Doors:   [door(Id,side(S),offset(Off),width(Wd))|...]
orient_tiles(Rects, Meta, Doors, Oris, Proof) :-
    meta_unpack(Meta, S, X0,Y0, Cx,Cy, Wr,Hr),

    % id↔cell maps
    index_rect_cells(S, X0,Y0, Rects, IdCells, Cell2Id, Id2Cell),

    % двери в SAFE-индексах 
    tiles:door_goal_edges(Wr,Hr, S, Doors, DoorEdgesFull),
    safe_edge_indices(X0,Y0,S, Cx,Cy, DoorEdgesFull, DoorEdges),

    % периметр и его ориентация
    classify_perimeter(IdCells, PerimPairs, InnerPairs),
    orient_perimeter(PerimPairs, Id2Cell, Cx,Cy, DoorEdges, AssignedP, AssocP),

    % id внутренних
    maplist(arg(1), InnerPairs, InnerIds),

       % соседства один раз
    neighbor_assoc(Id2Cell, Cell2Id, NeighA),

    grid_bounds(Id2Cell, Bounds),

    % 1) сиды от периметра по компонентам
    seed_connectivity_components(AssignedP, Id2Cell, NeighA, ConnSeeds0),

    % 2) door-shadow сиды + принудительные ориентации «к двери»
    door_shadow_seeds(DoorEdges, Cell2Id, Id2Cell, DoorSeedIds, DoorForcePairs),

    % 3) объединяем сиды
    ord_union(ConnSeeds0, DoorSeedIds, ConnSeeds1),

    % 4) применяем форс-ориентации и убираем их из списка необработанных
    foldl([Id-O,A0,A1]>>put_assoc(Id, A0, O, A1), DoorForcePairs, AssocP, Assoc1),
    subtract(InnerIds, DoorSeedIds, InnerIds1),

    % 5) (опционально) проставим ориентации одиночным сиду-узлам deg=0
    preassign_isolated_seeds(ConnSeeds1, InnerIds1, NeighA, Assoc1, InnerIds2, Assoc2),

    % 6) волна с фоллбэком на BFS (не фэйлит на «островах»)
    (   solve_fill(state(InnerIds2, Assoc2, ConnSeeds1, NeighA, Id2Cell, Bounds), FinalA0)
    ->  FinalA = FinalA0
    ;   bfs_orient_all(Id2Cell, NeighA, ConnSeeds1, Bounds, AssocB),
        FinalA = _{ assoc: AssocB, conn: ConnSeeds1 }
    ),



    assoc_to_list(FinalA.assoc, _Pairs),       % уже не нужен порядок из ассока
    length(Rects, N),
    numlist(1, N, Ids),
    findall(Ori,
            ( member(I, Ids),
              ( get_assoc(I, FinalA.assoc, O) -> Ori = O ; Ori = 0 )
            ),
            Oris),

    connectivity:validate_pass_connectivity(Rects, Oris, Meta, Doors, PassDiag),
    ( PassDiag = success -> true
    ; format(user_error, '[orient] PASS connectivity failed: ~w~n', [PassDiag]),
      fail
    ),

    Proof = _{ perimeter:AssignedP,
              door_edges:DoorEdges,
              conn_seeds:ConnSeeds1,
              final:FinalA }.


%% meta helpers -----------------------------------------------------

meta_unpack(Meta, S, X0,Y0, Cx,Cy, Wr,Hr) :-
    S  = Meta.grid_mm,
    X0 = Meta.safe.x0,  Y0 = Meta.safe.y0,
    Cx = Meta.cells.x,  Cy = Meta.cells.y,
    Wr = Meta.room.w,   Hr = Meta.room.h.

index_rect_cells(S, X0,Y0, Rects, IdCells, Cell2Id, Id2Cell) :-
    findall(Id-(Xg-Yg),
      ( nth1(Id, Rects, rect(X,Y,W,H)),
        W =:= S, H =:= S,
        Xg is (X - X0) // S,
        Yg is (Y - Y0) // S
      ),
      Pairs),
    % Build Id→Cell and Cell→Id both
    findall(Cell-Id, member(Id-Cell, Pairs), CPairs),
    list_to_assoc(CPairs, Cell2Id),
    list_to_assoc(Pairs, Id2Cell),
    pairs_keys(Pairs, Ids),
    maplist({Id2Cell}/[I,I-Cell]>>get_assoc(I, Id2Cell, Cell),
            Ids, IdCells).

safe_edge_indices(X0,Y0,S, Cx,Cy, EdgesFull, EdgesSafe) :-
    Kx is X0 // S, Ky is Y0 // S,
    Xmax is Cx - 1, Ymax is Cy - 1,
    findall(edge(Xs,Ys,Dir),
      ( member(edge(Xf,Yf,Dir), EdgesFull),
        Xs is Xf - Kx, Ys is Yf - Ky,
        Xs >= 0, Xs =< Xmax, Ys >= 0, Ys =< Ymax
      ),
      Raw),
    sort(Raw, EdgesSafe).

classify_perimeter(IdCells, Perim, Inner) :-
    % IdCells: [Id-(X-Y)|...]
    findall(X, member(_-(X-_), IdCells), Xs),
    findall(Y, member(_-(_-Y), IdCells), Ys),
    min_list(Xs, Xmin), max_list(Xs, Xmax),
    min_list(Ys, Ymin), max_list(Ys, Ymax),
    partition(
      {Xmin,Xmax,Ymin,Ymax}/[_Id-(X-Y)]>>(
         X =:= Xmin ; X =:= Xmax ; Y =:= Ymin ; Y =:= Ymax
      ),
      IdCells, Perim, Inner).

neighbor_assoc(Id2Cell, Cell2Id, NeighA) :-
    empty_assoc(NeighA0),
    assoc_to_list(Id2Cell, IdCells),
    foldl(build_neigh(Cell2Id), IdCells, NeighA0, NeighA).

build_neigh(Cell2Id, Id-(X-Y), A0, A) :-
    findall(Dir-IdN,
      ( neighbor4(X,Y, Dir, X2,Y2),
        get_assoc((X2-Y2), Cell2Id, IdN)
      ),
      Neighs),
    list_to_assoc(Neighs, NA),
    put_assoc(Id, A0, NA, A).

neighbor4(X,Y, north, X, Y-1).
neighbor4(X,Y, south, X, Y+1).
neighbor4(X,Y, west,  X-1, Y).
neighbor4(X,Y, east,  X+1, Y).

% унификация коротких и длинных имён направлений
dir_norm(n, north).
dir_norm(s, south).
dir_norm(e, east).
dir_norm(w, west).
dir_norm(north, north).
dir_norm(south, south).
dir_norm(east,  east).
dir_norm(west,  west).

dir_short(n, n).    dir_short(s, s).    dir_short(e, e).    dir_short(w, w).
dir_short(north, n). dir_short(south, s). dir_short(east, e). dir_short(west, w).

pass_on_dir(Ori, Dir) :-
    dir_short(Dir, Ds),
    tiles:tile_main_dir(Ori, Ds).


% ori_for_dir принимает оба варианта
ori_for_dir(Dir, Ori) :-
    dir_norm(Dir, Dn),
    ori_for_dir_norm(Dn, Ori).

ori_for_dir_norm(north, 0).
ori_for_dir_norm(west,  1).
ori_for_dir_norm(south, 2).
ori_for_dir_norm(east,  3).

% оппоненты для обоих вариантов
dir_op(north, south).
dir_op(south, north).
dir_op(west,  east).
dir_op(east,  west).


%% stage 1: perimeter ----------------------------------------------

orient_perimeter(PerimPairs, _Id2Cell, Cx,Cy, DoorEdges, AssignedList, AssocOut) :-
    Xmin = 0,            Xmax is Cx - 1,
    Ymin = 0,            Ymax is Cy - 1,
    findall(Id-Ori,
      ( member(Id-Cell, PerimPairs),
        Cell = (X-Y),
        door_dirs_for_cell_on_boundary(Cell, DoorEdges, Xmin,Xmax,Ymin,Ymax, Dirs),
        choose_perimeter_ori(X,Y, Xmin,Xmax,Ymin,Ymax, Dirs, Ori)
      ),
      AssignPairs),
    list_to_assoc(AssignPairs, AssocOut),
    AssignedList = AssignPairs.



door_dirs_for_cell(X-Y, DoorEdges, Dirs) :-
    findall(Dir, member(edge(X,Y,Dir), DoorEdges), Ds),
    sort(Ds, Dirs).

% СТАЛО: учитываем двери только если (X,Y) на границе, соответствующей Dir
door_dirs_for_cell_on_boundary(X-Y, DoorEdges, Xmin,Xmax,Ymin,Ymax, Dirs) :-
    findall(Dn,
      ( member(edge(X,Y,Dir0), DoorEdges),
        dir_norm(Dir0, Dn),
        boundary_match(X,Y, Dn, Xmin,Xmax,Ymin,Ymax)
      ),
      Ds),
    sort(Ds, Dirs).

% ВНИМАНИЕ: теперь без инверсии
boundary_match(_X, Y, north, _Xmin,_Xmax, Ymin,_Ymax) :- Y =:= Ymin.  % верхний ряд
boundary_match(_X, Y, south, _Xmin,_Xmax,_Ymin, Ymax) :- Y =:= Ymax.  % нижний ряд
boundary_match( X,_Y, west,   Xmin,_Xmax,_Ymin,_Ymax) :- X =:= Xmin.  % левый столбец
boundary_match( X,_Y, east,  _Xmin, Xmax,_Ymin,_Ymax) :- X =:= Xmax.  % правый столбец

choose_perimeter_ori(X, Y, Xmin, Xmax, Ymin, Ymax, DoorDirs, Ori) :-
    ( DoorDirs = [Dir|_] -> ori_for_dir(Dir, Ori)      % приоритет двери
    ; Y =:= Ymin        -> ori_for_dir(south, Ori)     % верхний ряд → проход вниз
    ; Y =:= Ymax        -> ori_for_dir(north, Ori)     % нижний ряд → вверх
    ; X =:= Xmin        -> ori_for_dir(east,  Ori)     % левый столбец → вправо
    ; X =:= Xmax        -> ori_for_dir(west,  Ori)     % правый столбец → влево
    ).

%% connectivity seed & closure over assigned -----------------------

connected_from_doors(AssignedPairs, Id2Cell, DoorEdges, Conn) :-
    findall(Id,
      ( member(Id-Ori, AssignedPairs),
        get_assoc(Id, Id2Cell, X-Y),
        member(edge(X,Y,Dir), DoorEdges),
        pass_on_dir(Ori, Dir)
      ),
      Seeds0),
    sort(Seeds0, Seeds),
    list_to_ord_set(Seeds, Conn0),
    Conn = Conn0.

close_connected_over_assigned(AssignedPairs, Id2Cell, ConnIn, ConnOut) :-
    % BFS/closure along assigned tiles using handshakes
    list_to_assoc(AssignedPairs, Assoc),
    ( ord_empty(ConnIn) ->
        ConnOut = ConnIn
    ;   closure_step(Assoc, Id2Cell, ConnIn, ConnOut)
    ).

closure_step(Assoc, Id2Cell, ConnIn, ConnOut) :-
    ( add_one(Assoc, Id2Cell, ConnIn, IdNew)
      -> ord_add_element(ConnIn, IdNew, Conn1),
         closure_step(Assoc, Id2Cell, Conn1, ConnOut)
      ;  ConnOut = ConnIn
    ).

add_one(Assoc, Id2Cell, Conn, IdNew) :-
    assoc_to_list(Assoc, Assigns),
    member(Id-Ori, Assigns),
    \+ ord_memberchk(Id, Conn),
    get_assoc(Id, Id2Cell, X-Y),
    member(IdC, Conn),
    get_assoc(IdC, Id2Cell, Xc-Yc),
    neighbor_dir(X-Y, Xc-Yc, DirToC),
    get_assoc(IdC, Assoc, OriC),
    dir_op(DirToC, Back),
    (   pass_on_dir(Ori,  DirToC)         % достаточно одной стороны
    ;   pass_on_dir(OriC, Back)
    ),
    IdNew = Id, !.


neighbor_dir(X-Y, X2-Y2, Dir) :-
    ( X2 =:= X,   Y2 =:= Y-1 -> Dir = north
    ; X2 =:= X,   Y2 =:= Y+1 -> Dir = south
    ; X2 =:= X-1, Y2 =:= Y   -> Dir = west
    ; X2 =:= X+1, Y2 =:= Y   -> Dir = east
    ).





%% stage 2: fill ----------------------------------------------------

% State.dict:
%   unassigned : [Id|...]
%   assoc      : Assoc(Id->Ori) for assigned
%   conn       : ordset of Id connected to a door
%   neigh      : Assoc(Id->Assoc(Dir->IdNeighbor))
%   id2cell    : Assoc(Id->(X-Y))
%   bounds     : bounds(Xmin,Xmax,Ymin,Ymax)
solve_fill(state([], Assoc, Conn, _Neigh,_Id2Cell,_Bounds),
           _{assoc:Assoc, conn:Conn}).

% Дет. волна: на каждом шаге ориентируем всех фронтовиков сразу.
solve_fill(state(Unassigned, Assoc, Conn, Neigh, Id2Cell, Bounds), Final) :-
    % собрать фронтир
    include(has_conn_neighbor(Neigh, Conn), Unassigned, Frontier),
    ( Frontier == [] ->
        % либо всё готово, либо граф реально несвязен
        ( Unassigned == [] ->
            Final = _{assoc:Assoc, conn:Conn}
        ;   % тупик: остались острова
            fail
        )
    ;   % рассчитать ориентацию для каждого Id из фронтира
        findall(Id-Ori,
          ( member(Id, Frontier),
            pick_ori(Id, Conn, Neigh, Id2Cell, Bounds, Ori)
          ),
          AssignsFront),
        % применить их разом
        foldl(
          [I-O, st(U,A,C,N,I2,B), st(U1,A1,C1,N,I2,B)]>>
            ( select(I, U, U1),
              put_assoc(I, A, O, A1),
              ord_add_element(C, I, C1)
            ),
          AssignsFront,
          st(Unassigned, Assoc, Conn, Neigh, Id2Cell, Bounds),
          st(Un1, Assoc1, Conn1, Neigh, Id2Cell, Bounds)
        ),
        % следующий шаг волны
        solve_fill(state(Un1, Assoc1, Conn1, Neigh, Id2Cell, Bounds), Final)
    ).


% Выбрать ориентацию "в сторону" любого соседа из Conn (стабильный приоритет)
pick_ori_towards_conn(Id, Conn, Neigh, Ori) :-
    get_assoc(Id, Neigh, NAssoc),
    assoc_to_list(NAssoc, Pairs0),                         % [Dir-IdN|...]
    include([Dir-IdN]>>ord_memberchk(IdN, Conn), Pairs0, Toward0),
    Toward0 \= [],
    prefer_dir([north, east, south, west], Toward0, Dir), % стабильный порядок
    ori_for_dir(Dir, Ori).

prefer_dir([D|_], Pairs, D) :- member(D-_, Pairs), !.
prefer_dir([_|Ds], Pairs, D) :- prefer_dir(Ds, Pairs, D).

has_conn_neighbor(Neigh, Conn, Id) :-
    get_assoc(Id, Neigh, NAssoc),
    assoc_to_list(NAssoc, Pairs),
    member(_-J, Pairs),
    ord_memberchk(J, Conn), !.


% forward-check for an unassigned neighbor J of Id along direction DirFromIdToJ:



% Разбиваем множество всех Id на компоненты по 4-соседям
seed_connectivity_components(AssignedP, Id2Cell, NeighA, ConnOut) :-
    % множество всех Id
    assoc_to_list(Id2Cell, Pairs),        % [Id-(X-Y)|...]
    findall(Id, member(Id-_, Pairs), AllIds0),
    list_to_ord_set(AllIds0, AllIds),

    % множество периметровых Id (из AssignedP)
    maplist(arg(1), AssignedP, PerimIds0),
    list_to_ord_set(PerimIds0, PerimIds),

    % компоненты
    components(AllIds, NeighA, Comps),

    % сиды: для каждой компоненты берём её пересечение с Perim;
    % если пусто — берём минимальный Id компоненты
    findall(SeedSet,
      ( member(Comp, Comps),
        ord_intersection(Comp, PerimIds, OnPerim),
        ( OnPerim \= [] -> SeedSet = OnPerim
        ; Comp = [Min|_], SeedSet = [Min] )
      ),
      SeedSets),
    append(SeedSets, SeedsFlat),
    list_to_ord_set(SeedsFlat, ConnOut).

% Поиск компонент связности (BFS/DFS по NeighA)
components(AllIds, NeighA, Comps) :-
    components_loop(AllIds, NeighA, [], Comps).

components_loop([], _NeighA, Acc, Comps) :- reverse(Acc, Comps).
components_loop([Id|Rest], NeighA, Acc, Comps) :-
    % обходим компоненту из Id
    bfs_component([Id], NeighA, [], CompSet),
    % убираем посещённые из Rest
    ord_subtract(Rest, CompSet, Rest1),
    components_loop(Rest1, NeighA, [CompSet|Acc], Comps).

bfs_component([], _NeighA, Vis, Vis).
bfs_component([I|Q], NeighA, Vis0, Vis) :-
    ( ord_memberchk(I, Vis0) ->
        bfs_component(Q, NeighA, Vis0, Vis)
    ;   get_assoc(I, NeighA, NAssoc),
        assoc_to_list(NAssoc, Pairs),          % [Dir-IdN|...]
        findall(N, member(_-N, Pairs), Ns0),
        ord_add_element(Vis0, I, Vis1),
        % в очередь добавляем только ещё не посещённых
        exclude({Vis1}/[N]>>ord_memberchk(N, Vis1), Ns0, Ns),
        append(Q, Ns, Q1),
        bfs_component(Q1, NeighA, Vis1, Vis)
    ).


% Присваиваем ориентацию сид-узлам без соседей (deg=0),
% чтобы они не валили волну (Frontier==[])
preassign_isolated_seeds(ConnSeeds, InnerIds, NeighA, AssocIn, InnerOut, AssocOut) :-
    % найдём внутренние узлы, которые одновременно в сид-множестве и без соседей
    include(
      {ConnSeeds,NeighA}/[Id]>>(
          ord_memberchk(Id, ConnSeeds),
          get_assoc(Id, NeighA, NAssoc),
          assoc_to_list(NAssoc, L),
          L == []
      ),
      InnerIds,
      Isolated
    ),
    ( Isolated == [] ->
        InnerOut = InnerIds,
        AssocOut = AssocIn
    ;   % ориентируем их произвольно (north=0) и исключаем из Unassigned
        foldl([I,A0,A1]>>put_assoc(I, A0, 0, A1), Isolated, AssocIn, AssocOut),
        subtract(InnerIds, Isolated, InnerOut)
    ).


door_shadow_seeds(DoorEdges, Cell2Id, Id2Cell, SeedIds, ForcePairs) :-
    % Соберём размеры сетки из уже имеющегося Id2Cell
    assoc_to_list(Id2Cell, L), findall(X, member(_-(X-_), L), Xs),
    findall(Y, member(_-(_-Y), L), Ys),
    max_list(Xs, Xmax), max_list(Ys, Ymax),

    findall(Id-Ori,
      ( member(edge(X,Y,Dir), DoorEdges),
        door_shadow_pick(X,Y,Dir, Xmax,Ymax, Cell2Id, Id),
        door_force_ori(Dir, Ori)
      ),
      Pairs0),
    sort(Pairs0, Pairs),                     % убираем дубликаты
    pairs_keys(Pairs, SeedIds),
    ForcePairs = Pairs.

door_force_ori(north, 0).
door_force_ori(south, 2).
door_force_ori(west,  1).
door_force_ori(east,  3).

% Ищем ближайший тайл вглубь помещения по нормали к двери.
door_shadow_pick(X,_Y, Dir0, _Xmax, Ymax, Cell2Id, Id) :-
    dir_norm(Dir0, north),
    between(0, Ymax, Y),                          % сверху вниз
    get_assoc((X-Y), Cell2Id, Id), !.

door_shadow_pick(X,_Y, Dir0, _Xmax, Ymax, Cell2Id, Id) :-
    dir_norm(Dir0, south),
    between(0, Ymax, K), Y is Ymax - K,           % снизу вверх
    get_assoc((X-Y), Cell2Id, Id), !.

door_shadow_pick(_X,Y, Dir0, Xmax, _Ymax, Cell2Id, Id) :-
    dir_norm(Dir0, west),
    between(0, Xmax, X),                          % слева направо
    get_assoc((X-Y), Cell2Id, Id), !.

door_shadow_pick(_X,Y, Dir0, Xmax, _Ymax, Cell2Id, Id) :-
    dir_norm(Dir0, east),
    between(0, Xmax, K), X is Xmax - K,           % справа налево
    get_assoc((X-Y), Cell2Id, Id), !.


bfs_orient_all(Id2Cell, NeighA, Seeds, Bounds, AssocOut) :-
    % Dist: Assoc(Id -> Dist)
    bfs_build_dists(Seeds, NeighA, Dists0),
    % Дополнить сидами компоненты без дистанции (на всякий случай)
    assoc_to_list(Id2Cell, LIds),
    findall(Id, member(Id-_, LIds), AllIds),
    ensure_cover_all(AllIds, Dists0, NeighA, Dists),
    % Вычислить ориентации по градиенту расстояний
    findall(Id-Ori,
      ( member(Id, AllIds),
        pick_ori_by_dist(Id, Dists, NeighA, Id2Cell, Bounds, Ori)
      ),
      Pairs),
    list_to_assoc(Pairs, AssocOut).


ensure_cover_all(AllIds, D0, NeighA, D) :-
    % если какая-то компонента не покрыта сидом — взять её min Id как доп. сид
    findall(MinId,
      ( components_from_ids(AllIds, NeighA, Comps),
        member(Comp, Comps),
        \+ ( member(I, Comp), get_assoc(I, D0, _) ),
        Comp = [MinId|_]
      ),
      ExtraSeeds),
    ( ExtraSeeds == [] ->
        D = D0
    ; bfs_build_dists(ExtraSeeds, NeighA, Dextra),
      % объединяем (не затирая уже меньшие расстояния)
      assoc_to_list(Dextra, KL),
      foldl(keep_min_dist, KL, D0, D)
    ).

keep_min_dist(Id-Dn, A0, A) :-
    ( get_assoc(Id, A0, D0), D0 =< Dn ->
        A = A0
    ; put_assoc(Id, A0, Dn, A)
    ).

bfs_build_dists(SeedIds, NeighA, Dists) :-
    empty_assoc(D0),
    foldl([Id,A0,A]>>put_assoc(Id, A0, 0, A), SeedIds, D0, Dinit),
    bfs_queue(SeedIds, Dinit, NeighA, Dists).

bfs_queue([], D, _NeighA, D).
bfs_queue([I|Q], D0, NeighA, D) :-
    get_assoc(I, NeighA, NAssoc),
    assoc_to_list(NAssoc, Pairs),
    get_assoc(I, D0, Di),
    findall(N,
      ( member(_-N, Pairs),
        \+ get_assoc(N, D0, _)
      ),
      New),
    Dnext is Di + 1,
    foldl({Dnext}/[N,A0,A]>>put_assoc(N, A0, Dnext, A), New, D0, D1),
    append(Q, New, Q1),
    bfs_queue(Q1, D1, NeighA, D).

grid_bounds(Id2Cell, bounds(Xmin,Xmax,Ymin,Ymax)) :-
    assoc_to_list(Id2Cell, L),
    findall(X, member(_-(X-_), L), Xs),
    findall(Y, member(_-(_-Y), L), Ys),
    min_list(Xs, Xmin), max_list(Xs, Xmax),
    min_list(Ys, Ymin), max_list(Ys, Ymax).

% приоритет для «пустых» внутренних сторон — хотим сохранять коридоры
prefer_dir_atom([D|_], Options, D) :- member(D, Options), !.
prefer_dir_atom([_|Ds], Options, D) :- prefer_dir_atom(Ds, Options, D).

interior_void_dirs(Id, Neigh, Id2Cell, bounds(Xmin,Xmax,Ymin,Ymax), Dirs) :-
    get_assoc(Id, Id2Cell, X-Y),
    get_assoc(Id, Neigh, NAssoc),
    findall(D,
      ( member(D, [north,east,south,west]),
        \+ get_assoc(D, NAssoc, _),              % соседа-тайла нет
        neighbor4(X,Y, D, X2,Y2),
        X2>=Xmin, X2=<Xmax, Y2>=Ymin, Y2=<Ymax   % но это не внешняя стена — внутри рамки
      ),
      Dirs).

pick_ori(Id, Conn, Neigh, Id2Cell, Bounds, Ori) :-
    % 1) если рядом внутренняя «пустота», открываемся туда (бережём коридор)
    ( interior_void_dirs(Id, Neigh, Id2Cell, Bounds, IV), IV \= [] ->
        prefer_dir_atom([south,east,west,north], IV, Dir)   % чуть тянем вниз по макету
    ; % 2) иначе — как раньше, к любому соседу из Conn
      get_assoc(Id, Neigh, NAssoc),
      assoc_to_list(NAssoc, Pairs0),
      include([Dir-IdN]>>ord_memberchk(IdN, Conn), Pairs0, Toward0),
      prefer_dir([north, east, south, west], Toward0, Dir)
    ),
    ori_for_dir(Dir, Ori).

pick_ori_by_dist(Id, Dists, NeighA, Id2Cell, Bounds, Ori) :-
    % сначала пытаемся идти к соседу с меньшей дистанцией
    ( get_assoc(Id, Dists, Di),
      get_assoc(Id, NeighA, NAssoc),
      assoc_to_list(NAssoc, Pairs),
      include({Dists,Di}/[_Dir-N]>>(get_assoc(N, Dists, Dn), Dn < Di), Pairs, Downs),
      Downs \= [],
      prefer_dir([north,east,south,west], Downs, DirChosen),
      ori_for_dir(DirChosen, Ori)
    ; % если нет нисходящих соседей (локальный минимум/сид) — бережём коридор
      pick_ori(Id, _ConnEmpty, NeighA, Id2Cell, Bounds, Ori)
    ).

components_from_ids(AllIds, NeighA, Comps) :-
    % как твои components/… но вход — уже список Id
    list_to_ord_set(AllIds, Set),
    components(Set, NeighA, Comps).

% Безопасный выбор, не использующий Conn (для BFS-фоллбэка)
pick_ori_nodoor(Id, Neigh, Id2Cell, Bounds, Ori) :-
    (   interior_void_dirs(Id, Neigh, Id2Cell, Bounds, IV), IV \= []
    ->  prefer_dir_atom([south,east,west,north], IV, Dir)
    ;   get_assoc(Id, Neigh, NAssoc),
        assoc_to_list(NAssoc, Pairs0),
        (   prefer_dir([north,east,south,west], Pairs0, Dir)
        ->  true
        ;   Dir = south                       % вообще без соседей
        )
    ),
    ori_for_dir(Dir, Ori).

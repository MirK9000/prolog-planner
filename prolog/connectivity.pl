:- module(connectivity,
  [ filter_isolated_cells/5,
    isolated_cell/4,
    validate_grid_connectivity/4,
    build_grid_graph/5,
    bfs_reachable/3,
    validate_pass_connectivity/5,
    compute_pass_areas/4,
    build_pass_graph/3
  ]).

:- use_module(library(ordsets)).
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(apply)).

:- use_module(rects).
:- use_module(zones).
:- use_module(tiles).

/* ------------------------------------------------------------------
   Public helpers for Stage 1 filtering
   ------------------------------------------------------------------ */
filter_isolated_cells(BlockedSet, Cx, Cy, FreeCells0, FreeCells) :-
    exclude(isolated_cell(BlockedSet, Cx, Cy), FreeCells0, FreeCells).

isolated_cell(BlockedSet, Cx, Cy, (X,Y)) :-
    X1 is X - 1, X2 is X + 1,
    Y1 is Y - 1, Y2 is Y + 1,
    \+ accessible_neighbor(BlockedSet, Cx, Cy, X1, Y),
    \+ accessible_neighbor(BlockedSet, Cx, Cy, X2, Y),
    \+ accessible_neighbor(BlockedSet, Cx, Cy, X, Y1),
    \+ accessible_neighbor(BlockedSet, Cx, Cy, X, Y2).

accessible_neighbor(BlockedSet, Cx, Cy, X, Y) :-
    X >= 0, X < Cx,
    Y >= 0, Y < Cy,
    \+ ord_memberchk((X,Y), BlockedSet).

/* ------------------------------------------------------------------
   Grid connectivity validation (Stage 2)
   ------------------------------------------------------------------ */
validate_grid_connectivity(RectsMM, Meta, Doors, Diagnostic) :-
    meta_unpack(Meta, S, X0,Y0, Cx,Cy, Wr,Hr),

    maplist(rect_to_cell(S, X0, Y0), RectsMM, PlacedCells0),
    list_to_ord_set(PlacedCells0, PlacedSet),

    hard_obstacles_mm(ObsMM),
    rects_to_cells_grid(S, X0,Y0, Cx,Cy, ObsMM, BlockedSet),

    all_grid_cells(Cx, Cy, AllCells),
    include({BlockedSet}/[Cell]>>(\+ ord_memberchk(Cell, BlockedSet)),
            AllCells, FreeCells),

    list_to_ord_set(FreeCells, FreeSet),
    ord_union(FreeSet, PlacedSet, GraphCells),
    build_grid_graph(GraphCells, BlockedSet, Cx, Cy, Graph),

    tiles:door_goal_edges(Wr,Hr, S, Doors, DoorEdgesFull),
    safe_edge_indices(X0,Y0,S, Cx,Cy, DoorEdgesFull, DoorEdges),
    findall((X,Y),
        ( member(edge(X,Y,_Dir), DoorEdges),
          ord_memberchk((X,Y), FreeSet)
        ),
        StartCells0),
    sort(StartCells0, StartCells),

    ( StartCells == [] ->
        Diagnostic = no_door_access,
        !, fail
    ; true ),

    bfs_reachable(StartCells, Graph, ReachableSet),

    ( ord_subset(PlacedSet, ReachableSet) ->
        Diagnostic = success
    ; ord_subtract(PlacedSet, ReachableSet, Missing),
      Diagnostic = unreachable_cells(Missing),
      fail
    ).

rect_to_cell(S, X0, Y0, rect(X,Y,_,_), (Xg,Yg)) :-
    Xg is (X - X0) // S,
    Yg is (Y - Y0) // S.

/* ------------------------------------------------------------------
   Pass connectivity validation (Stage 3)
   ------------------------------------------------------------------ */
validate_pass_connectivity(RectsMM, Oris, Meta, Doors, Diagnostic) :-
    compute_pass_areas(RectsMM, Oris, Meta, PassAreas),
    compute_door_access_zones(Doors, DoorZones),

    tiles:struct_obstacles(StructObs),
    findall(Id-Obs, pass_blocked(PassAreas, StructObs, Id-Obs), BlockedList0),
    sort(BlockedList0, BlockedList),
    ( BlockedList == [] -> true
    ; Diagnostic = pass_blocked(BlockedList),
      format(user_error, '[connectivity] Pass strips blocked: ~w~n', [BlockedList]),
      !, fail
    ),

    ( DoorZones == [] ->
        Diagnostic = no_door_zones,
        !, fail
    ; true ),

    build_pass_graph(PassAreas, DoorZones, PassGraph),
    maplist(arg(1), PassAreas, PassIds0),
    list_to_ord_set(PassIds0, PassIdSet),

    findall(door(Id), member(door(Id,_), DoorZones), DoorNodes),
    bfs_reachable(DoorNodes, PassGraph, ReachableNodes),
    ord_intersection(ReachableNodes, PassIdSet, ReachablePassIds),

    ( ReachablePassIds == PassIdSet ->
        Diagnostic = success
    ; ord_subtract(PassIdSet, ReachablePassIds, MissingPass),
      Diagnostic = unreachable_pass(MissingPass),
      fail
    ).

compute_pass_areas(RectsMM, Oris, Meta, PassAreas) :-
    meta_unpack(Meta, S, _X0,_Y0, _Cx,_Cy, _Wr,_Hr),
    tile_meta(Meta, DeskW, DeskH, Aisle),
    findall(pass(Id, rect(Gx,Gy,Lw,Lh)),
        ( nth1(Id, RectsMM, rect(X,Y,_,_)),
          nth1(Id, Oris, Ori),
          tiles:tile_local_geometry(Ori, S, DeskW, DeskH, Aisle, _DeskLocal, rect(Lx,Ly,Lw,Lh)),
          Gx is X + Lx,
          Gy is Y + Ly
        ),
        PassAreas).

compute_door_access_zones(Doors, Zones) :-
    findall(door(Id, Rect),
        ( member(D, Doors),
          D = door(Id,_,_,_),
          zones:door_clear_zone(D, Rect)
        ),
        Zones).

pass_blocked(PassAreas, Obstacles, Id-Obs) :-
    member(pass(Id, PassRect), PassAreas),
    member(Obs, Obstacles),
    \+ rects:rect_no_overlap(PassRect, Obs).

build_pass_graph(PassAreas, DoorZones, Graph) :-
    empty_assoc(Empty),
    foldl(add_pass_neighbors(PassAreas), PassAreas, Empty, Graph0),
    foldl(add_door_neighbors(PassAreas), DoorZones, Graph0, Graph).

add_pass_neighbors(AllPass, pass(Id, PassRect), GraphIn, GraphOut) :-
    findall(NId,
        ( member(pass(NId, RectN), AllPass),
          NId \= Id,
          rects_touch_or_overlap(PassRect, RectN)
        ),
        Ns0),
    sort(Ns0, Ns),
    put_assoc(Id, GraphIn, Ns, GraphOut).

add_door_neighbors(AllPass, door(Id, DoorRect), GraphIn, GraphOut) :-
    findall(PassId,
        ( member(pass(PassId, PassRect), AllPass),
          rects_touch_or_overlap(DoorRect, PassRect)
        ),
        Ps0),
    sort(Ps0, Ps),
    put_assoc(door(Id), GraphIn, Ps, GraphOut).

rects_touch_or_overlap(rect(X1,Y1,W1,H1), rect(X2,Y2,W2,H2)) :-
    X1r is X1 + W1,
    Y1b is Y1 + H1,
    X2r is X2 + W2,
    Y2b is Y2 + H2,
    X1 =< X2r,
    X2 =< X1r,
    Y1 =< Y2b,
    Y2 =< Y1b.

/* ------------------------------------------------------------------
   Generic BFS over adjacency assoc
   ------------------------------------------------------------------ */
bfs_reachable(StartNodes, Graph, ReachableSet) :-
    bfs_queue(StartNodes, [], Graph, ReachableSet).

bfs_queue([], Visited, _Graph, Visited).
bfs_queue([Node|Rest], Visited, Graph, Reachable) :-
    ( ord_memberchk(Node, Visited) ->
        bfs_queue(Rest, Visited, Graph, Reachable)
    ; ord_add_element(Visited, Node, Visited1),
      ( get_assoc(Node, Graph, Neigh) -> true ; Neigh = [] ),
      append(Rest, Neigh, Queue1),
      bfs_queue(Queue1, Visited1, Graph, Reachable)
    ).

/* ------------------------------------------------------------------
   Internal helpers
   ------------------------------------------------------------------ */
meta_unpack(Meta, S, X0,Y0, Cx,Cy, Wr,Hr) :-
    S  = Meta.grid_mm,
    X0 = Meta.safe.x0,  Y0 = Meta.safe.y0,
    Cx = Meta.cells.x,  Cy = Meta.cells.y,
    Wr = Meta.room.w,   Hr = Meta.room.h.

tile_meta(Meta, DeskW, DeskH, Aisle) :-
    DeskW = Meta.tile.desk_w,
    DeskH = Meta.tile.desk_h,
    Aisle = Meta.tile.pass_mm.

all_grid_cells(Cx, Cy, Cells) :-
    Xmax is Cx - 1,
    Ymax is Cy - 1,
    findall((X,Y), (between(0, Xmax, X), between(0, Ymax, Y)), Cells).

safe_edge_indices(X0,Y0,S, Cx,Cy, EdgesFull, EdgesSafe) :-
    Kx is X0 // S,
    Ky is Y0 // S,
    Xmax is Cx - 1,
    Ymax is Cy - 1,
    findall(edge(Xs,Ys,Dir),
        ( member(edge(Xf,Yf,Dir), EdgesFull),
          Xs is Xf - Kx,
          Ys is Yf - Ky,
          Xs >= 0, Xs =< Xmax,
          Ys >= 0, Ys =< Ymax
        ),
        Raw),
    sort(Raw, EdgesSafe).

build_grid_graph(Cells, _BlockedSet, _Cx, _Cy, Graph) :-
    list_to_ord_set(Cells, CellSet),
    empty_assoc(Empty),
    foldl(add_cell_neighbors(CellSet), Cells, Empty, Graph).

add_cell_neighbors(CellSet, (X,Y), GraphIn, GraphOut) :-
    findall((Nx,Ny),
        ( neighbor_delta(Dx,Dy),
          Nx is X + Dx,
          Ny is Y + Dy,
          ord_memberchk((Nx,Ny), CellSet)
        ),
        Ns0),
    sort(Ns0, Ns),
    put_assoc((X,Y), GraphIn, Ns, GraphOut).

neighbor_delta(0,1).
neighbor_delta(1,0).
neighbor_delta(0,-1).
neighbor_delta(-1,0).

hard_obstacles_mm(Rects) :-
    zones:collect_zones(Rects).

rects_to_cells_grid(S, X0,Y0, Cx,Cy, RectsMM, Blocked) :-
    safe_area_rect(X0,Y0, Cx,Cy, S, Safe),
    findall((Xg,Yg),
      ( member(R0, RectsMM),
        intersect_rect(R0, Safe, rect(X,Y,W,H)),
        I0 is (X       - X0) // S,
        J0 is (Y       - Y0) // S,
        I1 is (X+W-1   - X0) // S,
        J1 is (Y+H-1   - Y0) // S,
        between(I0, I1, Xg),
        between(J0, J1, Yg)
      ),
      Raw),
    sort(Raw, Blocked).

safe_area_rect(X0,Y0, Cx,Cy, S, rect(X0,Y0, W,H)) :-
    W is Cx * S,
    H is Cy * S.

intersect_rect(rect(X1,Y1,W1,H1), rect(X2,Y2,W2,H2), rect(Xi,Yi,Wi,Hi)) :-
    Xr1 is X1 + W1,
    Yr1 is Y1 + H1,
    Xr2 is X2 + W2,
    Yr2 is Y2 + H2,
    Xi  is max(X1,X2),
    Yi  is max(Y1,Y2),
    Xo  is min(Xr1,Xr2),
    Yo  is min(Yr1,Yr2),
    Wi  is Xo - Xi,
    Hi  is Yo - Yi,
    Wi > 0,
    Hi > 0.

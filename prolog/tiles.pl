:- module(tiles,
  [ tile_square_size/4,            % +Wmm,+Hmm,+Aisle,-S
    tile_local_geometry/7,         % +Ori,+S,+W,+H,+Aisle,-DeskLocalRect,-PassLocalRect
    tile_main_dir/2,               % +Ori,-Dir
    cell_origin_mm/4,              % +S,+Xidx,+Yidx,-rect(X0,Y0,S,S)
    door_goal_edges/5,             % +Wr,+Hr,+S,+Doors,-GoalEdges
    struct_obstacles/1             % -RectsMM (columns/cabinets/etc. that block walk)
  ]).

:- use_module(rects).
:- use_module(zones).

/* ======= размер клетки-тайла ======= */
tile_square_size(W,H,Aisle,S) :-
    S is max(W, H + Aisle).    % для 1400×800 и A=600 => S=1400

/* ======= геометрия тайла (локально в пределах S×S) =======
   Ori∈{0,1,2,3}: полоса на стороне {n,w,s,e} соответственно.
   Добивка прилегает к столу и имеет размер:
     - горизонтальная полоса: W×A
     - вертикальная полоса:   A×W
*/
tile_local_geometry(0,_S,W,H,A, Desk, Pass) :-     % полоса СВЕРХУ (north)
    Desk = rect(0, A, W, H),
    Pass = rect(0, 0, W, A).

tile_local_geometry(1,_S,W,H,A, Desk, Pass) :-     % полоса СЛЕВА (west)
    Desk = rect(A, 0, H, W),
    Pass = rect(0, 0, A, W).

tile_local_geometry(2,_S,W,H,A, Desk, Pass) :-     % полоса СНИЗУ (south)
    Desk = rect(0, 0, W, H),
    Pass = rect(0, H, W, A).

tile_local_geometry(3,_S,W,H,A, Desk, Pass) :-     % полоса СПРАВА (east)
    Desk = rect(0, 0, H, W),
    Pass = rect(H, 0, A, W).

% ВАЖНО: main_dir должен соответствовать стороне ПОЛОСЫ:
tile_main_dir(0, n).
tile_main_dir(1, w).
tile_main_dir(2, s).
tile_main_dir(3, e).


/* ======= якорь клетки в мм ======= */
cell_origin_mm(S, Xidx, Yidx, rect(X0,Y0,S,S)) :-
    X0 is Xidx*S,
    Y0 is Yidx*S.

/* ======= двери -> «краевые» рёбра клетки, на которые разрешено примыкать =======
   Представляем «краевое» ребро как edge(X,Y,Dir), где (X,Y) — индекс клетки,
   Dir ∈ {n,e,s,w} — её внешняя сторона, которая граничит со стеной с дверью.
*/
door_goal_edges(Wr,Hr,S, Doors, GoalEdges) :-
    Cx is Wr // S, Cy is Hr // S,
    Xmax is Cx - 1, Ymax is Cy - 1,
    findall(edge(X,Y,Dir),
      ( member(door(_Id, side(Side), offset(Off), width(Wd)), Doors),
        ( Side == north ->
            Y is 0, Dir = n,
            X0 is Off // S, X1 is min(Xmax, (Off+Wd-1)//S), between(X0,X1,X)
        ; Side == south ->
            Y is Ymax, Dir = s,
            X0 is Off // S, X1 is min(Xmax, (Off+Wd-1)//S), between(X0,X1,X)
        ; Side == west  ->
            X is 0, Dir = w,
            Y0 is Off // S, Y1 is min(Ymax, (Off+Wd-1)//S), between(Y0,Y1,Y)
        ; Side == east  ->
            X is Xmax, Dir = e,
            Y0 is Off // S, Y1 is min(Ymax, (Off+Wd-1)//S), between(Y0,Y1,Y)
        )
      ),
      Raw),
    sort(Raw, GoalEdges).

/* ======= структурные препятствия, которые блокируют ПОЛОСУ (но не стол) ======= */
struct_obstacles(Rects) :-
    findall(R,
      ( zones:forbidden_zone(_Id,Type,R),
        blocks_walk_type(Type)
      ),
      Rects).

blocks_walk_type(struct).
blocks_walk_type(column).
blocks_walk_type(cabinet).
blocks_walk_type(obstacle).

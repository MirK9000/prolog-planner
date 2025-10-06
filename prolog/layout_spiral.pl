%%% ================================================================
%%% FILE: layout_spiral.pl
%%% Purpose: Stage 1 (Placement) — spiral-based tile placement core
%%% Exports: layout_tiles_spiral/6, layout_tiles_spiral_opts/7
%%% ================================================================
:- module(layout_spiral,
  [ layout_tiles_spiral/6,            % +N,+Wmm,+Hmm,-RectsMM,-Oris,-Meta
    layout_tiles_spiral_opts/7        % +N,+Wmm,+Hmm,+Opts,-RectsMM,-Oris,-Meta
  ]).

:- use_module(rects).
:- use_module(zones).
:- use_module(tiles).
:- use_module(connectivity).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(apply)).
:- use_module(library(yall)).

:- multifile progress_hook/1.
emit_progress(E) :- ignore(catch(progress_hook(E), _, true)).

/* ------------------------------------------------------------------
   Public API
   ------------------------------------------------------------------ */
layout_tiles_spiral(N, Wmm,Hmm, RectsMM, Oris, Meta) :-
    layout_tiles_spiral_opts(N, Wmm,Hmm, _{}, RectsMM, Oris, Meta).

% Opts is a dict with optional keys:
%   mode: greedy | bt | auto (default: auto -> greedy, fallback bt)
%   corner: tl | tr | br | bl (default: tl)
%   holes_budget: integer >= 0 (only for bt; default: inf)
%   wall_clear_mm: integer (override policy; default from zones:policy/2 or 0)
%   grid_offset_x, grid_offset_y: integers in mm (single offset)
%   grid_offsets: list of (OffX,OffY) pairs (in mm)
%   offset_search: auto | none   (default: none: no extra offsets)
layout_tiles_spiral_opts(N, Wmm,Hmm, Opts0, RectsMM, Oris, Meta) :-
    must_be(integer, N), N >= 0,
    rects:room_size(Wr,Hr),

    zones:get_policy(min_aisle_width_mm, Aisle),
    tiles:tile_square_size(Wmm,Hmm, Aisle, S),

    opt_or(Opts0, wall_clear_mm, WallClear0),
    resolve_wall_clear(WallClear0, WallClear),           % resolve to integer

    % Offsets list to iterate
    opt_grid_offsets(Opts0, S, Wr,Hr, WallClear, OffPairs),

    % options used equally for all offsets
    opt_corner(Opts0, Corner),
    opt_mode(Opts0, Mode),
    opt_holes_budget(Opts0, Budget),

    % try offsets in order, take first that succeeds
    member((OffX,OffY), OffPairs),
      attempt_one_offset(N, Wr,Hr, S, WallClear, Corner, Mode, Budget,
                         OffX,OffY, RectsMM, Oris, Meta),
    !.  % cut after first success

/* ------------------------------------------------------------------
   Attempt for a single (OffX,OffY)
   ------------------------------------------------------------------ */
attempt_one_offset(N, Wr,Hr, S, WallClear, Corner, Mode, Budget,
                   OffX,OffY, RectsMM, Oris, Meta) :-

    % Safe zone grid (origin snapped to S with offset, bounds shrunk by WallClear)
    safe_area_with_offsets(Wr,Hr, S, WallClear, OffX,OffY, X0,Y0, Cx,Cy),
    Cx > 0, Cy > 0,

    % Obstacles -> blocked grid cells
    hard_obstacles_mm(ObsMM),
    rects_to_cells_grid(S, X0,Y0, Cx,Cy, ObsMM, BlockedSet),

    % Spiral order (allow corner switch by mapping indices)
    spiral_cells(Cx,Cy, CellsBase),
    map_corner(Cx,Cy, Corner, CellsBase, CellsCornered),

    % Filter to free cells
    include({BlockedSet}/[XY]>>is_free_cell(BlockedSet, XY),
        CellsCornered, FreeCells0),
    connectivity:filter_isolated_cells(BlockedSet, Cx, Cy, FreeCells0, FreeCells),
    length(FreeCells0, FreeCount0),
    length(FreeCells, FreeCount),
    ( FreeCount0 =\= FreeCount ->
        Filtered is FreeCount0 - FreeCount,
        emit_progress(spiral_info(filtered_isolated(Filtered)))
    ; true ),

    emit_progress(spiral_start(_{n:N, room:_{w:Wr,h:Hr}, grid:S,
                                 safe:_{wall:WallClear, x0:X0,y0:Y0, cx:Cx,cy:Cy},
                                 offset:_{x:OffX,y:OffY},
                                 free:FreeCount, corner:Corner})),

    ( FreeCount < N -> emit_progress(spiral_finish(no_space, 0)), fail ; true ),

    % Choose placement mode
    ( Mode == greedy -> Try = greedy
    ; Mode == bt     -> Try = bt
    ; Try = auto
    ),

    ( Try == greedy ->
        ( place_spiral_greedy(N, X0,Y0,S, FreeCells, RectsMM)
        -> emit_progress(spiral_finish(ok_greedy, N))
        ;  emit_progress(spiral_info(greedy_failed)), fail
        )
    ; Try == bt ->
        place_spiral_bt(N, X0,Y0,S, FreeCells, Budget, RectsMM),
        emit_progress(spiral_finish(ok_bt, N))
    ; % auto: greedy then bt fallback
        ( place_spiral_greedy(N, X0,Y0,S, FreeCells, RectsMM)
        -> emit_progress(spiral_finish(ok_greedy, N))
        ;  emit_progress(spiral_info(greedy_failed)),
           place_spiral_bt(N, X0,Y0,S, FreeCells, Budget, RectsMM),
           emit_progress(spiral_finish(ok_bt, N))
        )
    ),

    % Orientations are placeholders for Stage 2
    length(RectsMM, K), K =:= N,
    length(Oris, N), maplist(=(0), Oris),

    % Get aisle width for Meta (required by connectivity validation)
    zones:get_policy(min_aisle_width_mm, Aisle),

    Meta = _{ grid_mm:S,
              cells:_{x:Cx,y:Cy},
              room:_{w:Wr,h:Hr},
              safe:_{wall:WallClear, x0:X0,y0:Y0},
              offset:_{x:OffX,y:OffY},
              mode:Mode, corner:Corner,
              tile:_{desk_w:Wmm, desk_h:Hmm, pass_mm:Aisle} },

    findall(door(Id,side(Side),offset(Off),width(Wd)),
            zones:door(Id,side(Side),offset(Off),width(Wd)),
            Doors),
    ( connectivity:validate_grid_connectivity(RectsMM, Meta, Doors, GridDiag) -> true
    ; emit_progress(spiral_finish(connectivity_failed, GridDiag)),
      fail
    ),

    % --- SAFETY CHECK: ни один тайл не пересекает collect_zones/1 ---
    zones:collect_zones(Zs),
    (   forall(member(R, RectsMM),
            forall(member(Z, Zs), rects:rect_no_overlap(R,Z)))
    ->  true
    ;   emit_progress(spiral_finish(conflict_zones, _)),
        member(BadR, RectsMM),
        member(BadZ, Zs),
        \+ rects:rect_no_overlap(BadR, BadZ),
        format(user_error, '~n[spiral] CONFLICT ~p vs zone ~p~n', [BadR,BadZ]),
        fail
    ),


/* ------------------------------------------------------------------
   Options helpers
   ------------------------------------------------------------------ */
opt_or(Opts, Key, Val) :- ( get_dict(Key, Opts, V) -> Val=V ; Val=unset ).

% resolve_wall_clear(+OptValOrUnset, -WallClearMM)
% безопасное разрешение отступа от стен, мм
resolve_wall_clear(OptVal, M) :-
    ( OptVal == unset
      -> zones:get_policy(wall_clear_mm, Val0)
      ;  Val0 = OptVal
    ),
    % страхуемся от мусора: если не integer — берём 0; ниже отрезаем < 0
    ( integer(Val0) -> M1 = Val0 ; M1 = 0 ),
    ( M1 >= 0 -> M = M1 ; M = 0 ).

opt_corner(Opts, Corner) :-
    ( get_dict(corner, Opts, C0) -> C=C0 ; C=tl ),
    memberchk(C, [tl,tr,br,bl]),
    Corner = C.

opt_mode(Opts, Mode) :-
    ( get_dict(mode, Opts, M0) -> M=M0 ; M=auto ),
    memberchk(M, [greedy,bt,auto]),
    Mode = M.

opt_holes_budget(Opts, Budget) :-
    ( get_dict(holes_budget, Opts, B0) -> B=B0 ; B=inf ),
    ( B == inf -> Budget=inf ; (must_be(integer,B), B>=0, Budget=B) ).

/* ------------------------------------------------------------------
   Offsets helpers
   ------------------------------------------------------------------ */
% Normalize Offsets source -> list of pairs within [0..S)
/* ------------------------------------------------------------------
   Offsets helpers (capacity-preserving)
   ------------------------------------------------------------------ */
% opt_grid_offsets(+Opts,+S,+Wr,+Hr,+Wall,-OffPairs)
% Auto: ищем смещения, НЕ уменьшающие число колонок/рядов относительно базового (0,0).
opt_grid_offsets(Opts, S, Wr,Hr, Wall, OffPairs) :-
    ( get_dict(grid_offsets, Opts, Pairs0) ->
        maplist(normalize_pair(S), Pairs0, OffPairs)
    ; ( get_dict(grid_offset_x, Opts, _OX) ; get_dict(grid_offset_y, Opts, _OY) ) ->
        ( get_dict(grid_offset_x, Opts, OX0) -> true ; OX0 = 0 ),
        ( get_dict(grid_offset_y, Opts, OY0) -> true ; OY0 = 0 ),
        normalize_pair(S, (OX0,OY0), P),
        OffPairs = [P]
    ; ( get_dict(offset_search, Opts, ModeS), ModeS == auto ) ->
        zones:get_policy(tile_slide_step_mm, Step0),
        ( integer(Step0), Step0 > 0 -> Step = Step0 ; Step = 100 ),
        % базовая ёмкость сетки без смещения
        CxBase is max(0, ((Wr - Wall) // S)),
        CyBase is max(0, ((Hr - Wall) // S)),
        % допустимые OffX, OffY, не снижающие Cx/Cy
        RemX is (Wr - Wall) mod S,
        RemY is (Hr - Wall) mod S,
        x_candidates(S, Step, RemX, Xs),
        y_candidates(S, Step, RemY, Ys),
        findall((Xn,Yn),
                ( member(X, Xs), member(Y, Ys),
                  normalize_pair(S, (X,Y), (OX,OY)),
                  % пересчитаем Cx/Cy для данного оффсета
                  ceil_to_grid_with_off(Wall, S, OX, X0),
                  ceil_to_grid_with_off(Wall, S, OY, Y0),
                  Cx is max(0, ((Wr - Wall - X0) // S)),
                  Cy is max(0, ((Hr - Wall - Y0) // S)),
                  Cx =:= CxBase,  % сохраняем ёмкость
                  Cy =:= CyBase,
                  Xn = OX, Yn = OY
                ),
                Pairs1),
        ( Pairs1 == [] -> OffPairs = [(0,0)] ; sort(Pairs1, OffPairs) )
    ; OffPairs = [(0,0)]
    ).

x_candidates(S, Step, Rem, Xs) :-
    Max is min(Rem, S-1),
    findall(X, (between(0, Max, X), 0 is X mod Step), Xs).

y_candidates(S, Step, Rem, Ys) :-
    % если остаток 0 — любые положительные смещения режут число рядов -> оставляем только 0
    ( Rem =:= 0 -> Ys = [0]
    ; Max is min(Rem, S-1),
      findall(Y, (between(0, Max, Y), 0 is Y mod Step), Ys)
    ).

normalize_pair(S, (OX0,OY0), (OX,OY)) :-
    must_be(integer, OX0), must_be(integer, OY0),
    norm_offset(OX0, S, OX),
    norm_offset(OY0, S, OY).

norm_offset(Off, S, N) :-
    T is Off mod S,
    ( T >= 0 -> N = T ; N is T + S ).



/* ------------------------------------------------------------------
   Safe area computation for shifted grid
   ------------------------------------------------------------------ */
ceil_to_grid_with_off(Value, S, Off, Aligned) :-
    Aligned is ((Value - Off + S - 1) // S) * S + Off.

floor_to_grid_with_off(Value, S, Off, Aligned) :-
    Aligned is ((Value - Off) // S) * S + Off.

safe_area_with_offsets(Wr,Hr, S, Wall, OffX,OffY, X0,Y0, Cx,Cy) :-
    Xmin is Wall,  Ymin is Wall,
    ceil_to_grid_with_off(Xmin, S, OffX, X0),
    ceil_to_grid_with_off(Ymin, S, OffY, Y0),
    Xmax is Wr - Wall,
    Ymax is Hr - Wall,
    Cx is max(0, (Xmax - X0) // S),
    Cy is max(0, (Ymax - Y0) // S).

/* ------------------------------------------------------------------
   Obstacles -> blocked grid cells
   ------------------------------------------------------------------ */
% By default we treat *all* forbidden zones as hard obstacles for tile squares.
hard_obstacles_mm(Rects) :-
    zones:collect_zones(Rects).

% прямоугольник безопасной области
safe_area_rect(X0,Y0, Cx,Cy, S, rect(X0,Y0, W,H)) :-
    W is Cx*S, H is Cy*S.

% пересечение прямоугольников (строгое по площади)
intersect_rect(rect(X1,Y1,W1,H1), rect(X2,Y2,W2,H2), rect(Xi,Yi,Wi,Hi)) :-
    Xr1 is X1+W1,  Yr1 is Y1+H1,
    Xr2 is X2+W2,  Yr2 is Y2+H2,
    Xi  is max(X1,X2),
    Yi  is max(Y1,Y2),
    Xo  is min(Xr1,Xr2),
    Yo  is min(Yr1,Yr2),
    Wi  is Xo - Xi,
    Hi  is Yo - Yi,
    Wi > 0, Hi > 0.

% Было: прямой расчёт индексов -> теряем граничные случаи
% Стало: клиппинг зоны по безопасной области + индексация
rects_to_cells_grid(S, X0,Y0, Cx,Cy, RectsMM, Blocked) :-
    safe_area_rect(X0,Y0, Cx,Cy, S, Safe),
    findall((Xg,Yg),
      ( member(R0, RectsMM),
        intersect_rect(R0, Safe, rect(X,Y,W,H)),   % <-- клиппинг
        I0 is (X       - X0) // S,
        J0 is (Y       - Y0) // S,
        I1 is (X+W-1   - X0) // S,
        J1 is (Y+H-1   - Y0) // S,
        between(I0, I1, Xg),
        between(J0, J1, Yg)
      ),
      Raw),
    sort(Raw, Blocked).

is_free_cell(BlockedSet, XY) :-
    \+ ord_memberchk(XY, BlockedSet).

/* ------------------------------------------------------------------
   Spiral order over Cx×Cy grid. Base corner = top-left (tl).
   Other corners map via coordinate transforms.
   ------------------------------------------------------------------ */
spiral_cells(Cx,Cy, Cells) :-
    R is Cx - 1,
    B is Cy - 1,
    spiral_bounds(0,0, R,B, right, Cells).

spiral_bounds(L,T, R,B, _, []) :- (L>R ; T>B), !.
spiral_bounds(L,T, R,B, right, Cells) :-
    findall((X,T), between(L,R,X), Top),
    T1 is T+1,
    spiral_bounds(L,T1, R,B, down, Rest),
    append(Top, Rest, Cells).
spiral_bounds(L,T, R,B, down, Cells) :-
    findall((R,Y), between(T,B,Y), RightCol),
    R1 is R-1,
    spiral_bounds(L,T, R1,B, left, Rest),
    append(RightCol, Rest, Cells).
spiral_bounds(L,T, R,B, left, Cells) :-
    ( L =< R -> findall((X,B), between(L,R,X), BotF), reverse(BotF, Bot) ; Bot = [] ),
    B1 is B-1,
    spiral_bounds(L,T, R,B1, up, Rest),
    append(Bot, Rest, Cells).
spiral_bounds(L,T, R,B, up, Cells) :-
    ( T =< B -> findall((L,Y), between(T,B,Y), LeftF), reverse(LeftF, Left) ; Left = [] ),
    L1 is L+1,
    spiral_bounds(L1,T, R,B, right, Rest),
    append(Left, Rest, Cells).

% Corner mapping
map_corner(_Cx,_Cy, tl, Cells, Cells).
map_corner(Cx,_Cy, tr, Cells, Cells2) :- maplist({Cx}/[(X,Y),(X2,Y)]>>(X2 is (Cx-1)-X), Cells, Cells2).
map_corner(Cx,Cy, br, Cells, Cells3) :- maplist({Cx,Cy}/[(X,Y),(X2,Y2)]>>(X2 is (Cx-1)-X, Y2 is (Cy-1)-Y), Cells, Cells3).
map_corner(_Cx,Cy, bl, Cells, Cells4) :- maplist({Cy}/[(X,Y),(X,Y2)]>>(Y2 is (Cy-1)-Y), Cells, Cells4).

/* ------------------------------------------------------------------
   Placement strategies
   ------------------------------------------------------------------ */
% Greedy: take first N free cells
place_spiral_greedy(N, X0,Y0,S, FreeCells, RectsMM) :-
    length(FreeCells, L), L >= N,
    length(Chosen, N), append(Chosen, _, FreeCells),
    maplist(cell_to_rect(X0,Y0,S), Chosen, RectsMM).

% Backtracking: put-or-skip with early pruning and optional holes budget
place_spiral_bt(N, X0,Y0,S, Cells, Budget, RectsMM) :-
    place_bt(N, X0,Y0,S, Cells, 0, Budget, RectsMM).

place_bt(0, _X0,_Y0,_S, _Cells, _Placed, _Budget, []) :- !.
place_bt(N, _X0,_Y0,_S, [], Placed, _Budget, _) :- Placed < N, !, fail.
place_bt(N, X0,Y0,S, [C|Cs], Placed, Budget, Rects) :-
    length(Cs, Rem), Need is N - Placed,
    ( Rem + 1 < Need -> !, fail ; true ),  % early cutoff
    ( % A) put here
      Need > 0,
      cell_to_rect(X0,Y0,S, C, R),
      Rects = [R|Rs],
      Pl1 is Placed + 1,
      place_bt(N, X0,Y0,S, Cs, Pl1, Budget, Rs)
    ; % B) skip here (respect holes budget if finite)
      ( Budget == inf -> Budget1 = inf
      ; Budget  >  0  -> Budget1 is Budget - 1
      ),
      nonvar(Budget1),
      place_bt(N, X0,Y0,S, Cs, Placed, Budget1, Rects)
    ).

cell_to_rect(X0,Y0,S, (Xg,Yg), rect(X,Y,S,S)) :-
    X is X0 + Xg*S,
    Y is Y0 + Yg*S.

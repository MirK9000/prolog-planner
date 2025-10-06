:- module(layout_tiles,
  [ layout_tiles/6,                % +N,+Wmm,+Hmm,-RectsMM,-Oris,-Meta
    layout_tiles_to_file/2,
    run_tiles_benchmark/0
  ]).

:- use_module(rects).
:- use_module(zones).
:- use_module(paths).
:- use_module(tiles).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(debug)).
:- use_module(library(http/json)).   % для JSON writer в layout_tiles_to_file/2


:- multifile progress_hook/1.
emit_progress(E) :- ignore(catch(progress_hook(E), _, true)).

/* ========= публичный API ========= */
layout_tiles(N, Wmm,Hmm, RectsMM, Oris, Meta) :-
    must_be(integer, N), N >= 0,
    rects:room_size(Wr,Hr),
    zones:get_policy(min_aisle_width_mm, Aisle),
    tiles:tile_square_size(Wmm,Hmm, Aisle, S),
    Cx is Wr // S, Cy is Hr // S,
    Cx > 0, Cy > 0,


    % собрать зоны для «стола» и «полосы»
    once(zones:collect_zones(ZonesDesk)),                      % стол их избегает
    tiles:struct_obstacles(StructObs),                         % полоса их избегает

    % двери: целевые краевые рёбра
    findall(door(Id,side(S),offset(Off),width(Wd)),
            zones:door(Id,side(S),offset(Off),width(Wd)),
            Doors),
    tiles:door_goal_edges(Wr,Hr, S, Doors, DoorEdges),

    zones:get_policy(tiles_connectivity, ConnMode),
    emit_progress(tiles_start(N, _{room:_{w:Wr,h:Hr}, grid:S, cells:_{x:Cx,y:Cy}, connectivity:ConnMode})),


    % поиск
    Grid = _{cx:Cx, cy:Cy, s:S, wr:Wr, hr:Hr},
    place_tiles(N, Wmm,Hmm, Aisle, Grid, ZonesDesk, StructObs, DoorEdges,
                [], [], Placed),                % Placed = [cell(X,Y,Ori,DeskRectMM)|...]

    % распаковать результат
    maplist(placed_pair, Placed, Pairs),     % Pairs: Rect-Ori
    pairs_keys_values(Pairs, RectsMM, Oris),

    Meta = _{ grid_mm:S, cells:_{x:Cx,y:Cy}, aisle:Aisle, room:_{w:Wr,h:Hr} },

    % финальная гарантия эвакуации: один тяжёлый прогон
    ( Doors == [] ->
        true
    ; 
    evac_grid_mm(EGrid),
    paths:check_evac_all(Wr,Hr, EGrid, Doors, Aisle, RectsMM)
      -> true
      ;  emit_progress(tiles_finish(no_solution_evac, 0)), fail
    ),
    length(RectsMM, K),
    emit_progress(tiles_finish(ok, K)).

    placed_pair(cell(_X,_Y,O,R), R-O).

/* ========= поисковая часть ========= */

% place_tiles(+N,+W,+H,+Aisle,+Grid,+ZonesDesk,+StructObs,+DoorEdges,
%             +PlacedSoFar,+OccupiedCells,-PlacedOut)
place_tiles(0,_W,_H,_A, _Grid,_Z,_S,_DoorEdges, Placed, _Occ, Placed) :- !.
place_tiles(N,W,H,A, Grid,ZonesDesk,StructObs,DoorEdges, Placed0, Occupied, Placed) :-
    % выбрать следующую клетку (для первого тайла — волна от дверей)
    next_cells_smart(W,H,A, Grid, ZonesDesk, StructObs, DoorEdges,
                     Placed0, Occupied, Cells),

    % попробуем по кандидатным клеткам
    sort_cells(Cells, CellsSorted),
    member((X,Y), CellsSorted),

    % четыре ориентации
    between(0,3, Ori),
    try_place_tile(W,H,A, Grid, ZonesDesk,StructObs,DoorEdges, Placed0, (X,Y,Ori), CellPlaced),
    % успех
    N1 is N - 1,
    place_tiles(N1,W,H,A, Grid,ZonesDesk,StructObs,DoorEdges,
                [CellPlaced|Placed0], [X-Y|Occupied], Placed).


/* --- выбор следующей клетки --- */
next_cells(Grid, DoorEdges, [], Occupied, Cells) :- !,
    % стартуем у дверей, если есть подходящие клетки; иначе fallback ко "всем"
    door_adjacent_cells(Grid, DoorEdges, Candidates0),
    exclude(is_occupied(Occupied), Candidates0, Candidates),
    ( Candidates \= [] ->
        Cells = Candidates
    ;   all_grid_cells(Grid, All),
        exclude(is_occupied(Occupied), All, Cells)
    ).
next_cells(Grid, _DoorEdges, Placed, Occupied, Cells) :-
    % фронтир = 4-соседи уже поставленных
    findall((X2,Y2),
      ( member(cell(X,Y,_Ori,_Rect), Placed),
        neighbor4(Grid, X,Y, X2,Y2),
        \+ is_occupied(Occupied, X2-Y2)
      ),
      Raw),
    sort(Raw, Frontier),
    ( Frontier \= [] ->
        Cells = Frontier
    ;   % Fallback: если рядом вообще негде — ищем просто свободные клетки
        all_grid_cells(Grid, All),
        exclude(is_occupied(Occupied), All, Cells)
    ).

is_occupied(Occ, X-Y) :- memberchk(X-Y, Occ).

neighbor4(_, X,Y, X2,Y) :- X2 is X-1, X2 >= 0.
neighbor4(G,  X,Y, X2,Y) :- X2 is X+1, X2 < G.cx.
neighbor4(_, X,Y, X,Y2) :- Y2 is Y-1, Y2 >= 0.
neighbor4(G,  X,Y, X,Y2) :- Y2 is Y+1, Y2 < G.cy.

door_adjacent_cells(G, DoorEdges, Cells) :-
    findall((X,Y),
      ( member(edge(Xe,Ye,Dir), DoorEdges),
        ( Dir==n -> X=Xe, Y=Ye
        ; Dir==s -> X=Xe, Y=Ye
        ; Dir==w -> X=Xe, Y=Ye
        ; Dir==e -> X=Xe, Y=Ye ),
        inside_grid(G, X,Y)
      ),
      Raw),
    sort(Raw, Cells).

inside_grid(G,X,Y) :- X>=0, X<G.cx, Y>=0, Y<G.cy.

sort_cells(Cells, Sorted) :-
    map_list_to_pairs(yx_key, Cells, Pairs),
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, Sorted).
yx_key((X,Y), (Y,X)).

/* --- проверка возможности поставить тайл в (X,Y) с ориентацией Ori --- */
try_place_tile(W,H,A, G, ZonesDesk,StructObs,DoorEdges, Placed, (X,Y,Ori), cell(X,Y,Ori, DeskMM)) :-
    % базовые локальные прямоугольники "стол + полоса" без сдвигов
    tiles:tile_local_geometry(Ori, G.s, W,H, A, DeskLocal0, PassLocal),
    % позволяем столу скользить в рамках клетки
    desk_slide_ranges(Ori, G.s, W,H,A, DxMax, DyMax),
    slide_step_mm(Step),

    between(0, DxMax, Dx), 0 is Dx mod Step,
    between(0, DyMax, Dy), 0 is Dy mod Step,

    shift_rect(Dx,Dy, DeskLocal0, DeskLocal),

    % перевод в мм
    cell_origin_mm(G.s, X,Y, rect(X0,Y0,_,_)),
    shift_rect(X0,Y0, DeskLocal, DeskMM),
    shift_rect(X0,Y0, PassLocal, PassMM),

    % 1) внутри комнаты
    rects:inside_room(rect(0,0,G.wr,G.hr), DeskMM),
    rects:inside_room(rect(0,0,G.wr,G.hr), PassMM),

    % 2) стол не пересекает зоны
    no_overlap_with_list(DeskMM, ZonesDesk),

    % 3) полоса не пересекает структурные препятствия
    no_overlap_with_list(PassMM, StructObs),

    % 4) локальная связность: кроме самого первого тайла
    tiles:tile_main_dir(Ori, Dir),
    ( Placed == [] ->
        true
    ;   zones:get_policy(tiles_connectivity, Mode),    % strict | loose (по умолчанию strict)
        connectivity_ok(Mode, X,Y,Dir, DoorEdges, Placed)
    ).


shift_rect(DX,DY, rect(X,Y,W,H), rect(X2,Y2,W,H)) :- X2 is DX+X, Y2 is DY+Y.

no_overlap_with_list(_Rect, []).
no_overlap_with_list(Rect, [R|Rs]) :-
    rects:rect_no_overlap(Rect, R),
    no_overlap_with_list(Rect, Rs).

touches_door_edge(X,Y,Dir, DoorEdges) :-
    memberchk(edge(X,Y,Dir), DoorEdges).

touches_any_tile(X,Y,Dir, Placed) :-
    opposite(Dir, Opp),
    neighbor_cell(X,Y, Dir, X2,Y2),
    member(cell(X2,Y2, Ori2, _), Placed),
    tiles:tile_main_dir(Ori2, Dir2),
    Dir2 == Opp.

neighbor_cell(X,Y, n, X, Y-1).
neighbor_cell(X,Y, s, X, Y+1).
neighbor_cell(X,Y, w, X-1, Y).
neighbor_cell(X,Y, e, X+1, Y).

opposite(n,s). opposite(s,n). opposite(w,e). opposite(e,w).

/* ========= утилиты выгрузки ========= */

layout_tiles_to_file(OutFile, Format) :-
    ( current_module(M), predicate_property(M:task(_,_), defined)
    -> clause(M:task(Count, size(W,H)), true)
    ;  throw(error(existence_error(predicate, task/2), _))
    ),
    layout_tiles(Count, W,H, RectsMM, Oris, Meta),
    ( Format == json -> write_solution_json(OutFile, Meta.grid_mm, RectsMM, Oris, W,H)
    ; write_solution_pl(OutFile, Meta.grid_mm, RectsMM, Oris)
    ).


write_solution_json(File, Grid, Rects, Oris, W0,H0) :-
    open(File, write, S, [encoding(utf8)]),
    maplist(rect_ori_dict(Grid, W0,H0), Rects, Oris, Dicts),
    Sol = _{ grid: Grid, desks: Dicts },
    json_write_dict(S, Sol, [width(0)]), nl(S), close(S).

write_solution_pl(File, Grid, Rects, Oris) :-
    open(File, write, S, [encoding(utf8)]),
    format(S, '% generated by tiles planner~n', []),
    format(S, 'solution(grid(~w)).~n~n', [Grid]),
    findall(_, (nth1(I,Rects,R), nth1(I,Oris,O), rect_to_pl(S,I,R,O)), _),
    close(S).

rect_to_pl(S,I, rect(X,Y,W,H), Ori) :-
    format(atom(Id), 'desk-~d', [I]),
    format(S, 'placed_object(~q, desk, rect(~w,~w,~w,~w), [orientation(~w)]).~n',
           [Id, X,Y,W,H, Ori]).

rect_ori_dict(Grid, W0,H0, rect(X,Y,W,H), Ori, D) :-
    gensym('desk-', Sym),
    D = _{ id:Sym, type:desk, rect:_{x:X,y:Y,w:W,h:H}, orientation:Ori,
           grid:Grid, base_size:_{w:W0,h:H0} }.

slide_step_mm(Step) :-
    ( zones:policy(tile_slide_step_mm, Step0) -> Step = Step0 ; Step = 100 ).

% Сколько «люфта» у стола внутри клетки по осям (мм)
desk_slide_ranges(Ori, S, W, H, A, DxMax, DyMax) :-
    ( Ori = 0 ; Ori = 2 ) ->
        DxMax is max(0, S - W),
        DyMax is max(0, S - (H + A))
    ; ( Ori = 1 ; Ori = 3 ) ->
        DxMax is max(0, S - (H + A)),
        DyMax is max(0, S - W).

% ВАЖНО: выше я оставил "800" жёстко как H-база столешницы при повороте.
% Если хотите полную универсальность без предположений — перепишите на:
%   - для Ori=1/3: ширина стола по X = H, значит DxMax is max(0, S - H).
%   - для Ori=0/2: высота стола по Y = H, значит DyMax is max(0, S - H).
% Тогда не будет хардкода на 800 мм.

all_grid_cells(G, Cells) :-
    Xmax is G.cx - 1,
    Ymax is G.cy - 1,
    findall((X,Y),
        ( between(0, Xmax, X),
          between(0, Ymax, Y)
        ),
        Raw),
    sort(Raw, Cells).


% layout_tiles.pl --- ДОБАВИТЬ рядом с утилитами:

% Определяем, есть ли у тайла полоса на конкретной стороне клетки.
% Вся логика уже есть в tiles:tile_main_dir/2.
pass_on_side(Ori, n) :- tiles:tile_main_dir(Ori, n).
pass_on_side(Ori, e) :- tiles:tile_main_dir(Ori, e).
pass_on_side(Ori, s) :- tiles:tile_main_dir(Ori, s).
pass_on_side(Ori, w) :- tiles:tile_main_dir(Ori, w).

% «Широкая» проверка: у соседа на общей грани есть полоса, обращённая к нам.
touches_any_tile_wide(X,Y, Placed) :-
    ( neighbor_cell(X,Y, n, Xn,Yn), member(cell(Xn,Yn, OriN, _), Placed), pass_on_side(OriN, s)
    ; neighbor_cell(X,Y, s, Xs,Ys), member(cell(Xs,Ys, OriS, _), Placed), pass_on_side(OriS, n)
    ; neighbor_cell(X,Y, w, Xw,Yw), member(cell(Xw,Yw, OriW, _), Placed), pass_on_side(OriW, e)
    ; neighbor_cell(X,Y, e, Xe,Ye), member(cell(Xe,Ye, OriE, _), Placed), pass_on_side(OriE, w)
    ).

% Под капотом переключаемся по политике:
connectivity_ok(strict, X,Y,Dir, DoorEdges, Placed) :-
    ( touches_door_edge(X,Y,Dir, DoorEdges)
    ; touches_any_tile(X,Y,Dir, Placed)
    ).
connectivity_ok(loose, X,Y,_Dir, DoorEdges, Placed) :-
    ( memberchk(edge(X,Y,_), DoorEdges)          % у любой двери, без ориентации
    ; touches_any_neighbor_cell(X,Y, Placed)     % соседняя по клетке — без полосного рукопожатия
    ).

evac_grid_mm(G) :-
    ( zones:policy(grid_mm, G0) -> G = G0    % можно переопределить в плане
    ; G = 100                                % иначе по умолчанию 100 мм
    ).


/* --- выбор следующей клетки (door-aware) --- */

% Умный выбор. Для первого тайла — волной от дверей до ближайших пригодных клеток.
% Для остальных тайлов оставляем прежнюю фронтир-логику.
next_cells_smart(W,H,A, G, ZonesDesk, StructObs, DoorEdges, [], Occupied, Cells) :- !,
    door_adjacent_cells(G, DoorEdges, Seeds0),
    exclude(is_occupied(Occupied), Seeds0, Seeds),
    ( Seeds == []
      -> all_grid_cells(G, All0),
         exclude(is_occupied(Occupied), All0, CellsAll),
         sort_cells(CellsAll, Cells)                % дверей нет — просто сортируем все
      ;  bfs_feasible_ring(W,H,A, G, ZonesDesk, StructObs, DoorEdges,
                           Seeds, Occupied, CellsRing),
         ( CellsRing \= []
           -> sort_cells(CellsRing, Cells)          % ближайшие пригодные к двери
           ;  % защиты на случай «в волне ничего не нашлось» (крайне редко)
              all_grid_cells(G, All0),
              exclude(is_occupied(Occupied), All0, CellsAll),
              sort_cells(CellsAll, Cells)
         )
    ).
next_cells_smart(_W,_H,_A, Grid,_Z,_S,DoorEdges, Placed, Occupied, Cells) :-
    % старая фронтир-логика
    next_cells(Grid, DoorEdges, Placed, Occupied, Cells).

% Волна по клеткам от набора Seeds; возвращает первое «кольцо» клеток,
% где существует хотя бы одна фактическая постановка стола без пересечений.
bfs_feasible_ring(W,H,A, G, ZonesDesk, StructObs, DoorEdges, Seeds, Occupied, RingOut) :-
    bfs_levels_init(Seeds, Occupied, Vis0, Q0),
    bfs_levels_loop(W,H,A, G, ZonesDesk, StructObs, DoorEdges, Vis0, Q0, RingOut).

bfs_levels_init(Seeds, Occupied, Vis, Q) :-
    exclude(is_occupied(Occupied), Seeds, SeedsFree),
    sort(SeedsFree, Vis),   % Vis как множество
    Q = [SeedsFree].        % уровни (список колец)

bfs_levels_loop(_W,_H,_A,_G,_Z,_S,_D, _Vis, [], []) :- !.
bfs_levels_loop(W,H,A, G, Z, S, D, Vis, [Level|Rest], RingOut) :-
    emit_progress(tiles_dbg(bfs_level(Level))),

    include(feasible_cell(W,H,A, G, Z, S, D), Level, Feasible),
    ( Feasible \= [] ->
        emit_progress(tiles_dbg(bfs_feasible(Feasible))),
        RingOut = Feasible
    ;   % расширяем на следующий уровень
        findall((X2,Y2),
          ( member((X,Y), Level),
            neighbor4(G, X,Y, X2,Y2),
            \+ memberchk((X2,Y2), Vis),
            inside_grid(G, X2,Y2)
          ),
          RawNext),
        sort(RawNext, NextLevel),
        append(Vis, NextLevel, Vis1),
        bfs_levels_loop(W,H,A, G, Z, S, D, Vis1, [NextLevel|Rest], RingOut)
    ).


% Клетка «пригодна», если существует хотя бы ОДНА ориентация/сдвиг,
% при которой try_place_tile/… проходит (т.е. стол и полоса не лезут в запретки).
feasible_cell(W,H,A, G, ZonesDesk, StructObs, DoorEdges, (X,Y)) :-
    between(0,3, Ori),
    once(try_place_tile(W,H,A, G, ZonesDesk, StructObs, DoorEdges,
                        [], (X,Y,Ori), _CellPlaced)).


touches_any_neighbor_cell(X,Y, Placed) :-
    ( neighbor_cell(X,Y, n, Xn,Yn), member(cell(Xn,Yn,_,_), Placed)
    ; neighbor_cell(X,Y, s, Xs,Ys), member(cell(Xs,Ys,_,_), Placed)
    ; neighbor_cell(X,Y, w, Xw,Yw), member(cell(Xw,Yw,_,_), Placed)
    ; neighbor_cell(X,Y, e, Xe,Ye), member(cell(Xe,Ye,_,_), Placed)
    ).

/* ========= тестовый бенч ========= */
run_tiles_benchmark :-
    zones:get_policy(min_aisle_width_mm, Aisle),
    format('Aisle=~w mm~n', [Aisle]),
    ( rects:room_size(_,_)
      -> true
      ;  asserta(rects:room_size(6000,4000))
    ),
    layout_tiles(7, 1200,800, _Rects,_Oris,_Meta).

:- begin_tests(layout_tiles_fallback, [setup(setup_rt), cleanup(cleanup_rt)]).
:- use_module(layout_tiles).
:- use_module(rects).
:- use_module(zones).

setup_rt :-
  asserta(rects:room_size(6000, 2000)),
  % одна дверь сверху
  asserta(zones:door(d1, side(north), offset(0), width(800))),
  % узкая «горлышко» справа, чтобы фронтир мог иссякнуть
  asserta(zones:forbidden_zone(fz, struct, rect(800,0,200,1000))),
  % политики по умолчанию: min_aisle_width_mm=1200 => под тест можно ослабить
  asserta(zones:policy(min_aisle_width_mm, 800)),
  asserta(zones:policy(tiles_connectivity, strict)).

cleanup_rt :-
  retractall(rects:room_size(_,_)),
  retractall(zones:door(_,_,_,_)),
  retractall(zones:forbidden_zone(_,_,_)),
  retractall(zones:policy(_,_)).

test('fallback places 3 desks (1200x800) with grid derived from tile size') :-
  once(layout_tiles:layout_tiles(3, 1200,800, Rects, Oris, Meta)),
  Rects \= [], Oris \= [],
  Meta.grid_mm > 0.

:- end_tests(layout_tiles_fallback).

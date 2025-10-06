:- module(plan_loader,
  [ load_plan/1,        % +File
    apply_plan/0,
    clear_plan/0,
    solve_plan/3,       % -Rects,-Oris,-Grid
    plan_info/0,
    solve_plan_to_file/1,    % <--- добавить
    solve_plan_to_file/2     % <--- добавить
  ]).

:- use_module(rects).
:- use_module(zones).
:- use_module(layout).
:- use_module(library(http/json)).   % для JSON-выгрузки
:- use_module(library(option)).
:- use_module(library(debug)).

:- multifile layout:progress_hook/1.
:- dynamic  pl_progress_state/1, pl_progress_stream/1, pl_progress_mode/1.
% pl_progress_mode(silent|brief|verbose).  % по умолчанию brief

enable_progress :- enable_progress(brief).
enable_progress(Mode) :-
    retractall(pl_progress_mode(_)),
    asserta(pl_progress_mode(Mode)).

enable_progress(json(File)) :-
    enable_progress(verbose),
    ( pl_progress_stream(_) -> true
    ; open(File, write, S, [encoding(utf8)]),
      asserta(pl_progress_stream(S))
    ).

disable_progress :-
    retractall(pl_progress_mode(_)),
    ( retract(pl_progress_stream(S)) -> close(S) ; true ).

layout:progress_hook(E) :-
    ( pl_progress_mode(_) -> true ; pl_progress_mode(brief) ),
    get_time(Now),
    ( E = start(N, Wr,H, Grid) ->
        retractall(pl_progress_state(_)),
        asserta(pl_progress_state(state{t0:Now, placed:0, total:N,
                                        room:Wr-H, grid:Grid})),
        say('[start] N=~w room=~wx~w grid=~w~n', [N,Wr,H,Grid]),
        jlog(_{event:start, n:N, room:_{w:Wr,h:H}, grid:Grid})
    ; E = try_place(D) ->
        maybe_verbose('try ~p~n', [D]),
        jlog(_{event:try_place, data:D})
    ; E = reject(Rect, Reason) ->
        maybe_verbose('reject ~p due to ~w~n', [Rect,Reason]),
        jlog(_{event:reject, rect:Rect, reason:Reason})
    ; E = placed(K, N, Rect, FLen) ->
        retract(pl_progress_state(S)),
        S1 = S.put(_{placed:K, last:Now, frontier:FLen}),
        asserta(pl_progress_state(S1)),
        eta_line(S1),
        jlog(_{event:placed, k:K, n:N, rect:Rect, frontier:FLen})
    ; E = evac(start, Info) ->
        maybe_verbose('evac check: ~p~n', [Info]),
        jlog(_{event:evac_start, info:Info})
    ; E = evac(done, Res) ->
        maybe_verbose('evac result: ~w~n', [Res]),
        jlog(_{event:evac_done, result:Res})
    ; E = finish(Status, K) ->
        say('~n[finish] ~w, placed=~w~n', [Status,K]),
        jlog(_{event:finish, status:Status, placed:K})
    ; true ).

say(Fmt, Args) :- format(user_error, Fmt, Args).

maybe_verbose(Fmt, Args) :-
    ( pl_progress_mode(verbose) -> say(Fmt, Args) ; true ).

eta_line(S) :-
    S.get(t0) = T0, get_time(Now), Dt is max(0.001, Now-T0),
    K is S.get(placed), N is S.get(total),
    Rate is K / Dt, Rem is max(0,N-K),
    ( Rate =:= 0 -> Eta = inf ; Eta is Rem / Rate ),
    format(user_error, '\r[~w/~w] frontier=~w, rate=~2f/s, ETA=~1f s',
           [K,N,S.get(frontier), Rate, Eta]).

jlog(Dict) :-
    ( pl_progress_stream(S) ->
        json_write_dict(S, Dict, [width(0)]), nl(S)
    ; true ).
    
:- dynamic current_plan_module/1.

% -------- политики по умолчанию (можно переопределять в самом плане через zones:policy/2)
default_grid(100).
default_window_clear_depth_mm(1000).

% === ПУБЛИЧНОЕ АПИ =====================================================

load_plan(File) :-
    absolute_file_name(File, Abs, [access(read)]),
    load_files(Abs, [silent(true)]),
    detect_plan_module(Abs, M),
    retractall(current_plan_module(_)),
    asserta(current_plan_module(M)),
    apply_plan.

apply_plan :-
    current_plan_module(M),
    clear_plan,
    map_room(M),
    map_objects(M).

clear_plan :-
    retractall(rects:room_size(_,_)),
    retractall(zones:door(_,_,_,_)),
    retractall(zones:e_panel(_,_,_)),
    retractall(zones:net_cab(_,_,_,_)),
    retractall(zones:forbidden_zone(_,_,_)).
    % zones:policy/2 не чистим — их можно задавать в плане как zones:policy(...)

solve_plan(Rects, Oris, Grid) :-
    ( zones:policy(grid_mm, Grid) -> true ; default_grid(Grid) ),
    current_plan_module(M),
    ( predicate_property(M:task(_,_), defined)
    -> clause(M:task(Count, size(W,H)), true),
       layout:layout_desks_opts(Count, W,H, Grid, [], Rects, Oris)
    ;  throw(error(existence_error(predicate, M:task/2), _))
    ).

plan_info :-
    ( current_plan_module(M) -> true ; M = none ),
    format('Plan module: ~w~n', [M]),
    ( rects:room_size(Wr,Hr)
    -> format('Room: ~dx~d mm~n', [Wr,Hr])
    ;  writeln('Room: <not set>')
    ),
    findall(_, zones:door(_,_,_,_), Doors), length(Doors, Dn),
    aggregate_all(count, zones:forbidden_zone(_,_,_), Zc),
    format('Doors: ~d~nZones(forbidden+generated): ~d~n', [Dn, Zc]).

% === ОПРЕДЕЛЕНИЕ МОДУЛЯ ФАЙЛА ==========================================

detect_plan_module(Abs, M) :-
    % 1) Если файл объявляет модуль — берём его
    ( module_property(M0, file(Abs)) ->
        M = M0
    % 2) Иначе пробуем найти модуль, где действительно есть room/1|static_object/4|task/2
    ; find_module_with_pred(room/1,         M1) -> M = M1
    ; find_module_with_pred(static_object/4, M2) -> M = M2
    ; find_module_with_pred(task/2,          M3) -> M = M3
    % 3) В крайнем случае — user (но дальше map_room выдаст понятную ошибку)
    ; M = user
    ).

find_module_with_pred(PI, M) :-
    functor(Head, PI, _),             % создать терм-«болванку» по PI
    current_module(M),
    predicate_property(M:Head, defined), !.

% === МЭППИНГ ROOM/OBJECTS ==============================================

map_room(M) :-
    ( predicate_property(M:room(_), defined)
    -> clause(M:room(size(W,H)), true),
       asserta(rects:room_size(W,H))
    ;  print_message(error, plan_loader(no_room_fact(M))),
       throw(error(existence_error(predicate, M:room/1), _))
    ).

map_objects(M) :-
    % если в плане нет ни одного static_object — просто пропускаем без ошибки
    ( predicate_property(M:static_object(_,_,_,_), defined)
    -> forall(clause(M:static_object(Id,Type,Rect,Props), true),
              map_object(Id, Type, Rect, Props))
    ;  true
    ).

% ---- диспетчер объектов ----

map_object(Id, door, Rect, _Props) :- !,
    door_rect_to_side_offset_width(Rect, Side, Off, Wd),
    asserta(zones:door(Id, side(Side), offset(Off), width(Wd))).

map_object(Id, window, Rect, _Props) :- !,
    window_clear_zone(Rect, Z),
    asserta(zones:forbidden_zone(Id, window_clear, Z)).

map_object(Id, column, Rect, _Props) :- !,
    asserta(zones:forbidden_zone(Id, struct, Rect)).

map_object(Id, cabinet, Rect, _Props) :- !,
    asserta(zones:forbidden_zone(Id, struct, Rect)).

map_object(Id, electrical_shield, Rect, _Props) :- !,
    asserta(zones:forbidden_zone(Id, struct, Rect)),
    panel_service_zone(Rect, Z),
    asserta(zones:forbidden_zone(service(Id), service, Z)).

map_object(Id, net_cabinet, Rect, _Props) :- !,
    asserta(zones:forbidden_zone(Id, struct, Rect)),
    netcab_service_zone(Rect, Z),
    asserta(zones:forbidden_zone(service(Id), service, Z)).

map_object(Id, fire_extinguisher, Rect, Props) :- !,
    asserta(zones:forbidden_zone(Id, marker_fe, Rect)),
    ( memberchk(type(_), Props) -> true ; true ).

map_object(Id, fire_alarm, Rect, _Props) :- !,
    asserta(zones:forbidden_zone(Id, marker_fa, Rect)).

map_object(Id, comms_block, Rect, Props) :- !,
    asserta(zones:forbidden_zone(Id, marker_comms, Rect)),
    ( memberchk(capacity(_), Props) -> true ; true ).

map_object(Id, Unknown, Rect, _Props) :-
    print_message(warning, plan_loader(unknown_type(Unknown, Id))),
    asserta(zones:forbidden_zone(Id, struct, Rect)).

% === УТИЛИТЫ ДЛЯ ОБЪЕКТОВ НА СТЕНЕ =====================================

door_rect_to_side_offset_width(rect(X,Y,W,H), Side, Off, Wd) :-
    rects:room_size(Wr,Hr),
    X2 is X+W, Y2 is Y+H,
    ( Y =:= 0   -> Side = north, Off = X,   Wd = W
    ; Y2 =:= Hr -> Side = south, Off = X,   Wd = W
    ; X =:= 0   -> Side = west,  Off = Y,   Wd = H
    ; X2 =:= Wr -> Side = east,  Off = Y,   Wd = H
    ; throw(error(domain_error(door_on_wall, rect(X,Y,W,H)), _))
    ).

window_clear_zone(rect(X,Y,W,H), rect(Xz,Yz,Wz,Hz)) :-
    rects:room_size(Wr,Hr),
    ( zones:policy(window_clear_depth_mm, D) -> true ; default_window_clear_depth_mm(D) ),
    X2 is X+W, Y2 is Y+H,
    ( Y =:= 0   -> Xz is X,     Yz is 0,     Wz is W,  Hz is D
    ; Y2 =:= Hr -> Xz is X,     Yz is Hr-D,  Wz is W,  Hz is D
    ; X =:= 0   -> Xz is 0,     Yz is Y,     Wz is D,  Hz is H
    ; X2 =:= Wr -> Xz is Wr-D,  Yz is Y,     Wz is D,  Hz is H
    ; fail
    ).

panel_service_zone(rect(X,Y,W,H), rect(Xz,Yz,Wz,Hz)) :-
    rects:room_size(Wr,Hr),
    zones:get_policy(panel_clear_size, size(Along,Depth)),
    X2 is X+W, Y2 is Y+H,
    ( Y =:= 0   -> Xz is X,     Yz is 0,        Wz is Along, Hz is Depth
    ; Y2 =:= Hr -> Xz is X,     Yz is Hr-Depth, Wz is Along, Hz is Depth
    ; X =:= 0   -> Xz is 0,     Yz is Y,        Wz is Depth, Hz is Along
    ; X2 =:= Wr -> Xz is Wr-Depth, Yz is Y,     Wz is Depth, Hz is Along
    ; fail
    ).

netcab_service_zone(rect(X,Y,W,H), rect(Xz,Yz,Wz,Hz)) :-
    rects:room_size(Wr,Hr),
    zones:get_policy(netcab_clear_depth, D),
    X2 is X+W, Y2 is Y+H,
    ( Y =:= 0   -> Xz is X,     Yz is 0,     Wz is W,  Hz is D
    ; Y2 =:= Hr -> Xz is X,     Yz is Hr-D,  Wz is W,  Hz is D
    ; X =:= 0   -> Xz is 0,     Yz is Y,     Wz is D,  Hz is H
    ; X2 =:= Wr -> Xz is Wr-D,  Yz is Y,     Wz is D,  Hz is H
    ; fail
    ).

% — сообщения —
:- multifile prolog:message//1.
prolog:message(plan_loader(unknown_type(T, Id))) -->
  [ 'plan_loader: unknown static_object type ~p (id=~p); treating as struct obstacle'-[T,Id] ].

/* ============================
   EXPORT: save solution to file
   ============================ */

% solve_plan_to_file(+OutFile).
% Формат определяется по расширению: .json -> JSON, иначе Prolog-факты.
solve_plan_to_file(OutFile) :-
    ( file_name_extension(_, json, OutFile) ->
        solve_plan_to_file(OutFile, json)
    ; solve_plan_to_file(OutFile, pl)
    ).

% solve_plan_to_file(+OutFile, +Format) where Format ∈ {json, pl}.
solve_plan_to_file(OutFile, Format) :-
    % решаем задачу
    solve_plan(RectsMM, Oris, Grid),
    % получаем базовый размер из task/2 (для удобства в метаданных)
    current_plan_module(M),
    ( clause(M:task(_Count, size(W0,H0)), true) -> true ; (W0=_, H0=_) ),
    % сводим в структуру "placed" (список dict’ов)
    placed_desks_dicts(RectsMM, Oris, Grid, W0,H0, Placed),
    % пишем в нужном формате
    ( Format == json ->
        write_solution_json(OutFile, Grid, Placed)
    ; write_solution_pl(OutFile, Grid, Placed)
    ).

% -------------------
% Build "placed" list
% -------------------

% placed_desks_dicts(+Rects,+Oris,+Grid,+W0,+H0,-PlacedListDicts)
% Placed = [ _{ id:"desk-1", type:desk, rect:_{x:...,y:...,w:...,h:...},
%               orientation:0, grid:100, base_size:_{w:W0,h:H0} }, ... ]
placed_desks_dicts(Rects, Oris, Grid, W0,H0, Placed) :-
    length(Rects, N),
    length(Oris,  N),        % страховка на совпадение длины
    numlist(1, N, Idxs),
    maplist(rect_ori_to_dict(Grid, W0,H0), Idxs, Rects, Oris, Placed).

rect_ori_to_dict(Grid, W0,H0, I, rect(X,Y,W,H), Ori, D) :-
    format(atom(Id), 'desk-~d', [I]),
    D = _{
        id: Id,
        type: desk,
        rect: _{x:X, y:Y, w:W, h:H},     % миллиметры (как во входном плане)
        orientation: Ori,                % 0 — как есть, 1 — повернут на 90°
        grid: Grid,
        base_size: _{w:W0, h:H0}         % исходный размер из task/2 (опционально)
    }.

% -------------------
% Writers
% -------------------

% JSON:
% {
%   "grid": 100,
%   "desks": [ {id,type,rect:{x,y,w,h},orientation,grid,base_size:{w,h}}, ... ]
% }
write_solution_json(File, Grid, Placed) :-
    open(File, write, S, [encoding(utf8)]),
    Sol = _{ grid: Grid, desks: Placed },
    json_write_dict(S, Sol, [width(0)]),
    nl(S),
    close(S).

% Prolog-факты:
% solution(grid(100)).
% placed_object('desk-1', desk, rect(X,Y,W,H), [orientation(0)]).
write_solution_pl(File, Grid, Placed) :-
    open(File, write, S, [encoding(utf8)]),
    format(S, '% generated by planner~n', []),
    format(S, 'solution(grid(~w)).~n~n', [Grid]),
    forall(member(D, Placed),
      ( _{id:Id, rect:Rect, orientation:Ori} :< D,
        Rect = _{x:X, y:Y, w:W, h:H},
        format(S, 'placed_object(~q, desk, rect(~w,~w,~w,~w), [orientation(~w)]).~n',
               [Id, X,Y,W,H, Ori])
      )),
    close(S).

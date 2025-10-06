:- module(zones,
    [ % data (multifile) you may define in plan files
      door/4,                  % door(Id, side(Side), offset(Off), width(W))
      e_panel/3,               % e_panel(Id, side(Side), offset(Off))
      net_cab/4,               % net_cab(Id, side(Side), offset(Off), width(W))
      forbidden_zone/3,        % forbidden_zone(Id, Type, rect(X,Y,W,H))
      policy/2,                % policy(Name, Value)  (optional overrides)

      % generators
      door_clear_zone/2,       % door_clear_zone(door(...), RectZone)
      panel_clear_zone/2,      % panel_clear_zone(e_panel(...), RectZone)
      netcab_clear_zone/2,     % netcab_clear_zone(net_cab(...), RectZone)

      % collection & checks
      collect_zones/1,         % collect_zones(Zones)
      no_overlap_with_zones/2, % no_overlap_with_zones(WorkRects, Zones)

      % helper
      pairs_between/3          % pairs_between(As, Bs, Pairs)
    ]).

:- use_module(library(clpfd)).
:- use_module(rects).   % reuse room_rect/1, rect_no_overlap/2, inside_room/2

% -----------------------------
% Multifile data & policies
% -----------------------------
:- multifile door/4.
:- multifile e_panel/3.
:- multifile net_cab/4.
:- multifile forbidden_zone/3.
:- multifile policy/2.


:- dynamic  door/4.
:- dynamic  e_panel/3.
:- dynamic  net_cab/4.
:- dynamic  forbidden_zone/3.
:- dynamic  policy/2.

% Default policies (overridable by providing your own policy/2 facts)
default_policy(door_clear_depth,   1200).
default_policy(panel_clear_size,   size(1000,1000)).
default_policy(netcab_clear_depth, 1000).
default_policy(min_aisle_width_mm, 600).   % <--- НОВОЕ
default_policy(tile_slide_step_mm,   100).     % шаг сдвига стола внутри клетки (мм)
default_policy(tiles_connectivity,   strict).  % strict | loose
default_policy(wall_clear_mm, 0).

get_policy(Name, Value) :-
    (   policy(Name, Value)   % user override (if any)
    ->  true
    ;   default_policy(Name, Value)
    ).

% -----------------------------
% Zone generators
% -----------------------------

% door_clear_zone(+Door, -RectZone)
% Door = door(Id, side(Side), offset(Off), width(Wd))
% Side ∈ {north,south,west,east}. Off along the wall: X for north/south, Y for west/east.
door_clear_zone(door(_Id, side(Side), offset(Off), width(Wd)), rect(X,Y,W,H)) :-
    rects:room_rect(rect(0,0,Wr,Hr)),
    get_policy(door_clear_depth, D),
    (   Side == north -> X #= Off, Y #= 0,      W #= Wd,  H #= D
    ;   Side == south -> X #= Off, Y #= Hr - D, W #= Wd,  H #= D
    ;   Side == west  -> X #= 0,   Y #= Off,    W #= D,   H #= Wd
    ;   Side == east  -> X #= Wr - D, Y #= Off, W #= D,   H #= Wd
    ),
    rects:inside_room(rect(0,0,Wr,Hr), rect(X,Y,W,H)).

% panel_clear_zone(+Panel, -RectZone)
% Panel = e_panel(Id, side(Side), offset(Off))
% Uses policy(panel_clear_size, size(Along,Depth)).
panel_clear_zone(e_panel(_Id, side(Side), offset(Off)), rect(X,Y,W,H)) :-
    rects:room_rect(rect(0,0,Wr,Hr)),
    get_policy(panel_clear_size, size(Along,Depth)),
    (   Side == north -> X #= Off, Y #= 0,        W #= Along, H #= Depth
    ;   Side == south -> X #= Off, Y #= Hr-Depth, W #= Along, H #= Depth
    ;   Side == west  -> X #= 0,   Y #= Off,      W #= Depth, H #= Along
    ;   Side == east  -> X #= Wr-Depth, Y #= Off, W #= Depth, H #= Along
    ),
    rects:inside_room(rect(0,0,Wr,Hr), rect(X,Y,W,H)).

% netcab_clear_zone(+Cab, -RectZone)
% Cab = net_cab(Id, side(Side), offset(Off), width(Wc))
% Uses policy(netcab_clear_depth, Depth). Width along wall is Wc.
netcab_clear_zone(net_cab(_Id, side(Side), offset(Off), width(Wc)), rect(X,Y,W,H)) :-
    rects:room_rect(rect(0,0,Wr,Hr)),
    get_policy(netcab_clear_depth, D),
    (   Side == north -> X #= Off, Y #= 0,      W #= Wc, H #= D
    ;   Side == south -> X #= Off, Y #= Hr - D, W #= Wc, H #= D
    ;   Side == west  -> X #= 0,   Y #= Off,    W #= D,  H #= Wc
    ;   Side == east  -> X #= Wr - D, Y #= Off, W #= D,  H #= Wc
    ),
    rects:inside_room(rect(0,0,Wr,Hr), rect(X,Y,W,H)).

% -----------------------------
% Collect & check
% -----------------------------

% pairs_between(As, Bs, Pairs)  -> [(A,B) | A∈As, B∈Bs]  (без findall/3)
pairs_between([], _, []).
pairs_between([A|As], Bs, Pairs) :-
    pairs_with(A, Bs, P1),
    pairs_between(As, Bs, P2),
    append(P1, P2, Pairs).

pairs_with(_, [], []).
pairs_with(A, [B|Bs], [(A,B)|Ps]) :-
    pairs_with(A, Bs, Ps).

% collect_zones(-Zones)
% Merge: explicit forbidden_zone/3 + generated door/panel/netcab zones.
collect_zones(Zones) :-
    % explicit zones (filter inside room to be safe)
    findall(Z,
            ( forbidden_zone(_Id,_Type,Z),
              rects:inside_room(Z)
            ),
            Zexp),
    % doors
    findall(Zd,
            ( door(Id, side(S), offset(Off), width(Wd)),
              door_clear_zone(door(Id,side(S),offset(Off),width(Wd)), Zd)
            ),
            ZdList),
    % panels
    findall(Zp,
            ( e_panel(Id, side(S), offset(Off)),
              panel_clear_zone(e_panel(Id,side(S),offset(Off)), Zp)
            ),
            ZpList),
    % net cabinets
    findall(Zn,
            ( net_cab(Id, side(S), offset(Off), width(Wc)),
              netcab_clear_zone(net_cab(Id,side(S),offset(Off),width(Wc)), Zn)
            ),
            ZnList),
    append(Zexp, ZdList, T0),
    append(T0, ZpList, T1),
    append(T1, ZnList, Zones).

% no_overlap_with_zones(+WorkRects, +Zones)
% Навешиваем ограничения без списков пар и копий переменных.
no_overlap_with_zones(WorkRects, Zones) :-
    maplist(constrain_rect_against_zones(Zones), WorkRects).

constrain_rect_against_zones(Zones, Rect) :-
    % Для каждого Zone навешиваем ограничение на исходный Rect
    maplist(rects:rect_no_overlap(Rect), Zones).

no_overlap_pair_z((A,B)) :-
    rects:rect_no_overlap(A,B).


% ....................................................
% ВМЕСТО старого блока begin_tests(zones_tests) ... end_tests
% ....................................................

:- begin_tests(zones_tests, [setup(load_test_facts), cleanup(unload_test_facts)]).
:- use_module(zones).
:- use_module(rects).
:- use_module(library(clpfd)).

% --- фикстуры: поднимаем перед всем сьютом, снимаем после ---

load_test_facts :-
    % room для генерации зон
    asserta(rects:room_size(6000,4000)),

    % двери
    asserta(zones:door(dn, side(north), offset(1000), width(900))),
    asserta(zones:door(ds, side(south), offset( 500), width(1000))),
    asserta(zones:door(dw, side(west),  offset(2000), width(1000))),
    asserta(zones:door(de, side(east),  offset(1000), width( 900))),

    % щиты
    asserta(zones:e_panel(epn, side(north), offset(3000))),
    asserta(zones:e_panel(epw, side(west),  offset(2500))),

    % телеком-шкафы
    asserta(zones:net_cab(nce, side(east),  offset(1500), width(800))),
    asserta(zones:net_cab(ncs, side(south), offset(4000), width(1200))),

    % явная запретная зона
    asserta(zones:forbidden_zone(fz1, custom, rect(3000, 0, 500, 500))).

unload_test_facts :-
    % снимаем РОВНО то, что поднимали
    retractall(rects:room_size(6000,4000)),
    retractall(zones:door(dn, side(north), offset(1000), width(900))),
    retractall(zones:door(ds, side(south), offset(500),  width(1000))),
    retractall(zones:door(dw, side(west),  offset(2000), width(1000))),
    retractall(zones:door(de, side(east),  offset(1000), width(900))),
    retractall(zones:e_panel(epn, side(north), offset(3000))),
    retractall(zones:e_panel(epw, side(west),  offset(2500))),
    retractall(zones:net_cab(nce, side(east),  offset(1500), width(800))),
    retractall(zones:net_cab(ncs, side(south), offset(4000), width(1200))),
    retractall(zones:forbidden_zone(fz1, custom, rect(3000, 0, 500, 500))).

% ---- door tests ----
test('door_clear_zone - north') :-
    once(( door_clear_zone(door(dn,side(north),offset(1000),width(900)), Z),
           Z == rect(1000, 0, 900, 1200) )).

test('door_clear_zone - west') :-
    once(( door_clear_zone(door(dw,side(west),offset(2000),width(1000)), Z),
           Z == rect(0, 2000, 1200, 1000) )).

% ---- panel tests ----
test('panel_clear_zone - north (1000x1000)') :-
    once(( panel_clear_zone(e_panel(epn,side(north),offset(3000)), Z),
           Z == rect(3000, 0, 1000, 1000) )).

test('panel_clear_zone - west (1000x1000)') :-
    once(( panel_clear_zone(e_panel(epw,side(west),offset(2500)), Z),
           Z == rect(0, 2500, 1000, 1000) )).

% ---- net cabinet tests ----
test('netcab_clear_zone - east (policy-aware)') :-
    once((
        rects:room_rect(rect(0,0,Wr,_Hr)),
        get_policy(netcab_clear_depth, D),
        netcab_clear_zone(net_cab(nce,side(east),offset(1500),width(800)), Z),
        Xe is Wr - D,
        Z == rect(Xe, 1500, D, 800)
    )).

test('netcab_clear_zone - south (policy-aware)') :-
    once((
        rects:room_rect(rect(0,0,_Wr,Hr)),
        get_policy(netcab_clear_depth, D),
        netcab_clear_zone(net_cab(ncs,side(south),offset(4000),width(1200)), Z),
        Ys is Hr - D,
        Z == rect(4000, Ys, 1200, D)
    )).


% ---- collection ----
test('collect_zones - includes explicit, doors, panels, netcabs') :-
    once((
        collect_zones(Zs),
        member(rect(3000,0,500,500), Zs),        % explicit
        member(rect(1000,0,900,1200), Zs),       % door north
        member(rect(0,2000,1200,1000), Zs),      % door west
        member(rect(3000,0,1000,1000), Zs),      % panel north
        member(rect(0,2500,1000,1000), Zs),      % panel west
        member(rect(5000,1500,1000,800), Zs),    % netcab east
        member(rect(4000,3000,1200,1000), Zs)    % netcab south
    )).

% ---- overlap checks ----
test('no_overlap_with_zones - touching allowed at netcab strip') :-
    % east netcab zone: rect(5000,1500,1000,800), bottom edge Y=2300
    once((
        Work = [rect(5200, 2300, 300, 200)],   % touches at Y=2300
        collect_zones(Zs),
        no_overlap_with_zones(Work, Zs)
    )).

test('no_overlap_with_zones - overlap forbidden with netcab strip', [fail]) :-
    Work = [rect(5200, 2299, 300, 200)],      % overlaps by 1 mm
    collect_zones(Zs),
    no_overlap_with_zones(Work, Zs).

% ---- integration ----
test('integration: place 4 desks avoiding all zones (grid 100)') :-
    once((
        rects:rects_domain_same_size(6000,4000, 1400,700, 4, Rects, Vars),
        rects:snap_to_grid(100, Rects),
        rects:all_inside_room(Rects),
        rects:all_no_overlap(Rects),
        collect_zones(Zs),
        no_overlap_with_zones(Rects, Zs),
        rects:label_rects([ffc], Vars)
    )).

:- end_tests(zones_tests).

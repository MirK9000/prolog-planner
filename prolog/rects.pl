:- module(rects,
    [ % geometry
      rect_valid/1,              % validate rectangle dimensions
      rect_edges/5,              % compute L,R,T,B
      rect_no_overlap/2,         % NO overlap (touching is allowed)
      rect_overlap_strict/2,     % strict overlap (touching is NOT overlap)

      % room & containment
      room_rect/1,               % room rectangle from room_size/2
      inside_room/1,             % inside current room (room_size/2)
      inside_room/2,             % inside explicit room rectangle
      inside_room_strict/2,      % strictly inside (no touching)

      % list helpers
      pairs/2,
      all_inside_room/1, all_inside_room/2,
      all_no_overlap/1,

      % Variant A: domains & labeling utilities
      rect_domain/5,             % rect_domain(RoomW,RoomH,W,H, Rect)
      rects_domain_same_size/7,  % rects_domain_same_size(RoomW,RoomH,W,H,N, Rects, Vars)
      rect_vars/2,               % rect_vars(Rects, Vars)
      label_rects/2,             % label_rects(Opts, Vars)
      snap_to_grid/2,            % snap_to_grid(Grid, Rects)

      % Orientation (90° rotate)
      rect_with_orientation/6,         % rect_with_orientation(W0,H0, Ori, X,Y, Rect)
      rect_domain_oriented/6,          % rect_domain_oriented(RoomW,RoomH,W0,H0, Ori, Rect)
      rects_domain_same_size_oriented/8% rects_domain_same_size_oriented(RoomW,RoomH,W0,H0,N, Rects, Oris, Vars)
    ]).

:- use_module(library(clpfd)).

% ----------------------------------------------------------------------
%   Format: rect(X, Y, W, H)
%   Units: any (mm/cm), but be consistent.
%   Coordinates: (0,0) is top-left, X->right, Y->down.
% ----------------------------------------------------------------------

% 1) Positive sizes only.
rect_valid(rect(_X,_Y,W,H)) :-
    W #> 0, H #> 0.

% 2) Useful edges of a rectangle.
%    L — left, R — right, T — top, B — bottom.
rect_edges(rect(X,Y,W,H), L,R,T,B) :-
    L #= X,
    R #= X + W,
    T #= Y,
    B #= Y + H.

% 3) NO overlap (touching allowed).
%    Equivalent: at least one of A-left-of-B / right-of / above / below.
rect_no_overlap(A,B) :-
    rect_valid(A), rect_valid(B),
    rect_edges(A, L1,R1,T1,B1),
    rect_edges(B, L2,R2,T2,B2),
    R1 #=< L2 #\/
    R2 #=< L1 #\/
    B1 #=< T2 #\/
    B2 #=< T1.

% 4) STRICT overlap (area > 0). Touching is NOT overlap.
rect_overlap_strict(A,B) :-
    rect_valid(A), rect_valid(B),
    rect_edges(A, L1,R1,T1,B1),
    rect_edges(B, L2,R2,T2,B2),
    L1 #< R2, L2 #< R1,  % overlap on X
    T1 #< B2, T2 #< B1.  % and on Y


% =======================
% Room and "inside" checks
% =======================

:- multifile room_size/2.
:- dynamic  room_size/2.
% Define room_size/2 in plan data, e.g.:
% room_size(6000, 4000).

room_rect(rect(0,0,W,H)) :-
    room_size(W,H),
    W #> 0, H #> 0.

% inside_room/1 — uses global room_size/2.
% Touching walls is allowed (<=).
inside_room(Rect) :-
    room_rect(Room),
    inside_room(Room, Rect).

% inside_room/2 — explicit room rectangle (need not start at 0,0).
% Touching walls is allowed.
inside_room(RoomRect, Rect) :-
    rect_valid(RoomRect),
    rect_valid(Rect),
    rect_edges(RoomRect, Lr,Rr,Tr,Br),
    rect_edges(Rect,     L, R, T, B),
    L  #>= Lr,
    T  #>= Tr,
    R  #=< Rr,
    B  #=< Br.

% Strictly inside (no touching).
inside_room_strict(RoomRect, Rect) :-
    rect_valid(RoomRect),
    rect_valid(Rect),
    rect_edges(RoomRect, Lr,Rr,Tr,Br),
    rect_edges(Rect,     L, R, T, B),
    L  #> Lr,
    T  #> Tr,
    R  #< Rr,
    B  #< Br.


% =======================
% Bulk helpers for lists
% =======================

% Generate all unordered pairs from a list: [a,b,c] -> [(a,b),(a,c),(b,c)]
pairs([], []).
pairs([_], []).
pairs([H|T], Pairs) :-
    make_pairs(H, T, P1),
    pairs(T, P2),
    append(P1, P2, Pairs).

make_pairs(_, [], []).
make_pairs(A, [B|Bs], [(A,B)|Ps]) :-
    make_pairs(A, Bs, Ps).

% all_inside_room/1 — uses global room_size/2
all_inside_room(Rects) :-
    maplist(inside_room, Rects).

% all_inside_room/2 — explicit room rectangle
all_inside_room(RoomRect, Rects) :-
    maplist(inside_room(RoomRect), Rects).

% all_no_overlap/1 — every pair does NOT overlap (touching allowed)
all_no_overlap(Rects) :-
    pairs(Rects, Ps),
    maplist(no_overlap_pair, Ps).

no_overlap_pair((A,B)) :- rect_no_overlap(A,B).


% =======================
% Variant A: domains & labeling utils
% =======================

% rect_domain(RoomW,RoomH,W,H, rect(X,Y,W,H))
% Domains assuming room at (0,0). Safe even if W/H close to room size.
rect_domain(RoomW,RoomH,W,H, rect(X,Y,W,H)) :-
    % sanity
    RoomW #> 0, RoomH #> 0, W #> 0, H #> 0,
    W #=< RoomW, H #=< RoomH,
    % domains + bounds
    X in 0..RoomW,
    Y in 0..RoomH,
    X + W #=< RoomW,
    Y + H #=< RoomH.

% rects_domain_same_size(RoomW,RoomH,W,H,N, Rects, Vars)
% Create N rects of the same size, set domains, collect Vars = [X1,Y1,...].
rects_domain_same_size(RoomW,RoomH,W,H,N, Rects, Vars) :-
    N #>= 0,
    length(Rects, N),
    maplist({RoomW,RoomH,W,H}/[R]>>rect_domain(RoomW,RoomH,W,H,R), Rects),
    rect_vars(Rects, Vars).

% rect_vars(Rects, Vars) — flatten [rect(X,Y,_,_)…] -> [X,Y,...]
rect_vars([], []).
rect_vars([rect(X,Y,_,_)|Rs], [X,Y|Vs]) :-
    rect_vars(Rs, Vs).

% label_rects(Opts, Vars) — thin wrapper, keep it centralized.
label_rects(Opts, Vars) :-
    labeling(Opts, Vars).

% snap_to_grid(Grid, Rects) — force X,Y to lie on Grid (e.g., 100 mm)
snap_to_grid(Grid, Rects) :-
    Grid #> 0,
    maplist(snap_rect(Grid), Rects).

snap_rect(Grid, rect(X,Y,_,_)) :-
    X mod Grid #= 0,
    Y mod Grid #= 0.


% =======================
% Orientation (90° rotate)
% =======================

% rect_with_orientation(W0,H0, Ori, X,Y, Rect)
% Ori ∈ {0,1}: 0 -> (W,H)=(W0,H0); 1 -> (W,H)=(H0,W0).
% Implemented linearly to keep CLPFD efficient:
%   W = W0 + Ori*(H0 - W0)
%   H = H0 + Ori*(W0 - H0)
rect_with_orientation(W0,H0, Ori, X,Y, rect(X,Y,W,H)) :-
    W0 #> 0, H0 #> 0,
    Ori in 0..1,
    W #= W0 + Ori*(H0 - W0),
    H #= H0 + Ori*(W0 - H0).

% rect_domain_oriented(RoomW,RoomH,W0,H0, Ori, Rect)
% Domains for rect with orientation variable.
rect_domain_oriented(RoomW,RoomH,W0,H0, Ori, Rect) :-
    rect_with_orientation(W0,H0, Ori, _X, _Y, Rect),
    rect_domain(RoomW,RoomH, _,_, Rect).

% rects_domain_same_size_oriented(RoomW,RoomH,W0,H0,N, Rects, Oris, Vars)
% Create N oriented rects of same base size; collect orientation vars and [X,Y,...].
rects_domain_same_size_oriented(RoomW,RoomH,W0,H0,N, Rects, Oris, Vars) :-
    N #>= 0,
    length(Rects, N),
    length(Oris,  N),
    maplist({RoomW,RoomH,W0,H0}/[Ori,R]>>rect_domain_oriented(RoomW,RoomH,W0,H0, Ori, R),
            Oris, Rects),
    rect_vars(Rects, Vars).


/* =======================
   TESTS: rectangle overlap
   ======================= */
:- begin_tests(rects_tests).
:- use_module(rects).
:- use_module(library(clpfd)).

test('touching right edge - allowed') :-
    A = rect(0,0,10,10),
    B = rect(10,0,10,10),
    once(rect_no_overlap(A,B)),
    \+ rect_overlap_strict(A,B).

test('overlap by 1 unit - forbidden') :-
    A = rect(0,0,10,10),
    B = rect(9,0,10,10),
    \+ rect_no_overlap(A,B),
    rect_overlap_strict(A,B).

test('identical rectangles - forbidden') :-
    A = rect(5,5,10,10),
    B = rect(5,5,10,10),
    \+ rect_no_overlap(A,B),
    rect_overlap_strict(A,B).

test('far apart - allowed') :-
    A = rect(0,0,10,10),
    B = rect(30,40,5,5),
    once(rect_no_overlap(A,B)),
    \+ rect_overlap_strict(A,B).

test('CLPFD variables - deterministic') :-
    A = rect(X,0,10,10),
    B = rect(25,0,10,10),
    X in 0..100,
    rect_no_overlap(A,B),
    once(labeling([], [X])).
:- end_tests(rects_tests).


/* =======================
   TESTS: inside room (explicit Room)
   ======================= */
:- begin_tests(inside_room_tests).
:- use_module(rects).
:- use_module(library(clpfd)).

test('fully inside - true') :-
    Room = rect(0,0,100,80),
    once(inside_room(Room, rect(10, 10, 20, 20))).

test('touching left wall - true') :-
    Room = rect(0,0,100,80),
    once(inside_room(Room, rect(0, 10, 30, 30))).

test('touching right & bottom walls - true') :-
    Room = rect(0,0,100,80),
    once(inside_room(Room, rect(60, 60, 40, 20))).

test('spills to the right - fail', [fail]) :-
    Room = rect(0,0,100,80),
    inside_room(Room, rect(70, 10, 40, 10)).

test('spills above - fail', [fail]) :-
    Room = rect(0,0,100,80),
    inside_room(Room, rect(-1, 10, 10, 10)).

test('room not at (0,0): explicit version - true') :-
    Room = rect(5,5,90,70),
    once(inside_room(Room, rect(5,5,10,10))).

test('strictly inside (no touching) - fail on touching', [fail]) :-
    Room = rect(0,0,100,80),
    inside_room_strict(Room, rect(0,0,10,10)).
:- end_tests(inside_room_tests).


/* =======================
   TESTS: bulk helpers
   ======================= */
:- begin_tests(bulk_helpers_tests).
:- use_module(rects).
:- use_module(library(clpfd)).

test('all_inside_room/2 - all OK') :-
    Room = rect(0,0,100,80),
    Rs = [rect(0,0,10,10), rect(50,10,20,20), rect(70,60,30,20)],
    once(all_inside_room(Room, Rs)).

test('all_inside_room/2 - one spills -> fail', [fail]) :-
    Room = rect(0,0,100,80),
    Rs = [rect(0,0,10,10), rect(90,70,20,20)],
    all_inside_room(Room, Rs).

test('all_no_overlap/1 - touching allowed') :-
    Rs = [rect(0,0,10,10), rect(10,0,10,10), rect(0,20,5,5)],
    once(all_no_overlap(Rs)).

test('all_no_overlap/1 - overlap -> fail', [fail]) :-
    Rs = [rect(0,0,10,10), rect(9,0,10,10)],
    all_no_overlap(Rs).

test('CLPFD - 3 rects placed inside & non-overlapping') :-
    Room = rect(0,0,100,80),
    once((
        X1 in 0..100, X2 in 0..100, X3 in 0..100,
        Y1 #= 0,      Y2 #= 0,      Y3 #= 0,
        Rs = [rect(X1,Y1,30,10), rect(X2,Y2,30,10), rect(X3,Y3,30,10)],
        all_inside_room(Room, Rs),
        all_no_overlap(Rs),
        labeling([], [X1,X2,X3])
    )).

test('CLPFD - push X3 to the right with [down]') :-
    Room = rect(0,0,100,80),
    once((
        X1 in 0..100, X2 in 0..100, X3 in 0..100,
        Rs = [rect(X1,0,30,10), rect(X2,0,30,10), rect(X3,0,30,10)],
        all_inside_room(Room, Rs),
        all_no_overlap(Rs),
        labeling([down], [X3]),
        labeling([], [X1,X2]),
        X3 #= 70
    )).
:- end_tests(bulk_helpers_tests).


/* =======================
   TESTS: Variant A utilities
   ======================= */
:- begin_tests(variant_a_utils_tests).
:- use_module(rects).
:- use_module(library(clpfd)).

test('rect_domain - bounds, choose max values with [down]', [true(X==70), true(Y==70)]) :-
    once((
        rect_domain(100,80, 30,10, rect(X,Y,30,10)),
        label_rects([down], [X,Y])
    )).

test('rects_domain_same_size + snap_to_grid(100) + no overlap in custom room') :-
    once((
        rects_domain_same_size(400,80, 30,10, 3, Rects, Vars),
        snap_to_grid(100, Rects),
        all_inside_room(rect(0,0,400,80), Rects),
        all_no_overlap(Rects),
        label_rects([], Vars)
    )).

test('rect_vars - order is [X1,Y1,X2,Y2,...]') :-
    Rects = [rect(X1,Y1,10,10), rect(X2,Y2,10,10), rect(X3,Y3,10,10)],
    rect_vars(Rects, Vars),
    Vars == [X1,Y1,X2,Y2,X3,Y3].

test('label_rects - push last X to the right with [down]', [true(X3==300)]) :-
    once((
        rects_domain_same_size(400,80, 30,10, 3, Rects, Vars),
        snap_to_grid(100, Rects),
        all_inside_room(rect(0,0,400,80), Rects),
        all_no_overlap(Rects),
        Rects = [_,_,rect(X3,_,_,_)],
        label_rects([down], [X3]),
        label_rects([], Vars)
    )).
:- end_tests(variant_a_utils_tests).


/* =======================
   TESTS: Orientation utilities
   ======================= */
:- begin_tests(orientation_tests).
:- use_module(rects).
:- use_module(library(clpfd)).

test('rect_with_orientation - ori 0 and 1 swap sizes') :-
    once((
        rect_with_orientation(30,10, 0, 0,0, R0),
        R0 = rect(_,_,W0,H0),
        W0 #= 30, H0 #= 10,
        rect_with_orientation(30,10, 1, 0,0, R1),
        R1 = rect(_,_,W1,H1),
        W1 #= 10, H1 #= 30
    )).

test('rect_domain_oriented - two desks touch but do not overlap') :-
    Room = rect(0,0,100,80),
    R0 = rect(X0,Y0,_,_),
    R1 = rect(X1,Y1,_,_),
    once((
        rect_domain_oriented(100,80, 30,10, 0, R0), % 30x10
        rect_domain_oriented(100,80, 30,10, 1, R1), % 10x30
        X0 #= 0,  Y0 #= 0,
        X1 #= 30, Y1 #= 0,          % left edge = 30: touching allowed
        all_inside_room(Room, [R0,R1]),
        all_no_overlap([R0,R1]),
        label_rects([], [X0,Y0,X1,Y1])
    )).

test('rects_domain_same_size_oriented - 3 desks fit (some may rotate)') :-
    Room = rect(0,0,100,80),
    once((
        rects_domain_same_size_oriented(100,80, 30,10, 3, Rects, Oris, Vars),
        all_inside_room(Room, Rects),
        all_no_overlap(Rects),
        label_rects([], Vars),
        Oris ins 0..1
    )).
:- end_tests(orientation_tests).

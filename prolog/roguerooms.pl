:- module(roguerooms, [dungeon_graph/2]).
/** <Module> Collection of tools for making level graphs
 *
 */


:- use_module(library(chr)).
:- use_module(library(ugraphs)).

:- set_prolog_flag(chr_toplevel_show_store, false).

:- chr_constraint
    room/1,
    needs_cons/2,
    all_rooms_connected/0,
    room_total/0,
    room_pick_from/1,
    remaining_picks/1,
    conn/2,
    may_need_connex/1,
    collect_conns/1,
    reset_store/0.


%!  dungeon_graph(-Dungeon:list, +Options:list) is nondet
%
%   Generates an infinite number of dungeons on backtracking
%
%   @arg Dungeon a dungeon representation  TODO
%   @arg Options
%    * size(Min-Max)
%      min and max number of rooms to make
%    * connections(List)
%      a list of integers. # of connections to a room will
%      be picked from this list randomly. Occasionally a room will
%      have more connections than the largest # in this set

dungeon_graph(Dungeon, Options) :-
    option(size(MinRooms-MaxRooms), Options, 2-8),
    option(connections(ConnList), Options, [1,2,2,3,3,3,4]),
    MinRooms >= 2,
    MaxRooms >= MinRooms,
    repeat,
    random_between(MinRooms, MaxRooms, NumRooms),
    setof(X, between(1,NumRooms, X), RoomNumList),
    reset_store,
    room_pick_from(RoomNumList),
    remaining_picks([]),
    maplist(add_room(ConnList), RoomNumList),
    room_total,
    all_rooms_connected,
    collect_conns(Dungeon).

add_room(ConnList, RoomNum) :-
    room(RoomNum),
    random_member(Conns, ConnList),
    needs_cons(RoomNum, Conns).

reset_store \ room(_) <=> true.
reset_store \ needs_cons(_, _) <=> true.
reset_store \ all_rooms_connected <=> true.
reset_store \     room_total <=> true.
reset_store \     room_pick_from(_) <=> true.
reset_store \     remaining_picks(_) <=> true.
reset_store \     conn(_, _) <=> true.
reset_store \     may_need_connex(_) <=> true.
reset_store \     collect_conns(_) <=> true.
reset_store <=> true.


needs_cons(_, 0) <=> true.

needs_cons(Room, X), needs_cons(Room, Y) ==> X \= Y | fail.
room(Room) \ room(Room) <=> true.
room(Room) ==> may_need_connex(Room).

conn(A,A) <=> true.
conn(A,B) \ conn(B,A) <=> true.
conn(A,B) \ conn(A,B) <=> true.


room_pick_from(R) \ remaining_picks([_]) <=>
             random_permutation(R, PR),
             remaining_picks(PR).
room_pick_from(R) \ remaining_picks([]) <=>
             random_permutation(R, PR),
             remaining_picks(PR).


room_total \ needs_cons(Room1, R1C), needs_cons(Room2, R2C), remaining_picks([Room1, Room2 | Rest]) <=>
             succ(NR1C, R1C),
             succ(NR2C, R2C),
             conn(Room1, Room2),
             needs_cons(Room1, NR1C),
             needs_cons(Room2, NR2C),
             remaining_picks(Rest).

room_total <=> true.

conn(Room1, _) \ may_need_connex(Room1) <=> true.
conn(_, Room2) \ may_need_connex(Room2) <=> true.

all_rooms_connected, may_need_connex(_) ==> fail.
all_rooms_connected <=> true.

collect_conns(L), conn(A,B) <=>
          L = [A-B | L1],
          collect_conns(L1).
collect_conns(L) <=> L = [].








/*
zebra.pl: Based on the classic zebra puzzle
(C) Pieter Valcke, 2003-2004
(C) Tom.Schrijvers at cs.kuleuven.be, 2003-2004
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Solves the classic "zebra puzzle" a.k.a. "Einstein's Riddle" 
(cf. http://en.wikipedia.org/wiki/Zebra_Puzzle):
1.   The Englishman lives in the red house.
2.   The Spaniard owns the dog.
3.   Coffee is drunk in the green house.
4.   The Ukrainian drinks tea.
5.   The green house is immediately to the right of the ivory house.
6.   The Porsche driver owns snails.
7.   The Masserati is driven by the man who lives in the yellow house.
8.   Milk is drunk in the middle house.
9.   The Norwegian lives in the first house on the left.
10.  The man who drives a Saab lives in the house next to the man
     with the fox.
11.  The Masserati is driven by the man in the house next to the
     house where the horse is kept.
12.  The Honda driver drinks orange juice.
13.  The Japanese drives a Jaguar.
14.  The Norwegian lives next to the blue house.

%% HOW TO USE
zebra(S) returns the solution in a 5x5-matrix, one row for each person:
S = [ [HouseColor1, Nationality1, Car1, Drink1, Pet1],
      [ ... ], ... ]

%% SAMPLE QUERIES
Q: zebra(S).
A: S = [[yellow,norwegian,masserati,water,fox],
        [blue,ukranian,saab,tea,horse],
        [red,english,porsche,milk,snails],
        [ivory,spanish,honda,orange,dog],
        [green,japanese,jaguar,coffee,zebra]].
*/

:- module(zebra,[zebra/1]).
:- use_module(library(chr)).
:- use_module(library(lists)).

%% Deprecated syntax used for SICStus 3.x
%handler zebra.
%constraints domain/2, diff/2.

%% Syntax for SWI / SICStus 4.x
:- chr_constraint domain/2, diff/2.


domain(_,[]) <=> fail.
domain(X,[V]) <=> X = V.
domain(X,L) <=> nonvar(X) | member(X,L).

diff(X,Y) <=> nonvar(X), nonvar(Y) | X \== Y.
diff(Y,X) \ domain(X,L) <=> nonvar(Y), select(Y,L,NL) | domain(X,NL).
diff(X,Y) \ domain(X,L) <=> nonvar(Y), select(Y,L,NL) | domain(X,NL).

% all_different(L,E) enforces that each element of L is different to E
all_different([],_).
all_different([H|T],E) :-
    diff(H,E),
    all_different(T,E).

% make_domain(L,D) enforces for each element E of L:
%   E is from domain D and different to each other element from L
make_domain([],_).
make_domain([X|L],D) :- all_different(L,X), domain(X,D), make_domain(L,D).

% left_right(L, R, X) is true when L is to the immediate left of R in list X
left_right(L, R, List) :- append(_, [L,R|_], List).

% next_to(X, Y, L) is true when X and Y are next to each other in list L
next_to(X, Y, L) :- left_right(X, Y, L).
next_to(X, Y, L) :- left_right(Y, X, L).

% solves the zebra puzzle and returns solution in S
zebra(S) :-
    [ [ ACo, AN, ACa, AD, AP ],
      [ BCo, BN, BCa, BD, BP ],
      [ CCo, CN, CCa, CD, CP ],
      [ DCo, DN, DCa, DD, DP ],
      [ ECo, EN, ECa, ED, EP ] ] = S,
    make_domain([ACo,BCo,CCo,DCo,ECo], [red,green,ivory,yellow,blue]),
    make_domain([AN, BN, CN, DN, EN ], 
                               [english,spanish,ukranian,norwegian,japanese]),
    make_domain([ACa,BCa,CCa,DCa,ECa], [porsche,masserati,saab,honda,jaguar]),
    make_domain([AD, BD, CD, DD, ED ], [coffee,tea,milk,orange,water]),
    make_domain([AP, BP, CP, DP, EP ], [dog,snails,fox,horse,zebra]),
        [_,_,[_,_,_,milk,_],_,_]                     = S, % clue 8
        [[_,norwegian,_,_,_],_,_,_,_]                = S, % clue 9
        member( [green,_,_,coffee,_],                 S), % clue 3
        member( [red,english,_,_,_],                  S), % clue 1
        member( [_,ukranian,_,tea,_],                 S), % clue 4
        member( [yellow,_,masserati,_,_],             S), % clue 7
        member( [_,_,honda,orange,_],                 S), % clue 12
        member( [_,japanese,jaguar,_,_],              S), % clue 13
        member( [_,spanish,_,_,dog],                  S), % clue 2
        member( [_,_,porsche,_,snails],               S), % clue 6
        left_right( [ivory,_,_,_,_], [green,_,_,_,_], S), % clue 5
        next_to( [_,norwegian,_,_,_],[blue,_,_,_,_],  S), % clue 14
        next_to( [_,_,masserati,_,_],[_,_,_,_,horse], S), % clue 11
        next_to( [_,_,saab,_,_],     [_,_,_,_,fox],   S). % clue 10

/*
mergesort.pl: Mergesort
(C) Thom.Fruehwirth at uni-ulm.de, 2004/07/15
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Mergesorts individual numbers into a sorted chain. 
Time-complexity is O(n*log(n)) (n numbers to sort).

%% HOW TO USE
For numbers X1, .., Xn, you enter
  sort(X1), .., sort(Xn),
and get them sorted with duplicates removed (S1 < S2 < .. < Sm) as
  S1 <<< S2, .., S(m-1) <<< Sm.

%% SAMPLE QUERIES
Q: sort(6), sort(2), sort(1), sort(4), sort(1).
A: 1 <<< 2, 2 <<< 4, 4 <<< 6,  merge(0,1),merge(2,1).

Q: sort(1.4), sort(-3), sort(3.3), sort(3.3), sort(0).
A: -3 <<< 0, 0 <<< 1.4, 1.4 <<< 3.3,  merge(2,-3),merge(0,3.3).
*/

:- module(mergesort, [(<<<)/2, merge/2, sort/1, test/0]).
:- use_module(library(chr)).

option(debug,off).
option(optimize,full).
option(check_guard_bindings,off).

:- op(700, xfx, '<<<').

%% Deprecated syntax used for SICStus 3.x
%handler mergesort.
constraints (<<<)/2, merge/2, sort/1.

%% Syntax for SWI / SICStus 4.x
%:- chr_constraint 
%    (<<<)/2, merge/2, sort/1.


sort    @ X <<< A \ X <<< B      <=> A<B | A <<< B.
merge   @ merge(N,A), merge(N,B) <=> A<B | M is N+1, merge(M,A), A <<< B.

test :- merge(0,27),merge(0,74),merge(0,17),merge(0,33),merge(0,94),merge(0,18),merge(0,46),merge(0,83),merge(0,65),merge(0,2),merge(0,32),merge(0,53),merge(0,28),merge(0,85),merge(0,99),merge(0,47),merge(0,28),merge(0,82),merge(0,6),merge(0,11),merge(0,55),merge(0,29),merge(0,39),merge(0,81),merge(0,90),merge(0,37),merge(0,10),merge(0,0),merge(0,66),merge(0,51),merge(0,7),merge(0,21).

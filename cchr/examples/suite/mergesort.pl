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

:- module(mergesort, [(<<<)/2, merge/2, sort/1]).
:- use_module(library(chr)).

:- op(700, xfx, '<<<').

%% Deprecated syntax used for SICStus 3.x
%handler mergesort.
%constraints
%    (<<<)/2, merge/2, sort/1.

%% Syntax for SWI / SICStus 4.x
:- chr_constraint 
    (<<<)/2, merge/2, sort/1.


start   @ sort(X)                <=> merge(0,X).
dupl    @ A <<< B \ A <<< B      <=> true.
sort    @ X <<< A \ X <<< B      <=> A<B | A <<< B.
merge   @ merge(N,A), merge(N,B) <=> A<B | M is N+1, merge(M,A), A <<< B.

/*
sorting.pl: Numbers to sorted chain
(C) Thom.Fruehwirth at uni-ulm.de, 2004/07/15
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Constructs a sorted chain from single values using ordered merge. 
Time-complexity is O(n^2) (n numbers to sort).

%% HOW TO USE
For positive numbers X1, .., Xn, you enter
  sort(X1), .., sort(Xn),
and get them sorted with duplicates removed (S1 < S2 < .. < Sm) as
  0 <<< S1, S1 <<< S2, .., S(m-1) <<< Sm.

%% SAMPLE QUERIES
Q: sort(6), sort(2), sort(1), sort(4), sort(1).
A: 0 <<< 1, 1 <<< 2, 2 <<< 4, 4 <<< 6.

Q: sort(1.4), sort(33), sort(3.3), sort(3.3), sort(0.3).
A: 0.3<<<1.4, 1.4<<<3.3, 3.3<<<33, 0<<<0.3.
*/

:- module(sorting, [(<<<)/2, sort/1]).
:- use_module(library(chr)).

:- op(700, xfx, '<<<').

%% Deprecated syntax used for SICStus 3.x
%handler sorting.
%constraints (<<<)/2, sort/1.

%% Syntax for SWI / SICStus 4.x
:- chr_constraint 
    (<<<)/2, 
    sort(+number).


start   @ sort(X) <=> 0 <<< X.
trivial @ A <<< A <=> true.
sort    @ A <<< B \ A <<< C <=> A<B, B=<C | B <<< C.

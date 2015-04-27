/*
ordered_merge.pl: Merge and sort chains
(C) Thom.Fruehwirth at uni-ulm.de, 2004/07/15
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
One-rule program to merge several sorted chains, resulting in one sorted 
chain.

%% HOW TO USE
Sorted chains are represented as (A1 < A2 < .. < An)
  A1 <<< A2, A2 <<< A3, ... A(n-1) <<< An.
Several chains are merged and sorted, provided they start with the same value.

%% SEE ALSO
[optional references, related programs,...]

%% SAMPLE QUERIES
Q: 0 <<< 2,  0 <<< 5.
A: 0 <<< 2, 2 <<< 5.

Q: 0 <<< 2, 2 <<< 5,  0 <<< 3, 3 <<< 7.
A: 0 <<< 2, 2 <<< 3, 3 <<< 5, 5 <<< 7.

Q: 1 <<< 3, 3 <<< 5,  1 <<< 4, 4 <<< 5.
A: 1 <<< 3, 3 <<< 4, 4 <<< 5, 4 <<< 5.
*/

:- module(ordered_merge, [(<<<)/2]).
:- use_module(library(chr)).

:- op(700, xfx, '<<<').

%% Deprecated syntax used for SICStus 3.x
%handler ordered_merge.
%constraints (<<<)/2.

%% Syntax for SWI / SICStus 4.x
:- chr_constraint  (<<<)/2.


sort    @ A <<< B \ A <<< C <=> A<B, B<C | B <<< C.

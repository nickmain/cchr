/*
gcd.pl: Greatest Common Divisor
(C) Thom.Fruehwirth at uni-ulm.de, 1998/02/02, 1998/03/11, LMU
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Computes greatest common divisor by Euclidean algorithm.

%% HOW TO USE
For two or more positive integers (N1,N2,..), you enter
  gcd(N1), gcd(N2), ...
and get the greatest common divisor X as single remaining
  gcd(X).

%% SAMPLE QUERIES
Q: gcd(2), gcd(3).
A: gcd(1).

Q: gcd(12), gcd(27).
A: gcd(3).

Q: gcd(94017), gcd(1155), gcd(2035).
A: gcd(11).
*/

:- module(gcd, [gcd/1]).
:- use_module( library(chr) ).

%% Deprecated syntax used for SICStus 3.x
%handler gcd.
%constraints gcd/1.

%% Syntax for SWI / SICStus 4.x
:- chr_constraint gcd(+natural).


cleanup @ gcd(0) <=> true.
% gcd(N) \ gcd(M) <=> 0<N, N=<M | L is M - N, gcd(L).  %linear complexity
gcd(N) \ gcd(M) <=> 0<N, N=<M | L is M mod N, gcd(L).  %logarithmic complexity

/*
primes1.pl: Prime numbers
(C) Thom.Fruehwirth(at)uni-ulm.de, 92/02/18-20 ECRC, 98/03/11 LMU
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Generate prime numbers by Sieve of Eratosthenes.

%% HOW TO USE
For an integer N, you enter
  prime(N),
and get all primes P1,P2,..,Pm up to N
  prime(P1), prime(P2), prime(Pm).

%% SAMPLE QUERIES
Q: prime(12).
A: prime(11), prime(7), prime(5), prime(3), prime(2).

Q: prime(2).
A: prime(2).

Q: prime(-1).
A: no.
*/

:- module(primes1, [prime/1]).
:- use_module(library(chr)).

%% Deprecated syntax used for SICStus 3.x
%handler primes.
%constraints prime/1.

%% Syntax for SWI / SICStus 4.x
:- chr_constraint prime(+natural).


fail @ prime(N) <=> N<2 | fail.
gen  @ prime(N) ==> N>2 | M is N-1, prime(M).
sift @ prime(X) \ prime(Y) <=> Y mod X =:= 0 | true.

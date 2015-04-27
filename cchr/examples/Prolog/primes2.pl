/*
primes2.pl: Prime numbers (faster variant)
(C) Thom.Fruehwirth(at)uni-ulm.de, 92/02/18-20 ECRC, 98/03/11 LMU
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Generate prime numbers by Sieve of Eratosthenes.
By generating candidates in ascending order, inappropriate ones are sorted 
out earlier, so fewer candidates are checked against others.

%% HOW TO USE
For an integer N, you enter
  upto(N),
and get all primes P1,P2,..,Pm up to N
  prime(P1), prime(P2), prime(Pm).

%% SAMPLE QUERIES
Q: upto(12).
A: upto(1), prime(2), prime(3), prime(5), prime(7), prime(11).

Q: upto(2).
A: upto(1), prime(2).

Q: upto(-1).
A: upto(-1).
*/

:- module(primes2, [upto/1, prime/1, test/1]).
:- use_module(library(chr)).
:- chr_option(debug,off).                                                                                            
:- chr_option(optimize,full).                                                                                        
:- chr_option(check_guard_bindings,off).                                                                             

%% Deprecated syntax used for SICStus 3.x
%handler primes.
%constraints prime/1, upto/1.

%% Syntax for SWI / SICStus 4.x
:- chr_constraint 
    prime(+int), 
    upto(+natural).


gen   @ upto(N) <=> N>1 | M is N-1, upto(M), prime(N).
sift  @ prime(X) \ prime(Y) <=> Z is Y mod X, Z == 0 | true.

test(N) :- upto(N).

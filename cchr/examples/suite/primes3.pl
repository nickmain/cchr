/*
primes3.pl: Prime numbers (from "The Art of Prolog")
(C) Thom.Fruehwirth at uni-ulm.de, 92/02/18-20 ECRC, 98/03/11 LMU
(C) Example from the book "The Art of Prolog"
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Generate prime numbers by Sieve of Eratosthenes.

%% HOW TO USE
For an integer N, you enter
  primes(N,L),
and get for L the list of all primes P1,P2,..,Pm up to N.

%% SEE ALSO
L Sterling, E Shapiro. The Art of Prolog: Advanced Programming Techniques. 
MIT Press, 1986.

%% SAMPLE QUERIES
Q: primes(12,L).
A: L = [2,3,5,7,11].

Q: primes(2,L).
A: L = [2].

Q: primes(-1,L).
A: L = [].
*/

:- module(primes3, [primes/2]).
:- use_module(library(chr)).

%% Deprecated syntax used for SICStus 3.x
%handler primes.
%constraints primes/2, upto/3, sift/2, filter/3.
%% Syntax for SWI / SICStus 4.x
:- chr_type list(X) ---> [] ; [X|list(X)].
:- chr_constraint 
    primes(+natural,?list(natural)), 
    upto(+natural,+natural,?list(natural)), 
    sift(+list(natural),?list(natural)), 
    filter(?list(natural),+natural,?list(natural)).


primes(N,Ps) <=> upto(2,N,Ns), sift(Ns,Ps).

upto(F,T,Ns) <=> F > T | Ns=[].
upto(F,T,Ns) <=> F =< T | Ns=[F|Ns1], F1 is F+1, upto(F1,T,Ns1).

sift([P|Ns],Ps) <=> Ps=[P|Ps1], filter(Ns,P,Ns1), sift(Ns1,Ps1).
sift([],Ps) <=> Ps=[].

filter([X|In],P,Out) <=> 0 =\= X mod P | Out=[X|Out1], filter(In,P,Out1).
filter([X|In],P,Out) <=> 0 =:= X mod P | filter(In,P,Out).
filter([],_P,Out) <=> Out=[].

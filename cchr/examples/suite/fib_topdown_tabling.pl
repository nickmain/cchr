/*
fib_topdown_tabling.pl: Fibonnaci numbers (top-down w/tabling)
(C) Slim.Abdennadher at guc.edu.eg, 99/12/02, LMU
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html1

%% DESCRIPTION
Computes Fibonacci numbers by Top-Down Evaluation with Tabling.
Fibonacci series is formed by adding the latest two numbers 
to get the next one.

%% HOW TO USE
fib(N,M) is true if M is the Nth Fibonacci number.
To calculate the Nth Fibonacci number M, you enter
  fib(N,M).

%% SAMPLE QUERIES
Q: fib(8,A).
A: A=34, fib(8,34), fib(7,21), .., fib(1,1), fib(0,1).

Q: fib(12,233).
A: fib(12,233), fib(11,144), .., fib(1,1), fib(0,1).

Q: fib(11,233).
A: no.
*/

:- module(fib_tdt,[fib/2]).
:- use_module(library(chr)).

%% Deprecated syntax used for SICStus 3.x
%handler fib.
%constraints fib/2.

%% Syntax for SWI / SICStus 4.x
:- chr_constraint fib(?int,?int).


dup @ fib(N,M1) \ fib(N,M2) <=> M1=M2.
f0  @ fib(0,M) ==> M=1.
f1  @ fib(1,M) ==> M=1.
fn  @ fib(N,M) ==> N>=2 | 
        N1 is N-1, N2 is N-2, fib(N1,M1), fib(N2,M2), M is M1+M2.

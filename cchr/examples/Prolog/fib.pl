/*
fib_bottomup.pl: Fibonnaci numbers (bottom-up)
(C) Thom.Fruehwirth at uni-ulm.de
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Computes Fibonacci numbers by Bottom-Up Evaluation.
Fibonacci series is formed by adding the latest two numbers 
to get the next one.

%% HOW TO USE
fib(N,M) is true if M is the Nth Fibonacci number.
To calculate the first N Fibonacci numbers, you enter
  upto(N)
and get
  fib(0,1), fib(1,1), .., fib(N,M).

%% SAMPLE QUERIES
Q: upto(8).
A: upto(8), fib(0,1), fib(1,1), .., fib(7,21), fib(8,34).
*/

:- module(fib_bu,[fib/2, upto/1, test/1]).
:- use_module(library(chr)).
:- chr_option(debug,off).
:- chr_option(optimize,full).
:- chr_option(check_guard_bindings,off).

 
%% Deprecated syntax used for SICStus 3.x
%handler fib.
%constraints fib/2, upto/1.

%% Syntax for SWI / SICStus 4.x
:- chr_constraint
    fib(+int,+int), 
    upto(+natural).


start @ upto(_) ==> fib(0,1), fib(1,1).
next  @ upto(Max), fib(N2,M2) \ fib(N1,M1) <=> Max>N2, N2=:=N1+1 | 
            N is N2+1, M is M1+M2, fib(N,M).

test(N) :- upto(N).

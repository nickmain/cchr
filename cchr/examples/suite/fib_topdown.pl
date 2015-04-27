/*
fib_topdown.pl: Fibonnaci numbers (top-down)
(C) Thom.Fruehwirth at uni-ulm.de
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Computes Fibonacci numbers by Top-Down Evaluation.
Fibonacci series is formed by adding the latest two numbers 
to get the next one.

%% HOW TO USE
fib(N,M) is true if M is the Nth Fibonacci number.
To calculate the Nth Fibonacci number M, you enter
  fib(N,M).

%% SAMPLE QUERIES
Q: fib(8,A).
A: A = 34.

Q: fib(12,233).
A: yes.

Q: fib(11,233).
A: no.
*/

:- module(fib_td,[fib/2]).
:- use_module(library(chr)).

%% Deprecated syntax used for SICStus 3.x
%handler fib.
%constraints fib/2.

%% Syntax for SWI / SICStus 4.x:- chr_option(debug,off).
:- chr_constraint fib(+int,?int).


f0 @ fib(0,M) <=> M=1.
f1 @ fib(1,M) <=> M=1.
fn @ fib(N,M) <=> N>=2 | 
        N1 is N-1, N2 is N-2, fib(N1,M1), fib(N2,M2), M is M1+M2.

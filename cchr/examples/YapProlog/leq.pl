/*
leq.pl: less-equal handler
(C) Thom.Fruehwirth at uni-ulm.de, 1995/05/19, 1998/03/11
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Simple constraint solver for less-equal constraints between variables.

%% HOW TO USE
A is less-equal than B is written as leq(A,B).

%% SAMPLE QUERIES
Q: leq(A,B), leq(B,C).
A: leq(A,B), leq(B,C), leq(A,C).

Q: leq(A,B), leq(B,A).
A: B=A.

Q: leq(A,B), leq(B,C), leq(C,A).
A: B=A, C=A.

Q: leq(F,R), leq(R,U), leq(U,E), leq(E,H), leq(H,W), leq(W,I), leq(I,R), 
    leq(R,T), leq(T,H).
A: H = E, I = E, R = E, T = E, U = E, W = E, leq(F,E).
*/
:- module(leq, [test/1,leq/2]).
:- use_module(library(chr)).

option(debug,off).
option(optimize,full).
option(check_guard_bindings,off).


%% Deprecated syntax used for SICStus 3.x
%handler leq.
constraints leq/2.

%% Syntax for SWI / SICStus 4.x
%:- chr_constraint leq/2.


reflexivity  @ leq(X,X) <=> true.
antisymmetry @ leq(X,Y), leq(Y,X) <=> X = Y.
idempotence  @ leq(X,Y) \ leq(X,Y) <=> true.
transitivity @ leq(X,Y), leq(Y,Z) ==> leq(X,Z).

testcycle(F,P,[E|R]) :- leq(P,E), testcycle(F,E,R).
testcycle(F,P,[E]) :- leq(P,E), leq(E,F).
testcycle([F|R]) :- testcycle(F,F,R).

buildlist(A,1) :- A=[_].
buildlist(A,L) :- A=[_|B], L1 is L-1, buildlist(B,L1).

test(N) :- buildlist(A,N), testcycle(A).

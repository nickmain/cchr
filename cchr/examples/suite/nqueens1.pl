/*
nqueens1.pl: Solves n-queens problem
(C) Thom.Fruehwirth at uni-ulm.de
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Solves the N-queens puzzle:
Place N queens on an NxN chessboard such that they do not attack each other.
This means no two queens may be on the same row, column or diagonal.

%% HOW TO USE
The solution to the N-queens puzzle is a list with N elements: the M-th 
element defines the row for the queen in column M.
solve(N,Sol) returns one solution for the N-queens problem in Sol.
solveall(N,SolN,SolL) returns for the N-queens problem the number of 
solutions in SolN and the list of all solutions in SolL.

%% SAMPLE QUERIES
Q: solve(4,S).
A: S = [3,1,4,2] ;
A: S = [2,4,1,3].

Q: solveall(4,N,S).
A: N = 2, S = [[2,4,1,3],[3,1,4,2]].

Q: solveall(8,N,S).
A: N = 92, S = [[1,5,8,6,3,7,2,4],[1,6,8,3,7,4,2,5],[1,7,4,6,8,2,5,3],...].
*/

:- module(nqueens1, [solve/2, solveall/3]).
:- use_module(library(chr)).
:- use_module(library(lists)).

:- op(700,xfx,'in').

%% Deprecated syntax used for SICStus 3.x
%handler nqueens.
%constraints solve/2, queens/1, safe/3, noattack/3, in/2, enum/1.

%% Syntax for SWI / SICStus 4.x
:- chr_constraint solve/2, queens/1, safe/3, noattack/3, in/2, enum/1.


%% solve(N,Qs): Qs (N-elem. list) is the solution for the N-queens problem
%% M-th element of Qs determines the row for the queen in column M
solve(N,Qs)         <=> makedomains(N,Qs), queens(Qs), enum(Qs).

%% queens(Qs): queens in Qs don't attack each other
queens([])          <=> true.
queens([Q|Qs])      <=> safe(Q,Qs,1), queens(Qs).

% safe(X,Qs,N): queen X doesn't attack the queens Qs (column distance >= N)
safe(_,[],_)        <=> true.
safe(X,[Y|Qs],N)    <=> noattack(X,Y,N), N1 is N+1, safe(X,Qs,N1).

% X in L
_ in []             <=> fail.
X in [X1]           <=> X=X1.

% noattack(X,Y,N): queen X doesn't attack queen Y (column distance = N)
noattack(X,Y,N)     <=> number(X), number(Y) | X=\=Y, X+N=\=Y, Y+N=\=X.
noattack(X,Y,N), Y in L  <=> number(X), X1 is X-N, X2 is X+N, 
        delete(L,X,L1), delete(L1,X1,L2), delete(L2,X2,L0), L\==L0 | Y in L0.
noattack(Y,X,N), Y in L  <=> number(X), X1 is X-N, X2 is X+N, 
        delete(L,X,L1), delete(L1,X1,L2), delete(L2,X2,L0), L\==L0 | Y in L0.


%% enum(N,Qs): fill list Qs successively with values from 1..N
enum([])                <=> true.
enum([X|R])             <=> number(X) | enum(R).
enum([X|R]), X in L     <=> enum_val(X,L), enum(R).

%% enum_val(N,Q): try values N for Q
enum_val(X,[V|L]) :- X=V ; enum_val(X,L).


%% makedomains(N,Qs): Qs is an N-elem. list, create 'X in [1..N]' constraints
makedomains(N,Qs) :- length(Qs,N), upto(N,D), domain(Qs,D).

%% upto(N,L): L=[1..N]
upto(0,[]).
upto(N,[N|L]) :- N>0, N1 is N-1, upto(N1,L).

%% domain(Qs,D): create 'Q in D' constraints for all Q from Qs
domain([],_).
domain([Q|Qs],D) :- Q in D, domain(Qs,D).


%% solveall(N,SolN,SolL): find all solutions for N-queens problem
%% SolN: number of solutions, SolL: list with solutions
solveall(N,SolN,SolL) :- setof(Qs, solve(N,Qs), SolL), length(SolL,SolN).

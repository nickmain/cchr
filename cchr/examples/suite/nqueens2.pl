/*
nqueens2.pl: Solves n-queens problem
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
Q: solve(4).
A: labelq,
    q(1)in[2], 
    q(2)in[4],
    q(3)in[1],
    q(4)in[3] ;
A: labelq,
    q(1)in[3],
    q(2)in[1],
    q(3)in[4],
    q(4)in[2].

Q: solve(8).
A: labelq,
    q(1)in[1],
    q(2)in[5],
    q(3)in[8],
    q(4)in[6],
    q(5)in[3],
    q(6)in[7],
    q(7)in[2],
    q(8)in[4] ;
A: labelq,
    q(1)in[1],
    q(2)in[6],
    q(3)in[8],
    q(4)in[3],
    q(5)in[7],
    q(6)in[4],
    q(7)in[2],
    q(8)in[5] ;
... 90 more answers
*/

:- module(nqueens2, [solve/1, in/2]).
:- use_module(library(chr)).
:- use_module(library(lists)).

:- op(700,xfx,'in').

%% Deprecated syntax used for SICStus 3.x
%handler nqueens.
%constraints solve/1, queens/3, in/2, labelq/0.

%% Syntax for SWI / SICStus 4.x
:- chr_constraint solve/1, queens/3, in/2, labelq/0.


%% solve(N): solve N-queens problem
solve(N)            <=> queens(N,[],_), labelq.

%% queens(N,L,D): for each M in 1..N create 'q(M) in D' (domain D generated)
queens(N,L,D)        <=> N>0 | N1 is N-1, q(N) in D, queens(N1,[N|L],D).
queens(0,L,D)        <=> L=D.

%% q(N) in P: queen in column N may sit in rows P
%% reduce possible positions of queens; fail if no position remains
q(N1) in [P] \ q(N2) in D <=> P1 is P-(N1-N2), P2 is P+(N1-N2), 
        delete(D,P,D1), delete(D1,P1,D2), delete(D2,P2,D3), D\==D3 |
        D3\==[], q(N2) in D3.

%% next 3 rules are VARIANT to last rule
%q(N)in[P] \ q(N1)in D <=> select(P,D,D1) | D1\==[], q(N1) in D1.
%q(N)in[P] \ q(N1)in D <=> P1 is P-(N-N1),select(P1,D,D1)| D1\==[],q(N1)in D1.
%q(N)in[P] \ q(N1)in D <=> P1 is P+(N-N1),select(P1,D,D1)| D1\==[],q(N1)in D1.

%% label queens: select one of several queen positions
labelq \ q(N) in D  <=> D=[_,_|_] | member(P,D), q(N) in [P].

/*
minmax.pl: INEQUALITIES with MINIMIUM and MAXIMUM on ground terms
(C) Thom.Fruehwirth at uni-ulm.de, 1992/03/03 ECRC, 2005/05/12
(C) Christian Holzbaur, 1996/11/05
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Inequality (lss, grt, neq, geq, leq), minimum and maximum constraints on 
ground terms are simplified (support for labeling).

%% HOW TO USE
The following constraints are handled:
A lss B         A less than B
A grt B         A greater than B
A neq B         A not equal to B
A geq B         A greater or equal to B
A leq B         A less or equal to B
A ~= B          A not identical B
min(A,B,C)      C is the minimum of A and B
max(A,B,C)      C is the maximum of A and B
labeling        uses labeling with minimum and maximum 

%% SAMPLE QUERIES
Q: min(1,2,C).
A: C = 1.

Q: min(3,Y,1).
A: Y=1.

Q: min(X,1,1).
A: 1 leq X.

Q: A lss B, A grt B.
A: no.

Q: A leq B, A geq B.
A: B = A.

Q: A leq B, B grt A.
A: A lss B.

Q: min(A,B,C), max(A,B,C).
A: B = A, C = A.

Q: min(3,Y,Z), min(Y,Z,M).
A: Z = M, M leq Y, M leq 3, min(3,Y,M).

Q: min(A,B,C), A leq B.
A: C = A, A leq B.

Q: max(A,B,C), A lss C.
A: C = B, A lss B.

Q: min(A,B,C), max(B,C,D), min(C,D,A).
A: C = A, D = B, A leq B.

Q: min(A,B,C), min(B,C,A), min(C,A,B).
A: B = A, C = A.

Q: min(A,B,C), A neq B.
A: C leq A, C leq B, A neq B, min(A,B,C).

Q: min(A,B,C), A neq B, labeling.
A: C = A, A lss B, labeling ;
A: C = B, B lss A, labeling.
*/


:- module(minmax, [(~=)/2, (leq)/2, (lss)/2, (neq)/2, (geq)/2, (grt)/2, 
    min/3, max/3, labeling/0]).
:- use_module(library(chr)).

:- op(700, xfx, lss). % less than
:- op(700, xfx, grt). % greater than
:- op(700, xfx, neq). % not equal to
:- op(700, xfx, geq). % greater or equal to
:- op(700, xfx, leq). % less or equal to
:- op(700, xfx, ~=).  % not identical

%% Deprecated syntax used for SICStus 3.x
%handler minmax.
%constraints (~=)/2, (leq)/2, (lss)/2, (neq)/2, min/3, max/3, labeling/0.

%% Syntax for SWI / SICStus 4.x
:- chr_constraint (~=)/2, (leq)/2, (lss)/2, (neq)/2, min/3, max/3, labeling/0.


%% remove duplicates

A ~= B \ A ~= B <=> true.
A leq B \ A leq B <=> true.
A lss B \ A lss B <=> true.
A neq B \ A neq B <=> true.


%% geq, grt

X geq Y :- Y leq X.
X grt Y :- Y lss X.


%% ~=

X ~= X <=> fail.
X ~= Y <=> ground(X),ground(Y) | X\==Y.


%% leq

built_in     @ X leq Y <=> ground(X),ground(Y) | X @=< Y.
reflexivity  @ X leq X <=> true.

antisymmetry @ X leq Y, Y leq X <=> X = Y.

transitivity @ X leq Y, Y leq Z ==> X \== Y, Y \== Z, X \== Z | X leq Z.

subsumption  @ X leq N \ X leq M <=> ground(N),ground(M),N@<M | true.
subsumption  @ M leq X \ N leq X <=> ground(N),ground(M),N@<M | true.


%% lss

built_in     @ X lss Y <=> ground(X),ground(Y) | X @< Y.
irreflexivity@ X lss X <=> fail.

transitivity @ X lss Y, Y lss Z ==> X \== Y, Y \== Z | X lss Z.
transitivity @ X leq Y, Y lss Z ==> X \== Y, Y \== Z | X lss Z.
transitivity @ X lss Y, Y leq Z ==> X \== Y, Y \== Z | X lss Z.

subsumption  @ X lss Y \ X leq Y <=> true.

subsumption  @ X lss N \ X lss M <=> ground(N),ground(M),N@<M | true.
subsumption  @ M lss X \ N lss X <=> ground(N),ground(M),N@<M | true.

subsumption  @ X leq N \ X lss M <=> ground(N),ground(M),N@<M | true.
subsumption  @ M leq X \ N lss X <=> ground(N),ground(M),N@<M | true.
subsumption  @ X lss N \ X leq M <=> ground(N),ground(M),N@<M | true.
subsumption  @ M lss X \ N leq X <=> ground(N),ground(M),N@<M | true.


%% neq

built_in     @ X neq Y <=> ground(X),ground(Y) | X\==Y.
irreflexivity@ X neq X <=> fail. 

subsumption  @ X neq Y \ Y neq X <=> true.
subsumption  @ X lss Y \ X neq Y <=> true.
subsumption  @ X lss Y \ Y neq X <=> true.

simplification @ X neq Y, X leq Y <=> X lss Y. 
simplification @ Y neq X, X leq Y <=> X lss Y. 


%% MINIMUM

labeling, min(X, Y, Z)#Pc <=> 
    (X leq Y, Z = X ; Y lss X, Z = Y), 
    labeling
    pragma passive(Pc).

built_in @ min(X, Y, Z) <=> ground(X),ground(Y) | (X@=<Y -> Z=X ; Z=Y).
built_in @ min(X, Y, Z) <=> ground(X),ground(Z),X\==Z | Z = Y, Y lss X.
built_in @ min(Y, X, Z) <=> ground(X),ground(Z),X\==Z | Z = Y, Y lss X.

min_eq @ min(X, X, Y) <=> X = Y.

min_leq @ Y leq X \ min(X, Y, Z) <=> Y=Z.
min_leq @ X leq Y \ min(X, Y, Z) <=> X=Z.
min_lss @ Z lss X \ min(X, Y, Z) <=> Y=Z.
min_lss @ Z lss Y \ min(X, Y, Z) <=> X=Z. 

functional @ min(X, Y, Z) \ min(X, Y, Z1) <=> Z1=Z.
functional @ min(X, Y, Z) \ min(Y, X, Z1) <=> Z1=Z.

propagation @ min(X, Y, Z) ==> X\==Y | Z leq X, Z leq Y.


%% MAXIMUM

labeling, max(X, Y, Z)#Pc <=> 
    (X leq Y, Z = Y ; Y lss X, Z = X), 
    labeling
    pragma passive(Pc).

built_in @ max(X, Y, Z) <=> ground(X),ground(Y) | (Y@=<X -> Z=X ; Z=Y).
built_in @ max(X, Y, Z) <=> ground(X),ground(Z),X\==Z | Z = Y, X lss Y.
built_in @ max(Y, X, Z) <=> ground(X),ground(Z),X\==Z | Z = Y, X lss Y.

max_eq @ max(X, X, Y) <=> X = Y.

max_leq @ Y leq X \ max(X, Y, Z) <=> X=Z.
max_leq @ X leq Y \ max(X, Y, Z) <=> Y=Z.
max_lss @ X lss Z \ max(X, Y, Z) <=> Y=Z.
max_lss @ Y lss Z \ max(X, Y, Z) <=> X=Z. 

functional @ max(X, Y, Z) \ max(X, Y, Z1) <=> Z1=Z.
functional @ max(X, Y, Z) \ max(Y, X, Z1) <=> Z1=Z.

propagation @ max(X, Y, Z) ==> X\==Y | X leq Z, Y leq Z.

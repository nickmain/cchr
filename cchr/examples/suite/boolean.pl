/*
boolean.pl: boolean handler with labeling
(C) Thom.Fruehwirth at uni-ulm.de, 1991/05-1993/11
(C) Christian Holzbaur, 1996/11
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Boolean handler for and, or, xor, neg, and imp (implication) with labeling and
halfadder and fulladder.

%% HOW TO USE
The following constraints are handled:
boolean(A)      A is boolean: A=0 or A=1
and(A,B,C)      logical conjunction: C=A and B
or(A,B,C)       logical disjunction: C=A or B
xor(A,B,C)      logical exclusive-or: C=A xor B
neg(A,B)        logical negation: B=not A
imp(A,B)        logical implication: not A or B=true
labeling(A)     label variable A
half_adder(X,Y,S,C)         X+Y = S+2*C
full_adder(X,Y,Ci,S,Co)     X+Y+Z = S+2*Co

%% SAMPLE QUERIES
Q: and(0,1,A), xor(1,1,B).
A: A = 0, B = 0.

Q: neg(A,A).
A: no.

Q: or(A,B,0), and(A,B,C), neg(C,D).
A: A = 0, B = 0, C = 0, D = 1.

Q: neg(A,B), or(A,B,C).
A: C = 1, neg(A,B).

Q: neg(A,B), or(A,B,C), labeling.
A: A = 0, B = 1, C = 1, labeling ;
A: A = 1, B = 0, C = 1, labeling.

Q: and(X,Y,Z),(X=1;X=0;X=Y).
A: X = 1, Z = Y ;
A: X = 0, Z = 0 ;
A: Y = X, Z = X.

Q: imp(A,B), imp(B,A).
A: B = A.

Q: neg(A,B), imp(A,B).
A: A = 0, B = 1.

Q: and(A,B,C), or(A,B,D), imp(C,D).
A: imp(C,D), and(A,B,C), or(A,B,D).

Q: and(A,B,C), or(A,B,D), imp(C,D), labeling.
A: A = 0, C = 0, D = B, labeling ;
A: A = 1, C = B, D = 1, labeling.

Q: and(A,B,C), or(A,B,D), imp(D,C), labeling.
A: A = 0, B = 0, C = 0, D = 0, labeling ;
A: A = 1, B = 1, C = 1, D = 1, labeling.

Q: half_adder(A0,B0,S0,C1), full_adder(A1,B1,C1,S1,C2), 
    full_adder(A2,B2,C2,S2,C3), full_adder(A3,B3,C3,S3,C), 
    A3=1,A2=0,A1=0,A0=1, B3=0,B2=1,B1=1,B0=1.
A: C = 1, S3 = 0, S2 = 0, S1 = 0, S0 = 0,   C1 = 1, C2 = 1, C3 = 1,
    A0 = 1, A1 = 0, A2 = 0, A3 = 1, B0 = 1, B1 = 1, B2 = 1, B3 = 0, 
*/

:- module(boolean, 
[boolean/1, and/3, or/3, xor/3, neg/2, imp/2, labeling/0, 
    half_adder/4, full_adder/5]).
:- use_module( library(chr)).

%% Deprecated syntax used for SICStus 3.x
%handler boolean.
%constraints boolean/1, and/3, or/3, xor/3, neg/2, imp/2, 
%   labeling/0, half_adder/4, full_adder/5.

%% Syntax for SWI / SICStus 4.x
:- chr_type bool ---> 0 ; 1.

:- chr_constraint
    boolean(?bool),
    and(?bool,?bool,?bool), 
    or(?bool,?bool,?bool), 
    xor(?bool,?bool,?bool),
    neg(?bool,?bool), 
    imp(?bool,?bool),
    labeling,
    half_adder(?bool,?bool,?bool,?bool),
    full_adder(?bool,?bool,?bool,?bool,?bool).


boolean(0) <=> true.
boolean(1) <=> true.

labeling \ boolean(A) <=> A=0 ; A=1.


%% and/3 specification
%%and(0,0,0).
%%and(0,1,0).
%%and(1,0,0).
%%and(1,1,1).

and(0,_,Y) <=> Y=0.
and(_,0,Y) <=> Y=0.
and(1,X,Y) <=> Y=X.
and(X,1,Y) <=> Y=X.
and(X,Y,1) <=> X=1,Y=1.
and(X,X,Z) <=> X=Z.
and(X,Y,A) \ and(X,Y,B) <=> A=B.
and(X,Y,A) \ and(Y,X,B) <=> A=B.

labeling \ and(A,B,C) <=> label_and(A,B,C).

label_and(0,_,0).
label_and(1,X,X).


%% or/3 specification
%%or(0,0,0).
%%or(0,1,1).
%%or(1,0,1).
%%or(1,1,1).

or(0,X,Y) <=> Y=X.
or(X,0,Y) <=> Y=X.
or(X,Y,0) <=> X=0,Y=0.
or(1,_,Y) <=> Y=1.
or(_,1,Y) <=> Y=1.
or(X,X,Z) <=> X=Z.
or(X,Y,A) \ or(X,Y,B) <=> A=B.
or(X,Y,A) \ or(Y,X,B) <=> A=B.

labeling \ or(A,B,C) <=> label_or(A,B,C).

label_or(0,X,X).
label_or(1,_,1).


%% xor/3 specification
%%xor(0,0,0).
%%xor(0,1,1).
%%xor(1,0,1).
%%xor(1,1,0).

xor(0,X,Y) <=> X=Y.
xor(X,0,Y) <=> X=Y.
xor(X,Y,0) <=> X=Y.
xor(1,X,Y) <=> neg(X,Y).
xor(X,1,Y) <=> neg(X,Y).
xor(X,Y,1) <=> neg(X,Y).
xor(X,X,Y) <=> Y=0.
xor(X,Y,X) <=> Y=0.
xor(Y,X,X) <=> Y=0.
xor(X,Y,A) \ xor(X,Y,B) <=> A=B.
xor(X,Y,A) \ xor(Y,X,B) <=> A=B.

labeling \ xor(A,B,C) <=> label_xor(A,B,C).

label_xor(0,X,X).
label_xor(1,X,Y) :- neg(X,Y).


%% neg/2 specification
%%neg(0,1).
%%neg(1,0).

neg(0,X) <=> X=1.
neg(X,0) <=> X=1.
neg(1,X) <=> X=0.
neg(X,1) <=> X=0.
neg(X,X) <=> fail.
neg(X,Y) \ neg(Y,Z) <=> X=Z.
neg(X,Y) \ neg(Z,Y) <=> X=Z.
neg(Y,X) \ neg(Y,Z) <=> X=Z.

%% Interaction with other boolean constraints
neg(X,Y) \ and(X,Y,Z) <=> Z=0.
neg(Y,X) \ and(X,Y,Z) <=> Z=0.
neg(X,Z) , and(X,Y,Z) <=> X=1,Y=0,Z=0.
neg(Z,X) , and(X,Y,Z) <=> X=1,Y=0,Z=0.
neg(Y,Z) , and(X,Y,Z) <=> X=0,Y=1,Z=0.
neg(Z,Y) , and(X,Y,Z) <=> X=0,Y=1,Z=0.
neg(X,Y) \ or(X,Y,Z) <=> Z=1.
neg(Y,X) \ or(X,Y,Z) <=> Z=1.
neg(X,Z) , or(X,Y,Z) <=> X=0,Y=1,Z=1.
neg(Z,X) , or(X,Y,Z) <=> X=0,Y=1,Z=1.
neg(Y,Z) , or(X,Y,Z) <=> X=1,Y=0,Z=1.
neg(Z,Y) , or(X,Y,Z) <=> X=1,Y=0,Z=1.
neg(X,Y) \ xor(X,Y,Z) <=> Z=1.
neg(Y,X) \ xor(X,Y,Z) <=> Z=1.
neg(X,Z) \ xor(X,Y,Z) <=> Y=1.
neg(Z,X) \ xor(X,Y,Z) <=> Y=1.
neg(Y,Z) \ xor(X,Y,Z) <=> X=1.
neg(Z,Y) \ xor(X,Y,Z) <=> X=1.
neg(X,Y) , imp(X,Y) <=> X=0,Y=1.
neg(Y,X) , imp(X,Y) <=> X=0,Y=1.

labeling \ neg(A,B) <=> label_neg(A,B).

label_neg(0,1).
label_neg(1,0).


%% imp/2 specification (implication)
%%imp(0,0).
%%imp(0,1).
%%imp(1,1).

imp(0,_) <=> true.
imp(X,0) <=> X=0.
imp(1,X) <=> X=1.
imp(_,1) <=> true.
imp(X,X) <=> true.
imp(X,Y),imp(Y,X) <=> X=Y.

labeling \ imp(A,B) <=> label_imp(A,B).

label_imp(0,_).
label_imp(1,1).


%% half_adder/4: add bits X and Y, result in S and carry in C
%% X+Y = S+2*C
half_adder(X,Y,S,C) <=>
    xor(X,Y,S), and(X,Y,C).


%% full_adder/5: add bits X, Y and Ci, result in S and carry in Co
%% X+Y+Ci = S+2*Co
full_adder(X,Y,Ci,S,Co) <=>
    half_adder(X,Y,S1,Co1), half_adder(Ci,S1,S,Co2), or(Co1,Co2,Co).

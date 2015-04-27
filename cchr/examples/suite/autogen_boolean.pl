/*
autogen_boolean.pl: Auto-generated boolean handler
(C) Slim.Abdennadher at guc.edu.eg, 2005
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Automatically generated boolean handler for and, or, neg and xor.

%% HOW TO USE
The following constraints are handled:
and(A,B,C) logical conjunction
or(A,B,C)  logical disjunction
neg(A,B)   logical negation
xor(A,B,C) logical exclusive-or

%% SEE ALSO
S Abdennadher, C Rigotti. Automatic generation of CHR constraint solvers.
Theory and Practice of Logic Programming, 2005.

%% SAMPLE QUERIES
Q: and(0,1,A), xor(1,1,B).
A: A=0, B=0, neg(1,0), neg(1,0), and(0,1,0), xor(1,1,0).

Q: neg(A,A).
A: no.

Q: or(A,B,0), and(A,B,C), neg(C,D).
A: A=0, B=0, C=0, D=1, neg(0,1), or(0,0,0), and(0,0,0).

Q: neg(A,B), and(A,B,C).
A: C=0, neg(A,B), and(A,B,0).

Q: neg(A,B), or(A,B,C).
A: neg(A,B), or(A,B,C).

Q: and(X,Y,Z),(X=1;X=0;X=Y).
A: X=1, Z=Y, and(1,Y,Y).
A: X=0, Z=0, and(0,Y,0).
A: Y=X, Z=X, and(X,X,X).
*/

:- module(autogen_boolean, [and/3, or/3, neg/2, xor/3]).
:- use_module( library(chr)).

%% Deprecated syntax used for SICStus 3.x
%handler autogen_boolean.
%constraints and/3, or/3, neg/2, xor/3.

%% Syntax for SWI / SICStus 4.x
:- chr_type bool ---> 0 ; 1.
:- chr_constraint
    and(?bool,?bool,?bool), 
    or(?bool,?bool,?bool), 
    neg(?bool,?bool), 
    xor(?bool,?bool,?bool).


%% logical conjunction and/3

and(A,A,B)==>A=B.
and(A,B,1)==>A=B.
and(A,1,B)==>A=B.
and(_,0,A)==>A=0.
and(1,A,B)==>A=B.
and(0,_,A)==>A=0.


%% logical disjunction or/3

or(A,A,B)==>A=B.
or(A,B,0)==>A=B.
or(_,1,A)==>A=1.
or(A,0,B)==>A=B.
or(1,_,A)==>A=1.
or(0,A,B)==>A=B.


%% logical negation neg/2

neg(A,A)==>fail.
neg(A,1)==>A=0.
neg(A,0)==>A=1.
neg(1,A)==>A=0.
neg(0,A)==>A=1.


%% logical exclusive-or xor/3

xor(A,B,B)==>A=0.
xor(A,B,A)==>B=0.
xor(A,A,B)==>B=0.
xor(A,B,1)==>neg(A,B).
xor(A,B,0)==>A=B.
xor(A,1,B)==>neg(A,B).
xor(A,0,B)==>A=B.
xor(1,A,B)==>neg(A,B).
xor(0,A,B)==>A=B.


%% Interaction between conjunction and negation

and(A,B,C),neg(A,C)==>C=0,B=C.
and(A,B,C),neg(A,B)==>C=0.
and(A,B,C),neg(C,A)==>A=1,B=C.
and(A,B,C),neg(B,A)==>C=0.
and(A,B,C),neg(B,C)==>A=C.
and(A,B,C),neg(C,B)==>A=C.

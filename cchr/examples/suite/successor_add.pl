/*
successor_add.pl: addition of numbers in successor notation
(C) Slim.Abdennadher at guc.edu.eg
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Handler to finds solutions for addition constraints over numbers in successor 
notation.

%% HOW TO USE
The constraint add(X,Y,Z) means X+Y=Z. X, Y and Z may be variables or numbers 
in successor notation (i.e., 0,1,2 are represented as 0, s(0), s(s(0)), etc).
The constraint is checked for validity and variables values are inferred.

%% SAMPLE QUERIES
Q: add(s(s(0)),s(s(s(0))),s(s(s(s(s(0)))))).
A: yes.

Q: add(X,s(s(0)),s(s(s(0)))).
A: X = s(0).

Q: add(s(s(0)), s(0), Z).
A: Z = s(s(s(0))).

Q: add(X,Y,s(s(0))).
A: X = s(s(0)), Y = 0 ;
A: X = s(0),    Y = s(0) ;
A: X = s(0),    Y = s(0) ;
A: X = 0,       Y = s(s(0)).

Q: add(X,X,s(s(0))).
A: X = s(0).

Q: add(X,X,s(s(s(0)))).
A: no.

Q: add(s(0),X,Y), add(X,s(s(0)),s(s(s(0)))).
A: X = s(0), Y = s(s(0)).
*/

:- module(successor_add, [add/3]).
:- use_module(library(chr)).

%% Deprecated syntax used for SICStus 3.x
%handler add.
%constraints add/3.

%% Syntax for SWI / SICStus 4.x
:- chr_constraint add/3.


zero1 @ add(0,Y,Z) <=> Y = Z.
zero2 @ add(X,0,Z) <=> X = Z.
zero3 @ add(X,Y,0) <=> X = 0, Y = 0.

same1 @ add(X,E,E) <=> X = 0.
same2 @ add(E,Y,E) <=> Y = 0.

succ1 @ add(s(X),Y,Z) <=> Z = s(W), add(X,Y,W).
succ2 @ add(X,s(Y),Z) <=> Z = s(W), add(X,Y,W).
succ3 @ add(X,X,s(Z)) <=> Z = s(W), X = s(Y), add(Y,Y,W).

search @ add(X,Y,s(Z)) <=> true | add(X1,Y1,Z),
                                (X = s(X1),Y = Y1 ; X = X1,Y = s(Y1)).

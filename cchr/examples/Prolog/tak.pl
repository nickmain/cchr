/*
tak.pl: Takeuchi function
(C) Jon.Sneyers at cs.kuleuven.be
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
The TAK function is devised by I. Takeuchi in 1978 (cf. 
http://mathworld.wolfram.com/TAKFunction.html):
For integers X, Y, and Z, and a function h, it is
tak(h,X,Y,Z) = 
    h(X,Y,Z)                                                for X=<Y
    tak(h, tak(h,X-1,Y,Z), tak(h,Y-1,Z,X), tak(h,Z-1,X,Y))  for X>Y

%% HOW TO USE
Calculates the result R of the TAK function for h(X,Y,Z)=Z:
    tak(X,Y,Z, R).

%% SAMPLE QUERIES
Q: tak(18,12,6, R).
A: R = 7.
*/

:- module(tak, [tak/4]).
:- use_module(library(chr)).
:- chr_option(debug,off).                                                                                            
:- chr_option(optimize,full).                                                                                        
:- chr_option(check_guard_bindings,off).                                                                             

%% Deprecated syntax used for SICStus 3.x
%handler tak.
%constraints tak/4.

%% Syntax for SWI / SICStus 4.x
:- chr_constraint
    tak(+int,+int,+int, ?int).

tak(X,Y,Z,A1) \ tak(X,Y,Z,A2) <=> A1=A2.

tak(X,Y,Z,A) ==> X =< Y | Z = A.

tak(X,Y,Z,A) ==> X > Y | 
        X1 is X-1, Y1 is Y-1, Z1 is Z-1,
        tak(X1,Y,Z,A1), tak(Y1,Z,X,A2), tak(Z1,X,Y,A3),
        tak(A1,A2,A3,A).

test(X,Y,Z) :- tak(X,Y,Z,_).

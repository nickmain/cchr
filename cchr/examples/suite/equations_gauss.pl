/*
equations_gauss.pl: solving linear polynomial equations
(C) Thom.Fruehwirth at uni-ulm.de, 1991-1998
(C) Christian Holzbaur, 1996
(C) Martin.Kaeser at uni-ulm.de, 2006/12/01
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Solves linear polynomial equations by Gaussian variable elimination.

%% HOW TO USE
The handler solves a set of equations enclosed in curly brackets: 
{PL1=PR1, PL2=PR2, .., PLn=PRn}.  (PLi, PRi are linear polynomial terms)
If no solution exists the handler fails.

Variables can either be expressed by Prolog variables (X) or by atoms (x).
If Prolog variables are used, as soon as the handler has determined the value 
of a variable it is bound to this value, e.g.: X = 10.
For Prolog variables, the termination of the handler requires a stable 
variable order that is implemented by the imported module 'var_order'.
If you don't want to use this module, you can also use atoms for this purpose,
a variable binding is then represented as a normalized equation, e.g.: 
[1*x] eq 10.


%% SAMPLE QUERIES
Q: {x+y=10, x-y=6}.
A: [1*x]eq 8.0, [1*y]eq 2.0.

Q: {X+Y=10, X-Y=6}.
A: X = 8.0, Y = 2.0.

Q: {X+Y=10, X+Y=6}.
A: no.

Q: {3 * X + 2 * Y - 4 * (3 + Z) = 2 * (X - 3) + (Y + Z) * 7,
    2 * (X + Y + Z) = 3 * (X - Y - Z),
    5 * (X + Y) - 7 * X - Z = (2 + 1 + X) * 6}.
A: X = -1.7142857142857144,
    Y = 0.6571428571428571,
    Z = -1.0.

Q: {3 * X + 2 * Y - 4 * (3 + Z) = 2 * (X - 3) + (Y + Z) * 7,
    2 * (X + Y + Z) = 3 * (X - Y - Z)}.
A: Z = -1.0, [1*X,-5*Y]eq-5.0.

Q: {2+2*X+4*Y+2*Z=0, 4+2*X+4*Y+4*Z=0, 1+2*X+3*Z=0}.
A: X = 1.0, Y = -0.5, Z = -1.0.

Q: {-10+1*X+1*Y=0, 2+1*X+(-1)*Y=0}.
A: X = 4.0, Y = 6.0.

Q: {-10+1*X+1*Y=0, 2+1*X+(-1)*Y=0, -4+1*X=0}.
A: X = 4.0, Y = 6.0.

Q: {-10+1*X+1*Y=0, 2+1*X+(-1)*Y=0, -5+1*X=0}.
A: no.

Q: {-1+1*X+1*Y=0, -2+2*X+2*Y=0}.
A: [1*X,1*Y]eq 1 ? 

Q: {X=Y+Z, X+Z=10, Y=10}.
A: X = 10.0, Y = 10, Z = 0.0.

Q: {X+Y=10, X-Z=20, 2*Y+Z=30}.
A: X = -30.0, Y = 40.0, Z = -50.0.

Q: {-(X)+Y+Z+2*Y+X+3*X+2=12}.
A: [1*X,1.0*Y,0.3333333333333333*Z]eq 3.333333333333333.
*/

:- module(equations_gauss, [eq/2, {}/1]).
:- use_module(library(chr)).
:- use_module(library(lists)).
:- use_module(var_order).

:- op(600,xfx,eq).

%% Deprecated syntax used for SICStus 3.x
%handler gauss.
%constraints eq/2, {}/1.

%% Syntax for SWI / SICStus 4.x
:- chr_constraint eq/2, {}/1.

%% Poly eq Const, where Poly is list of monomials Coefficient*Variable
%%    eq/2

%% curly brackets as wrapper to avoid name clash with built-in =
%%    {}/1   


%% split system of equations
split @     {E, F} <=> {E}, {F}.

%% convert arbitrary polynomial equation to eq-notation
normalize @ {A = B} <=> globalize([A-B]), 
    normpoly([A-B] eq 0, Poly eq Const), Poly eq Const.


%% 0=C
zero @      [] eq C <=> zero(C).

%% normalize first coefficient to 1
norm1 @     [A*X|P] eq C <=> notzero(A-1) | 
    multpoly(1/A,P eq C,P2 eq C2), [1*X|P2] eq C2.

%% const on left side (because of bound variable)
bound @     P eq C <=> select(A*X, P, P0), number(X) | C0 is C-A*X, P0 eq C0.

%% A*X = C  =>  bind variable
bind @      [A*X] eq C <=> var(X) | X is C/A.


%% Gaussian elimination 
elimate @   [A*X|P] eq C \ Q eq D <=>
    select(B*X, Q, Q1) |
    multpoly(-B/A, P eq C, E),
    addpoly(E, Q1 eq D, Q0 eq D0), 
    Q0 eq D0.


%% Auxiliary predicates

%% stable variable order based on chr/ordering by C. Holzbaur (var_order.pl)
%% compare variables with var_compare
ord_lss(X,Y) :- var_compare(<,X,Y).
%% otherwise use Prolog standard order
ord_lss(X,Y) :- nonvar(Y), X @< Y.


%% numerical stabilty
notzero(A) :- abs(A) >  0.000001.
zero(A)    :- abs(A) =< 0.000001.

%% safe_is(X,Exp): X is Exp; fails, if Exp is not an arith. expression
safe_is(X,Exp) :- on_exception(_, X is Exp, fail).


%% normalize polynomial equation
normpoly([X] eq C, [1*X] eq C) :- var(X), !.
normpoly([X] eq C, [1*X] eq C) :- simple(X), \+ number(X).
normpoly([A] eq C, [] eq C0) :- number(A), C0 is C-A.
normpoly([A,B|P] eq C, E0) :- 
    normpoly([A] eq 0,E1), normpoly([B|P] eq C,E2), addpoly(E1,E2,E0).
normpoly([+A] eq 0, E0) :- 
    normpoly([A] eq 0, E0).
normpoly([-A] eq 0, E0) :- 
    normpoly([A] eq 0, E1), multpoly(-1, E1, E0).
normpoly([A+B] eq C, E0) :- 
    normpoly([A] eq C,E), normpoly([B] eq 0,F), addpoly(E,F,E0).
normpoly([A-B] eq C, E0) :- normpoly([A] eq C,E), normpoly([B] eq 0,F),
    multpoly(-1,F,F1), addpoly(E,F1,E0).
normpoly([A*X] eq 0, E0) :-
    safe_is(A1,A), normpoly([X] eq 0, E), multpoly(A1,E,E0).
normpoly([X*A] eq 0, E0) :-
    safe_is(A1,A), normpoly([X] eq 0, E), multpoly(A1,E,E0).
normpoly([X/A] eq 0, E0) :-
    safe_is(A1,1/A), normpoly([X] eq 0, E), multpoly(A1,E,E0).
normpoly([A] eq C, P0 eq C0) :- 
    C\==0, normpoly([A] eq 0, P0 eq C1), C0 is C+C1.


%% addpoly(E,F, E0): add polynomial equations
%% requires: polynomials ordered + duplicate free; retains properties
addpoly(P eq C, [] eq D, P eq C0) :- !, C0 is C+D.
addpoly([] eq C, Q eq D, Q eq C0) :- !, C0 is C+D.
addpoly([A*X|P] eq C, [B*Y|Q] eq D, E0) :- X==Y, !,
    A1 is A+B, addpoly(P eq C, Q eq D, P1 eq C1),
    (zero(A1) -> E0 = P1 eq C1 ; E0 = [A1*X|P1] eq C1).
addpoly([A*X|P] eq C, [B*Y|Q] eq D, [A*X|P0] eq C0) :- ord_lss(X,Y), !,
    addpoly(P eq C, [B*Y|Q] eq D, P0 eq C0).
addpoly([A*X|P] eq C, [B*Y|Q] eq D, [B*Y|P0] eq C0) :- ord_lss(Y,X), !,
    addpoly([A*X|P] eq C, Q eq D, P0 eq C0).
   
%% multpoly(M,E, E0): multiply polynomial equation with scalar
multpoly(M, [] eq C, [] eq C0) :- C0 is M*C.
multpoly(M, [A*X|P] eq C, E0) :- 
    A1 is M*A, multpoly(M,P eq C,P1 eq C1),
    (zero(A1) -> E0 = P1 eq C1 ; E0 = [A1*X|P1] eq C1).

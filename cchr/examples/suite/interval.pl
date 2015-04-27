/*
interval.pl: interval domains over integer and real
(C) Thom.Fruehwirth at uni-ulm.de, LMU, 1998/01/29, 1998/07/11
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Reasoning about real and integer intervals is provided including intersection,
addition, multiplication, comparison and labelling.

%% HOW TO USE
For unbound variables X, Y and Z and evaluable (i.e. ground) arithmetic 
expressions (or numbers) A and B the following constraints are supported:
- X::A:B        X is between numbers A and B, inclusively (X element of [A,B])
- X le Y        X is smaller or equal Y (X =< Y)
- X eq Y        X is equal Y (X = Y)
- X ne Y        X is not equal Y (X \= Y)
- add(X,Y,Z)    X + Y = Z
- mult(X,Y,Z)   X * Y = Z
- int(X)        X is an integer
- bool(X)       X is boolean (0 or 1)
- split0(X)     for X::A:B (A<0<B) cases X::A:0 and X::0:B are distinguished
- split(X)      for X::A:B cases X::A:M and X::M:B (M=(A+B)/2) are discerned
- label(X)      split(X) is applied repeatedly until interval is too small

%% SAMPLE QUERIES
Q: X::3:5, X::2:4.
A: X::3:4.

Q: X le Y, X::3:5, Y::2:4.
A: X le Y, X::3:4, Y::3:4.

Q: add(X,Y,Z), X::2:5, Y::3:4, Z::1:7.
A: add(X,Y,Z), X::2:4, Y::3:4, Z::5:7.

Q: mult(X,Y,Z), X:: -2:3, Y:: -3:4, Z:: -12: -9.
A: mult(X,Y,Z), X::3.0:3, Y:: -3.0: -3.0, Z:: -9: -9, 

Q: A::(-3):3, B::(-3):3, C::4:4,  mult(A,B,C), A eq B, label(A).
A: A:: -2.0000000000262146: -1.9999897600262142,
    B:: -2.0000000000262146: -1.9999897600262142,
    C::4:4, mult(A,B,C), A eq B, label(A).
A: A::1.9999897600262142:2.0000000000262146,
    B::1.9999897600262142:2.0000000000262146,
    C::4:4, mult(A,B,C), A eq B, label(A).

Q: int(A),int(B),int(C), mult(A,B,C), A::0:0.3, B::0:0.3, C::0:0.3, 
    A eq B, B eq C.
A: int(A),int(B),int(C), mult(A,B,C), A::0:0, B::0:0, C::0:0,
    A eq B, B eq C.
*/

:- module(interval, [(::)/2, le/2, eq/2, ne/2, add/3, mult/3, int/1, bool/1, 
    split0/1, split/1, label/1]).
:- use_module( library(chr)).

% for domain constraints
:- op( 700,xfx,'::').
:- op( 600,xfx,':').
% for inequality constraints
:- op( 700,xfx,le).
:- op( 700,xfx,ne).
:- op( 700,xfx,eq). 

%% Deprecated syntax used for SICStus 3.x
%handler interval.
%constraints 

%% Syntax for SWI / SICStus 4.x
:- chr_constraint

% X::Min:Max - X is between the numbers Min and Max, inclusively
% X must always be an unbound variable(!), and Min and Max evaluable
% (i.e. ground) arithmetic expressions (or numbers)
    (::)/2, 
    le/2, eq/2, ne/2, add/3, mult/3,
% int(X) says that X is an integer (default is a real)
    int/1,
% bool(X) says that X is a boolean (default is a real)
    bool/1,

    split0/1,
    split/1,
    label/1,    % repeated split/1

    browse/1.   % watch how domain of X evolves


% Auxiliary -----------------------------------

browse(X), X::A:B ==> write((X::A:B)),nl.

% define the smallest intervals you want to get:
% the smaller, the more precise, the longer the computation
small(A:B) :- A+2.0e-05>=B. 

% is zero part of A:B ?
withzero(A:B) :- A=<0, 0=<B.


% Intersection  -------------------------------

redundant @ X::A:B \ X::C:D <=>
        (C=<A, B=<D ; A<B,small(A:B), C<D,small(C:D)) | true.

intersect @ X::A:B , X::C:D <=>
        X::max(A,C):min(B,D).


% Special Cases  -------------------------------

failure @ _::A:B <=> A>B | fail.

compute @ X::A:B <=> \+ (number(A),number(B)) | C is A, D is B, X::C:D.

integer @ int(X), X::A:B ==> \+ (integer(A),integer(B)) | 
      C is integer(ceiling(A)), D is integer(floor(B)), X::C:D.

bool @ bool(X), X::_:B ==> B<1 | X::0:0.
bool @ bool(X), X::A:_ ==> A>0 | X::1:1.
bool @ bool(X) ==> X::0:1.


% Inequality -------------------------------

(le) @ X le Y, X::A:_, Y::_:D ==> Y::A:D, X::A:D.
(eq) @ X eq Y, X::A:B, Y::C:D ==> Y::A:B, X::C:D.
(ne) @ X ne Y, X::A:A, Y::A:A <=> fail.

(ne_int) @ int(X) \ X ne Y, X::A:B <=> A=Y | X::A+1:B.
(ne_int) @ int(X) \ X ne Y, X::A:B <=> B=Y | X::A:B-1.
(ne_int) @ int(X) \ Y ne X, X::A:B <=> A=Y | X::A+1:B.
(ne_int) @ int(X) \ Y ne X, X::A:B <=> B=Y | X::A:B-1.


% Addition X+Y=Z -------------------------------

add @ add(X,Y,Z), X::A:B, Y::C:D, Z::E:F ==>
        X::E-D:F-C, Y::E-B:F-A, Z::A+C:B+D.


% Multiplication X*Y=Z -------------------------------

mult_z @ mult(X,Y,Z), X::A:B, Y::C:D ==> 
         M1 is A*C, M2 is A*D, M3 is B*C, M4 is B*D,
         Z::min(min(M1,M2),min(M3,M4)):max(max(M1,M2),max(M3,M4)).

mult_y @ mult(X,Y,Z), X::A:B, Z::E:F ==>
         \+ withzero(A:B) |
         M1 is E/A, M2 is E/B, M3 is F/A, M4 is F/B,
         Y::min(min(M1,M2),min(M3,M4)):max(max(M1,M2),max(M3,M4)).
mult_x @ mult(Y,X,Z), X::A:B, Z::E:F ==>
         \+ withzero(A:B) |
         M1 is E/A, M2 is E/B, M3 is F/A, M4 is F/B,
         Y::min(min(M1,M2),min(M3,M4)):max(max(M1,M2),max(M3,M4)).

mult_xyz @ mult(X,Y,Z), X::A:B, Y::C:D, Z::E:F ==>
         withzero(A:B), withzero(C:D), \+ withzero(E:F) |
         (A*C<E -> D>0, X::E/D:B ; true),
         (B*D<E -> C<0, X::A:E/C ; true),
         (F<A*D -> C<0, X::F/C:B ; true),
         (F<B*C -> D>0, X::A:F/D ; true).


% Labeling --------------------------------------------------------

label @ split0(X), X::A:B <=> \+ small(A:B), A<0,0<B |
           (X::A:0 ; X::0:B). 

label @ split(X), X::A:B <=> \+ small(A:B) |
           Half is (A+B)/2,
           (X::A:Half ; X::Half:B).

label @ label(X), X::A:B <=> \+ small(A:B) |
           Half is (A+B)/2,
           (X::A:Half ; X::Half:B), 
           label(X).    

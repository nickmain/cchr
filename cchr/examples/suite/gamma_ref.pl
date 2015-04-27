/*
gamma_gen.pl: Gamma-to-CHR examples references (hand-translated version)
(C) Hariolf.Betz at uni-ulm.de, 2006/02/01
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
GAMMA is an acronym for General Abstract Model for Multiset 
Manipulation. It is a programming paradigm relying solely on multiset 
transformation.

A GAMMA program necessarily contains a multiset S, a predicate 
R(x1,...,xn) and a function A(x1,...,xn). S gives the data on which to 
operate. R is a condition that may or may not be fulfilled by x1,...,xn, 
which is a sequence of elements of S. A is a function that will be 
applied on x1,...,xn, in case R(x1,...,xn) is true. The result of 
A(x1,...,xn) will replace the x1,...,xn in S.

A GAMMA program is of the form:
identifier(args) = G((R,A)) (S) where
     R(x,y) = ...
     A(x,y) = ...

As for an example, the following GAMMA program calculates the faculty of 
a natural number N:
fact(N) = G((R,A)) ({1, ..., N}) where
     R(x,y) = true
     A(x,y) = {x*y}

This file is for your reference. It contains more conservative CHR versions 
of the GAMMA programs from [1] than "Gamma_gen.pl". They are closer to the 
usual CHR style of programming and probably much better readable.


%% HOW TO USE
These four small programs can be used:
- prime(N): prime numbers
Returns all prime numbers Pi up to N by prime_M(Pi).
- fact(N): factorial
Returns the factorial N! by fact_M(N!).
- sp(L1,L2): partition into sets
Partitions the elements of L1 and L2 into sets S and T (with size(S)=size(L1))
so that all elements from S are smaller than those from T.
E belongs to S is represented: sp_M((E,inS)).
E belongs to T is represented: sp_M((E,inT)).
- so(L): sort
List L (n elements) is sorted to [S1,S2,..Sn]: so_M((i-1,Si)).


%% SEE ALSO
File "Gamma_gen.pl" for the directly translated versions.
[1] JP Banatre, D Le Metayer. Programming by Multiset Transformation. 
Communications of the ACM, 1993.


%% SAMPLE QUERIES
Q: prime(7).
A: prime_M(2), prime_M(3), prime_M(5), prime_M(7).

Q: fact(5).
A: fact_M(120).

Q: sp([1,7,8],[4,5]).
A: sp_M((1,inS)), sp_M((4,inS)), sp_M((5,inS)), sp_M((7,inT)), sp_M((8,inT)).

Q: so([1,7,5,44,2]).
A: so_M((0,1)), so_M((1,2)), so_M((2,5)), so_M((3,7)), so_M((4,44)).
*/


:- module(gamma_ref, 
    [prime/1, prime_M/1, fact/1, fact_M/1, sp/2, sp_M/1, so/1, so_M/1]).
:- use_module(library(chr)).

%% Deprecated syntax used for SICStus 3.x
%handler gamma_ref.
%constraints 
%% Syntax for SWI / SICStus 4.x
:- chr_constraint

           prime/1, prime_M/1,
           fact/1, fact_M/1,
           sp/2, sp_M/1,
           so/1, so/2, so_M/1.


%---prime---

multiple(X,Y):- X mod Y=:=0.

prime(2) <=> prime_M(2).
prime(N) <=> N>2 |
           prime_M(N),
           N1 is N-1,
           prime(N1).

prime_M(X), prime_M(Y) <=>
           multiple(X,Y) |
           prime_M(Y).


% Original GAMMA program:
% (G replaces the '\Gamma' character)
%
% prime_numbers(N) = G((R,A)) ({2, ..., N}) where
%    R(x,y) = multiple(x,y)
%    A(x,y) = {y}


%---fact---

fact(1) <=> fact_M(1).
fact(N) <=> N>1 |
           fact_M(N),
           N1 is N-1,
           fact(N1).

fact_M(X), fact_M(Y) <=> A is X*Y, fact_M(A).


% Original GAMMA program:
%
% fact(N) = G((R,A)) ({1, ..., N}) where
%    R(x,y) = true
%    A(x,y) = {x*y}


%---sp---

sp([],[]) <=> true.
sp([S1|S],T) <=> sp_M((S1,inS)), sp(S,T).
sp(S,[T1|T]) <=> sp_M((T1,inT)), sp(S,T).

sp_M((X,inS)),sp_M((Y,inT)) <=>
           X>Y |
           sp_M((X,inT)),sp_M((Y,inS)).


% Original GAMMA program:
%
% sp(S,T) = G((R,A)) (S x {inS} + T x {inT}) where
%    R((x,inS), (y,inT)) = x>y
%    A((x,inS), (y,inT)) = {(x,inT),(y,inS)}


%---sort---

so(A) <=> so(0,A).
so(_,[]) <=> true.
so(N,[H|T]) <=>
           so_M((N,H)),
           N1 is N+1,
           so(N1,T).

so_M((I,V)), so_M((J,W)) <=>
           I>J, V<W |
           so_M((I,W)), so_M((J,V)).


% Original GAMMA program:
% (G replaces the '\Gamma' character)
%
% sort(Array) = G((R,A)) (Array) where
%    R((i,v), (j,w)) = (i>j) and (v<w)
%    A((i,v), (j,w)) = {(i,w),(j,v)}


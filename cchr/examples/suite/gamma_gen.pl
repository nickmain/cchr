/*
gamma_gen.pl: Gamma-to-CHR transformation
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

This file contains a number of translated GAMMA programs from a 1993 
journal paper[1] into CHR. This is done in a manner such that it could 
be the result of a source-to-source transformation from GAMMA to CHR. 
Refer to [1] also for specific information on GAMMA.


%% HOW TO USE
These four small programs can be used:
- gamma(prime, N): prime numbers
Returns all prime numbers Pi up to N by s(prime, Pi).
- gamma(fact, N): factorial
Returns the factorial N! by s(fact, N!).
- gamma(sp, (L1,L2)): partition into sets
Partitions the elements of L1 and L2 into sets S and T (with size(S)=size(L1))
so that all elements from S are smaller than those from T.
E belongs to S is represented: s(sp, (E,inS)).
E belongs to T is represented: s(sp, (E,inT)).
- gamma(sort, (L)): sort
List L (n elements) is sorted to [S1,S2,..Sn]: s(sort, (i-1,Si)).


%% SEE ALSO
File "Gamma_ref.pl" for more conservative and better readable versions.
[1] JP Banatre, D Le Metayer. Programming by Multiset Transformation. 
Communications of the ACM, 1993.


%% SAMPLE QUERIES
Q: gamma(prime, 7).
A: s(prime,2), s(prime,3), s(prime,5), s(prime,7).

Q: gamma(fact, 5).
A: s(fact,120).

Q: gamma(sp, ([1,7,8],[4,5])).
A: s(sp,(1,inS)), s(sp,(4,inS)), s(sp,(5,inS)), s(sp,(7,inT)), s(sp,(8,inT)).

Q: gamma(sort, ([1,7,5,44,2])).
A: s(sort,(0,1)), s(sort,(1,2)), s(sort,(2,5)), s(sort,(3,7)), s(sort,(4,44)).
*/


:- module(gamma_gen, [gamma/2, s/2]).
:- use_module(library(chr)).

%% Deprecated syntax used for SICStus 3.x
%handler gamma_gen.
%constraints gamma/2, a/2, s/2.

%% Syntax for SWI / SICStus 4.x
:- chr_constraint gamma/2, a/2, s/2.

%%
%% This could be the output of a yet-to-do Gamma-2-CHR STS transformer
%%

%%%---general part---%%%

%---set construction---

s(ID, interval(A,A)) <=> s(ID, A), true.
s(ID, interval(A,B)) <=> A<B | s(ID, A), A1 is A+1, s(ID, interval(A1,B)).

s(ID, array(A)) <=> s(ID, array(0,A)).
s(ID, array(_,[])) <=> true.
s(ID, array(N, [H|T]) ) <=> s(ID, (N,H)), N1 is N+1, s(ID, array(N1,T)).

%---set operations---

s(ID, union(X,Y)) <=> s(ID,X), s(ID,Y).

s(ID, cart([X1|X],Y)) <=> X\=[] | s(ID, cart([X1],Y)), s(ID, cart(X,Y)).
s(ID, cart([X],[Y1|Y])) <=> s(ID, (X,Y1)), s(ID, cart([X],Y)).
s(ID, cart(_,[])) <=> true.

%---gamma operational semantics---

s(ID,X), s(ID,Y) <=> r(ID, (X,Y)) | a(ID, (X,Y)).



%%%---specific part---%%%

%---prime---

multiple(X,Y):- X mod Y=:=0.

gamma(prime,N) <=> s(prime, interval(2,N)).
a(prime, (_,Y)) <=> s(prime, Y).
r(prime, (X,Y)):- multiple(X,Y).


% Original GAMMA program:
% (G replaces the '\Gamma' character)
%
% prime_numbers(N) = G((R,A)) ({2, ..., N}) where
%    R(x,y) = multiple(x,y)
%    A(x,y) = {y}


%---fact---

gamma(fact,N) <=> s(fact, interval(1,N)).
a(fact, (X,Y)) <=> A is X*Y, s(fact, A).
r(fact, _):- true.


% Original GAMMA program:
% (G replaces the '\Gamma' character)
%
% fact(N) = G((R,A)) ({1, ..., N}) where
%    R(x,y) = true
%    A(x,y) = {x*y}


%---sp---

gamma(sp,(S,T)) <=> s(sp, union(cart(S,[inS]),cart(T,[inT]) )).
a(sp, ((X,inS),(Y,inT)) ) <=> s(sp, (Y,inS)), s(sp,(X,inT)).
r(sp, ((X,inS),(Y,inT)) ) :- X>Y.


% Original GAMMA program:
% (G replaces the '\Gamma' character)
%
% sp(S,T) = G((R,A)) (S x {inS} + T x {inT}) where
%    R((x,inS), (y,inT)) = x>y
%    A((x,inS), (y,inT)) = {(x,inT),(y,inS)}


%---sort---

gamma(sort,A) <=> s(sort, array(A)).
a(sort, ((I,V),(J,W)) ) <=> s(sort, (I,W)), s(sort, (J,V)).
r(sort, ((I,V),(J,W)) ) :- I<J, V>W.


% Original GAMMA program:
% (G replaces the '\Gamma' character)
%
% sort(Array) = G((R,A)) (Array) where
%    R((i,v), (j,w)) = (i>j) and (v<w)
%    A((i,v), (j,w)) = {(i,w),(j,v)}


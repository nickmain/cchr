/*
unionfind.pl: Union find
(C) Thom.Fruehwirth at uni-ulm.de, 2005/01/25
(C) Tom.Schrijvers at cs.kuleuven.be, 2005/01/25
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Naive union-find implementation.
A new set can be created, sets can be united, and the representative for a set
can be searched.

%% HOW TO USE
The following operations are supported:
  make(A):    create a new set with single element A
  find(A,B):  B is representative of the set containing A
  union(A,B): join the two sets containing A and B
  link(A,B):  join the two sets represented by A and B (internal)
Data is represented as:
  root(A):    A is the representative of a set (root of tree)
  A ~> B:     A and B are in the same set (edge indirectly pointing to root)

%% SEE ALSO
T Schrijvers, T Fruehwirth. Optimal union-find in Constraint Handling Rules. 
Theory and Practice of Logic Programming, 2006.

%% SAMPLE QUERIES
Q: make(a), make(b), make(c), make(d), make(e), union(a,b), union(c,d), 
    union(e,c).
A: root(a), root(e), b~>a, d~>c, c~>e.

Q: make(a), make(b), make(c), make(d), make(e), union(a,b), union(c,d), 
    union(e,c), find(b,X), find(d,Y).
A: X=a, Y=e, root(a), root(e), b~>a, d~>c, c~>e.
*/

:- module(unionfind,[make/1,union/2,find/2,root/1,(~>)/2]).
:- use_module(library(chr)).

:- op(700, xfx, '~>').

%% Deprecated syntax used for SICStus 3.x
%handler unionfind.
%constraints
%    make/1,
%    find/2,
%    union/2,
%    (~>)/2,
%    link/2,
%    root/1.

%% Syntax for SWI / SICStus 4.x
:- chr_constraint 
    make/1,
    find/2,
    union/2,
    (~>)/2,
    link/2,
    root/1.


make     @ make(A) <=> root(A).

union    @ union(A,B) <=> find(A,X), find(B,Y), link(X,Y).

findNode @ A ~> B \ find(A,X) <=> find(B,X).  
findRoot @ root(B) \ find(B,X) <=> X=B.

linkEq   @ link(A,A) <=> true.  
link     @ link(A,B), root(A), root(B) <=>  B ~> A, root(A).

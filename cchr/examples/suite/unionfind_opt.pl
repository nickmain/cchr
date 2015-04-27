/*
unionfind_opt.pl: Union find (optimised)
(C) Thom.Fruehwirth at uni-ulm.de, 2005/01/25
(C) Tom.Schrijvers at cs.kuleuven.be, 2005/01/25
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Optimised union-find implementation.
A new set can be created, sets can be united, and the representative for a set
can be searched.

%% HOW TO USE
The following operations are supported:
  make(A):    create a new set with single element A
  find(A,B):  B is representative of the set containing A
  union(A,B): join the two sets containing A and B
  link(A,B):  join the two sets represented by A and B (internal)
Data is represented as:
  root(A,N):  A is the representative of a set (root of tree with depth N)
  A ~> B:     A and B are in the same set (edge indirectly pointing to root)

%% SEE ALSO
T Schrijvers, T Fruehwirth. Optimal union-find in Constraint Handling Rules. 
Theory and Practice of Logic Programming, 2006.

%% SAMPLE QUERIES
Q: make(a), make(b), make(c), make(d), make(e), union(a,b), union(c,d), 
    union(e,c).
A: root(a,1), root(c,1), b~>a, d~>c, e~>c.

Q: make(a), make(b), make(c), make(d), make(e), union(a,b), union(c,d), 
    union(e,c), find(b,X), find(d,Y).
A: X=a, Y=c, root(a,1), root(c,1), b~>a, d~>c, e~>c.
*/

:- module(unionfind_opt, [make/1,union/2,find/2,root/2,(~>)/2]).
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
%    root/2.

%% Syntax for SWI / SICStus 4.x
:- chr_constraint
    make(+element),
    find(?element,?element),
    union(+element,+element),
    (?element) ~> (+element),
    link(+element,?element),
    root(+element,?natural).

%:- chr_type element == dense_int.          % efficient: arrays
:- chr_type element == int.                % less efficient: hashtables


make     @ make(A) <=> root(A,0).

union    @ union(A,B) <=> find(A,X), find(B,Y), link(X,Y).

findNode @ A ~> B, find(A,X) <=> find(B,X), A ~> X.
findRoot @ root(B,_) \ find(B,X) <=> X=B.

linkEq   @ link(A,A) <=> true.  
linkLeft @ link(A,B), root(A,NA), root(B,NB) <=> NA>=NB | 
                B ~> A, NA1 is max(NA,NB+1), root(A,NA1).
linkRight@ link(B,A), root(A,NA), root(B,NB) <=> NA>=NB |
                B ~> A, NA1 is max(NA,NB+1), root(A,NA1).

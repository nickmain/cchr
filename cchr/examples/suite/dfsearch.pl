/*
dfsearch.pl: depth first search in tree
(C) Jon.Sneyers at cs.kuleuven.be, 2004
(C) Tom.Schrijvers at cs.kuleuven.be, 2004
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Depth first search for value in binary tree.

%% HOW TO USE
V is a value, T, L, R are trees. A tree has one of two forms:
    node(V,L,R)  or
    leaf(V)
A depth first search for value V in tree T is invoked by
    dfsearch(T, V).

%% SAMPLE QUERIES
Q: dfsearch(node(0,node(1,leaf(2),leaf(3)),leaf(4)), 0).
A: yes.

Q: dfsearch(node(0,node(1,leaf(2),leaf(3)),leaf(4)), 4).
A: yes.

Q: dfsearch(node(0,node(1,leaf(2),leaf(3)),leaf(4)), 5).
A: no.
*/

:- module(dfsearch, [dfsearch/2]).
:- use_module(library(chr)).

%% Deprecated syntax used for SICStus 3.x
%handler dfsearch.
%constraints dfsearch/2.

%% Syntax for SWI / SICStus 4.x
:- chr_type 
    tree(X) ---> node(X,tree(X),tree(X)) ; leaf(X).
:- chr_constraint
    dfsearch(+tree(number), +number).


leaf        @ dfsearch(leaf(Val),X) <=> X == Val.
node_found  @ dfsearch(node(Val,_,_),Val) <=> true.
node_search @ dfsearch(node(Val,L,R),X) <=> X \== Val | 
                (dfsearch(L,X) ; dfsearch(R,X)).

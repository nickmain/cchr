/*
transitive_closure_2.pl: Transitive Closure w/candidates
(C) Thom.Fruehwirth at uni-ulm.de
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Computes the transitive closure 'path' for a binary relation 'edge', e.g. all 
the paths along the edges of a graph. After creating all possible candidates 
for closure from the given nodes, the inappropriate ones are removed 
afterwards. Terminates even under abstract semantics.

%% HOW TO USE
For a relation over the set N1, .., Np containing the pairs (A1,B1), .., 
(An,Bn), you enter
  node(N1), .., node(Np),  edge(A1,B1), .., edge(An,Bn),
and get the pairs (C1,D1), .., (Cm,Dm) of its transitive closure
  path(C1,D1), .., path(Cm,Dm).

%% SEE ALSO
Thom Fruehwirth. Rule-Based Programming with CHR. 2006.

%% SAMPLE QUERIES
Q: edge(1,2), edge(2,3), edge(1,3), node(1), node(2), node(3).
A: edge(1,2), edge(2,3), edge(1,3), node(1), node(2), node(3), 
    candidate_path(2,1), candidate_path(3,2), candidate_path(3,1), path(1,2), 
    path(2,3), path(1,3). 

Q: edge(1,2), edge(2,3), edge(3,1), node(1), node(2), node(3).
A: edge(1,2), edge(2,3), edge(3,1), node(1), node(2), node(3), path(1,2), 
    path(2,3), path(1,3), path(3,1), path(2,1), path(3,2).
*/

:- module(transite_closure_2, [edge/2, path/2, candidate_path/2, node/1]).
:- use_module(library(chr)).

%% Deprecated syntax used for SICStus 3.x
%handler tc2.
%constraints edge/2, path/2, candidate_path/2, node/1.

%% Syntax for SWI / SICStus 4.x
:- chr_constraint edge/2, path/2, candidate_path/2, node/1.


gen_cand @ node(X), node(Y) ==> candidate_path(X,Y).
path_1   @ edge(X,Y) \ candidate_path(X,Y) <=> path(X,Y).
path_add @ edge(X,Y), path(Y,Z) \ candidate_path(X,Z) <=> 
                                            X\==Y, Y\==Z | path(X,Z).

/*
transitive_closure_1.pl: Transitive Closure
(C) Thom.Fruehwirth at uni-ulm.de
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Computes the transitive closure 'path' for a binary relation 'edge', e.g. all 
the paths along the edges of a graph. Terminates only under refined semantics.

%% HOW TO USE
For a relation containing the pairs (A1,B1), .., (An,Bn), you enter
  edge(A1,B1), .., edge(An,Bn),
and get the pairs (C1,D1), .., (Cm,Dm) of its transitive closure
  path(C1,D1), .., path(Cm,Dm).

%% SEE ALSO
Thom Fruehwirth. Rule-Based Programming with CHR. 2006.

%% SAMPLE QUERIES
Q: edge(1,2), edge(2,3), edge(2,4).
A: edge(1,2), path(1,2), edge(2,3), path(2,3), path(1,3), edge(2,4), 
    path(2,4), path(1,4).

Q: edge(1,2), edge(2,3), edge(1,3).
A: edge(1,2), path(1,2), edge(2,3), path(2,3), path(1,3), edge(1,3).

Q: edge(1,2), edge(2,3), edge(3,1).
A: edge(1,2), path(1,2), edge(2,3), path(2,3), path(1,3), edge(3,1), 
    path(3,1), path(2,1), path(1,1), path(3,3), path(3,2), path(2,2).
*/

:- module(transitive_closure_1, [edge/2, path/2]).
:- use_module(library(chr)).

%% Deprecated syntax used for SICStus 3.x
%handler tc1.
%constraints edge/2, path/2.

%% Syntax for SWI / SICStus 4.x
:- chr_constraint edge/2, path/2.


rem_dup  @ path(X,Y) \ path(X,Y) <=> true.
path_1   @ edge(X,Y) ==> path(X,Y).
path_add @ edge(X,Y), path(Y,Z) ==> X\==Y, Y\==Z | path(X,Z).

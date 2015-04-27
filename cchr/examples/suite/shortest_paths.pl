/*
shortest_paths.pl: All-pairs shortest paths of a graph
(C) Thom.Fruehwirth at uni-ulm.de, LMU, 980129, 980311
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Computes for a directed graph the shortest length for each path.

%% HOW TO USE
For a directed graph with edges A1->B1, A2->B2, .., An->Bn
  edge(A1,B1), edge(A2,B2), .., edge(An,Bn),
you get all shortest paths from Xi to Yi with length Li (1<=i<=m)
  path(X1,Y1,L1), .., path(Xm,Ym,Lm).

%% SAMPLE QUERIES
Q: edge(A,B), edge(B,C).
A: edge(A,B), edge(B,C), path(A,B,1), path(B,C,1), path(A,C,2).

Q: edge(a,b), edge(b,c), edge(c,a).
A: edge(a,b), edge(b,c), edge(c,a), path(a,b,1), path(b,c,1), path(a,c,2), 
    path(c,a,1), path(b,a,2), path(a,a,3), path(c,c,3), path(c,b,2), 
    path(b,b,3).

Q: edge(a,b), edge(b,c), edge(c,a), edge(a,c).
A: edge(a,b), edge(b,c), edge(c,a), edge(a,c), path(a,b,1), path(b,c,1), 
    path(c,a,1), path(b,a,2), path(c,b,2), path(b,b,3), path(a,c,1), 
    path(c,c,2), path(a,a,2).
*/

:- module(shortest_paths, [edge/2, path/3]).
:- use_module(library(chr)).

%% Deprecated syntax used for SICStus 3.x
%handler shortest_paths.
%constraints edge/2, path/3.

%% Syntax for SWI / SICStus 4.x
:- chr_constraint edge/2, path/3.


% remove cyclic paths
%rem_cyc  @ path(X,X,_) <=> true.

% keep shorter of two paths from X to Y (termination!)
rem_long @ path(X,Y,L1) \ path(X,Y,L2) <=> L1=<L2 | true.

% generate paths of length one
path_1   @ edge(X,Y) ==> path(X,Y,1).

% extend path by one edge
path_add @ edge(X,Y), path(Y,Z,L) ==> L1 is L+1 | path(X,Z,L1).

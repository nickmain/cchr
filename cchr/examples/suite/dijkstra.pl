/*
dijkstra.pl: Dijkstra's single-source shortest path algorithm
(C) Jon.Sneyers at cs.kuleuven.be, 2006
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Single-source shortest path algorithm based on [Dijkstra 1959].
Given a weighted directed graph and a source node, shortest distances to all 
other nodes are computed.
Nodes are numbered from 1, weights can be any numeric type.

%% HOW TO USE
The graph is represented using edge/3 constraints:
  edge(A,B,W):      there is an edge between node A and node B, with weight W
The following operation is supported:
  dijkstra(S):      compute all distances from source node S
The resulting output is represented as:
  distance(B,D):    the distance from the source node to node B is D  
Internal operations are:
  scan(N,L):        scan the neighbours of node N, which is at distance L 
                    from the source node, then get the next non-scanned node 
                    with the smallest label (distance)
  relabel(N,L):     if N is already scanned, do nothing; otherwise set the 
                    label of N to L

%% SEE ALSO
Uses Fibonnacci heaps (see fib_heap.pl).
Reference paper:
  Dijkstra's Algorithm with Fibonacci Heaps: An Executable Description in CHR.
  Jon Sneyers, Tom Schrijvers and Bart Demoen. WLP 2006.
See also shortestpaths.pl for an all-pairs shortest path algorithm.

%% SAMPLE QUERY
Q: edge(1,2,3),edge(2,4,8),edge(1,3,5),edge(3,4,2),edge(2,3,1), dijkstra(1).
A: edge(1,3,5), edge(1,2,3), edge(2,3,1), edge(2,4,8), edge(3,4,2),
   distance(1,0), distance(2,3), distance(3,4), distance(4,6).
*/

:- module(dijkstra,[edge/3,dijkstra/1,distance/2]).
:- use_module(library(chr)).
:- use_module(fib_heap).

%% Deprecated syntax used for SICStus 3.x
%handler dijkstra.
%constraints
%    edge/3, dijkstra/1, distance/2, scan/2, relabel/2.

%% Syntax for SWI / SICStus 4.x
:- chr_constraint
    edge(?node,+int,+number),
    dijkstra(+int),
    distance(?node,+number),
    scan(+int,+number),
    relabel(+int,+number).

%:- chr_type node == dense_int.          % efficient: arrays
:- chr_type node == int.                % less efficient: hashtables


start_scanning @ dijkstra(A) <=> scan(A,0).
label_neighb @ scan(N,L), edge(N,N2,W) ==> L2 is L+W, relabel(N2,L2).
scan_next    @ scan(N,L) <=> distance(N,L), 
                            (extract_min(N2,L2) -> scan(N2,L2) ; true).
scanned     @ distance(N,_) \ relabel(N,_) <=> true.
not_scanned @ relabel(N,L) <=> decr_or_ins(N,L).

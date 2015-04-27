/*
lexicographic.pl: Lexicographic order
(C) Thom.Fruehwirth at uni-ulm.de, 2004/11/12
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Confluent, efficient and complete handler for Lexicographic order constraints 
for lists of atoms.

%% HOW TO USE
For two ground lists A and B,
  A lex B
indicates that A is lexicographically smaller than B.

%% SAMPLE QUERIES
Q: "alpha" lex "alternative".
A: yes.

Q: "alternative" lex "alpha".
A: no.

Q: "alpha" lex "alpha".
A: yes.

Q: "alpha" lex "alphabet".
A: yes.

Q: "alphabet" lex "alpha".
A: no.

Q: [1] lex [2].
A: yes.
*/

:- module(lex, [lex/2]).
:- use_module(library(chr)).

:- op(700, xfx, lex).

%% Deprecated syntax used for SICStus 3.x
%handler lex.
%constraints lex/2.

%% Syntax for SWI / SICStus 4.x
:- chr_type list(X) ---> [] ; [X|list(X)].
:- chr_constraint 
    (+list(int)) lex (+list(int)).


l1 @ []     lex _       <=> true.
l2 @ [_|_]  lex []      <=> fail.
l3 @ [X|_]  lex [Y|_]   <=> X@<Y | true.
l4 @ [X|L1] lex [Y|L2]  <=> X==Y | L1 lex L2.
l5 @ [X|_]  lex [Y|_]   <=> X@>Y | fail.

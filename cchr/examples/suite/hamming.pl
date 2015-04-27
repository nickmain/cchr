/*
hamming.pl: solves Hamming Problem
(C) Thom.Fruehwirth at uni-ulm.de, 1997/02/24
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Solves the Hamming Problem which is to build the infinite ascending sequence 
of positive numbers containing no prime factors other than 2, 3 and 5 
(cf. http://en.wikipedia.org/wiki/Hamming_problem).

%% HOW TO USE
The constraint hamming/0 the unlimited sequence of Hamming numbers.
As the sequence is unlimited the program does not terminate!

%% SAMPLE QUERIES
Q: hamming.
A: not terminating!
%% 1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, ...
*/

:- module(hamming, [hamming/0]).
:- use_module(library(chr)).

%% Deprecated syntax used for SICStus 3.x
%handler hamming.
%constraints hamming/0,watch/1,multlist/3,merge/3.

%% Syntax for SWI / SICStus 4.x
:- chr_constraint hamming/0,watch/1,multlist/3,merge/3.


%% hamming
%% the list of Hamming numbers is constructed and printed in parallel
hamming <=>
     watch(L),
     L=[1|L235],
     multlist(L,2,L2),
     multlist(L,3,L3),
     multlist(L,5,L5),
     merge(L2,L3,L23),
     merge(L5,L23,L235).

%% watch(L)
%% print elements of list L recursively
watch([X|L]) <=> print(X),nl, watch(L).

%% multlist(L,N,LN)
%% multiply each element of list L with N, resulting in list LN
%% here only do multiplication for 1st element, then use multlist recursively
multlist([X|L],N,XLN) <=> XN is X*N, XLN=[XN|LN], multlist(L,N,LN).

%% merge(In1,In2,Out)
%% merge sorted lists In1 and In2 into sorted list Out recursively
merge([X|In1],[Y|In2],XYOut) <=> X=:=Y | XYOut=[X|Out], merge(In1,In2,Out).
merge([X|In1],[Y|In2],XYOut) <=> X<Y | XYOut=[X|Out], merge(In1,[Y|In2],Out).
merge([X|In1],[Y|In2],XYOut) <=> X>Y | XYOut=[Y|Out], merge([X|In1],In2,Out).

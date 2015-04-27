/*
minset.pl: Minimum of set of numbers
(C) Thom.Fruehwirth at uni-ulm.de, 2002
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Calculates minimum of a set of numbers.

%% HOW TO USE
For numbers I1, .., In, you enter
  min(I1), .., min(In),
and get its minimum Ix as single remaining constraint
  min(Ix).

%% SAMPLE QUERIES
Q: min(1), min(2), min(0).
A: min(0).

Q: min(3), min(5), min(5), min(3).
A: min(3).
*/

:- module(minset, [min/1]).
:- use_module(library(chr)).

%% Deprecated syntax used for SICStus 3.x
%handler minset.
%constraints min/1.
%% Syntax for SWI / SICStus 4.x
:- chr_constraint min(+int).

min(I) \ min(J) <=> J>=I | true.     % works only with numbers

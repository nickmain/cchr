/*
time_point.pl: time point constraints
(C) Thom.Fruehwirth at uni-ulm.de, 1991-92, 93/12/23, LMU 96/10/28, 98/03/12
(C) Christian Holzbaur, SICStus mods 96/11/05
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Handles constraints about the approximate position and difference of time 
points.

%% HOW TO USE
Time is represented by float numbers, time points by variables.
The following constraints are used:
Time point P is between time T1 and T2: pos(T1,P,T2)
Distance of points P1 and P2 is between T1 and T2: dist(T1,P1,P2,T2)
P is the origin: start(P) (equivalent: pos(0,P,0))

%% SAMPLE QUERIES
Q: start(A), dist(4,A,B,7), dist(1,B,C,4), pos(10,C,13).
A: pos(0,A,0), dist(4,A,B,7), dist(1,B,C,4), pos(10,C,11), pos(6,B,7).

Q: start(A), dist(4,A,B,7), dist(1,B,C,2), pos(10,C,13).
A: no.

Q: start(X), dist(3,X,Y,10), dist(4,Y,Z,5).
A: pos(0,X,0), dist(3,X,Y,10), dist(4,Y,Z,5), pos(3,Y,10), pos(7,Z,15).

Q: start(A), dist(14.3,A,B,17.5), dist(14.3,A,B,16.3), dist(12.2,B,C,14.5).
A: pos(0.0,A,0.0), dist(14.3,A,B,16.3), dist(12.2,B,C,14.5),
    pos(14.3,B,16.3), pos(26.5,C,30.8).
*/

:- module(time_point, [pos/3, dist/4, start/1]).
:- use_module(library(chr)).

%% Deprecated syntax used for SICStus 3.x
%handler time_point.
%constraints 

%% Syntax for SWI / SICStus 4.x
:- chr_constraint

% pos(T1,P,T2) means: time point P is between time T1 and T2 (numbers)
    pos/3, 
% dist(T1,P1,P2,T2) means: distance of points P1 and P2 is between T1 and T2
    dist/4, 
% start(P) means: P is the origin, equivalent: pos(0,P,0)
    start/1.


start @             start(X)   <=> pos(0,X,0).

inconsistent @      pos(A,_,B) <=> A>B | fail.

%intersect @         pos(A,Y,B), pos(C,Y,D) <=> 
%                        AC is max(A,C), BD is min(B,D), pos(AC,Y,BD)
%                        pragma already_in_heads.  %important for termination!
%% avoid "pragma already_in_heads" through following two rules:

subset @            pos(A,Y,B) \ pos(C,Y,D) <=> 
                        A is max(A,C), B is min(B,D) | true.

intersect @         pos(A,Y,B), pos(C,Y,D) <=> 
                        AC is max(A,C), BD is min(B,D), pos(AC,Y,BD).

intersect_dist @    dist(A,X,Y,B), dist(C,X,Y,D) <=> 
                        AC is max(A,C), BD is min(B,D), dist(AC,X,Y,BD).

propagate_forward @ pos(A,Y,B), dist(C,Y,Z,D) ==> 
                        AC is A+C, BD is B+D, pos(AC,Z,BD).

propagate_backward@ pos(A,Y,B), dist(C,Z,Y,D) ==> 
                        AD is A-D, BC is B-C, pos(AD,Z,BC).

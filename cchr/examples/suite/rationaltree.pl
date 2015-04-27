/*
rationaltree.pl: rational (finite and infinite) tree handler
(C) Thom.Fruehwirth at uni-ulm.de, 1993-2006
(C) Marc.Meister at uni-ulm.de, 2006
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Handler for equality and inequality constraints between terms based on generic
strict total term order.

%% HOW TO USE
Equality between terms A and B is written as
  A~B,
inequality as
  A#~B,
labeling is invoked by
  label.

%% SEE ALSO
M Meister, T Fruehwirth. Complexity of the CHR Rational Tree Equation Solver. 
Proceedings of the Third Workshop on Constraint Handling Rules, June 2006.

%% SAMPLE QUERIES
Q: f(X,b)~f(a,Y).
A: X~a, Y~b.

Q: f(a,b)~f(X).
A: no.

Q: X~f(X), X~f(f(X)).
A: X~f(X).

Q: A~B,B~A,c~B.
A: A~B, B~c.

Q: X~f(Y,f(a,X)), X~f(a,X).
A: Y~a, X~f(a,X).

Q: X#~a,X~b.
A: X~b.

Q: A~B,B~C,C#~A.
A: no.

Q: f(X,Y)#~f(Y,X).
A: neq_list([X,Y],[Y,X]).

Q: f(X,Y)#~f(Y,X), label.
A: label, X#~Y.

Q: f(a,X,c)#~f(a,b,Y), X#~Y, label.
A: label, X#~Y, X#~b ;
A: label, Y#~b, Y#~c, X~b.
*/

:- module(rationaltree, [(~)/2, (#~)/2, neq_list/2, label/0]).
:- use_module(library(chr)).
:- use_module(library(lists)).
:- use_module(var_order).    % need stable order on variables

:- op(700,xfx,(~)).
:- op(700,xfx,(#~)).

%% Deprecated syntax used for SICStus 3.x
%handler rationaltree.
%constraints (~)/2, (#~)/2, eq_list/2, neq_list/2, label/0.

%% Syntax for SWI / SICStus 4.x
:- chr_constraint (~)/2, (#~)/2, eq_list/2, neq_list/2, label/0.

% T1 ~ T2 means: term T1 is syntactically equal to term T2
% T1 #~ T2 means: term T1 is syntactically different from term T2


eq_globalize        @ T1 ~ T2   ==> globalize(T1), globalize(T2).
neq_globalize       @ T1 #~ T2  ==> globalize(T1), globalize(T2).


eq_reflexivity      @ X ~ X     <=> true.
eq_orientation      @ T ~ X     <=> var(X), lss(X,T) | X ~ T.   
eq_decomposition    @ T1 ~ T2   <=> nonvar(T1),nonvar(T2) | 
                                    same_functions(T1,T2).
eq_confrontation    @ X ~ T1 \ X ~ T2   <=> var(X), lss(X,T1), leq(T1,T2) | 
                                            T1 ~ T2.


neq_reflexivity     @ X #~ X    <=> fail.
neq_orientation     @ T #~ X    <=> var(X), lss(X,T) | X #~ T. 
neq_decomposition   @ T1 #~ T2  <=> nonvar(T1), nonvar(T2) | 
                                    different_functions(T1,T2).
neq_confrontation   @ X ~ T1 \ X #~ T2  <=> T1 #~ T2. 


% two same-length lists must be equal, 
% i.e., every pair of elements must be equal
eq_list([],[])                <=> true.
eq_list([X|L1],[Y|L2])        <=> X ~ Y, eq_list(L1,L2).

% two same-length lists must not be equal, 
% i.e., at least one pair of elements must be different
neq_list([],[])               <=> fail.
neq_list([X],[Y])             <=> X #~ Y.
neq_list([X|L1],[X|L2])       <=> neq_list(L1,L2).
neq_list([X|L1],[Y|L2]), X~Y  <=> neq_list(L1,L2).

% label
label \ neq_list([X|L1],[Y|L2])#Id <=> true |  
                    (X #~ Y ; X ~ Y, neq_list(L1,L2))
                    pragma passive(Id).


%% Auxiliary

% lss(X,Y): X is smaller than Y by term-size order
lss(X,Y) :- var(X),var(Y), var_compare(<,X,Y).      % stable var order
lss(X,Y) :- var(X),nonvar(Y).
lss(X,Y) :- nonvar(X),nonvar(Y), termsize(X,M),termsize(Y,N),M<N.

% leq(X,Y): X is smaller-eq than Y by term-size order
leq(X,Y) :- \+ lss(Y,X).


% functions must be equal
same_functions(T1,T2) :-
    T1=..[F|L1], T2=..[F|L2], same_length(L1,L2), eq_list(L1,L2).

% functions must be different
different_functions(T1,T2) :-
    T1=..[F|L1], T2=..[F|L2], same_length(L1,L2), !, neq_list(L1,L2).
different_functions(_,_).

% termsize
termsize_list([],0).
termsize_list([X|L],N) :- termsize(X,N1), termsize_list(L,N2), N is N1+N2.
termsize(X,0) :- var(X).
termsize(T,1) :- atom(T).
termsize(T,N) :- compound(T), T=..L, termsize_list(L,N).

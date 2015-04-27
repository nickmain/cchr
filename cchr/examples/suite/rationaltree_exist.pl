/*
rationaltree_exist.pl: rational tree with exist. quantifiers
(C) Marc.Meister at uni-ulm.de, 2006/10
(C) Khalil.Djelloul at uni-ulm.de, 2006/10
(C) Thom.Fruehwirth at uni-ulm.de, 1993, 1998, 2006/10
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
CHR solver for existentially quantified conjunctions of non-flat equations
overs trees with quadratic complexity.
The algorithm includes three parts:
* Transforms the original equations into equivalent existentially quantified
flat equations.
* Applies the classic rationaltree solver (with term-order where existentially
quantified variables are smaller than free variables).
* The reachability of variables and equations is analysed, unreachable ones
are removed.

%% HOW TO USE
Existentially quantified variables are identified by 
    exists(V)
where V is a list of variables. This must be the first constraint.
Equality between trees A and B is written as 
    A eq B.
To trigger elimination of unreachable variables and equations the last
constraint must be
    elim.
In the answer the constraints exists_s(V) and A eq_s B (for solved) are used.

%% SAMPLE QUERIES
% note that we quantify *all* variables:
% the answer "yes" tells us that the problem has a solution
Q: exists([X,Y,Z,V]), f(X) eq f(g(X,Y)), Z eq f(V), Z eq f(f(Y)), elim.
A: yes.

% compared to the first example, variables X and V are now free:
% note our clear and explicit answer: 
% exists Y: V eq f(Y), X eq g(X,Y) with free variables V and X
Q: exists([Y,Z]), f(X) eq f(g(X,Y)), Z eq f(V), Z eq f(f(Y)), elim.
A: exists_s(Y), 
    V eq_s f(Y),
    X eq_s g(X,Y).

Q: exists([X,Y,U]), h(Y,f(a),g(X,a)) eq h(f(U),Y,g(h(Y),U)), elim.
A: yes.

Q: f(X) eq X, X eq f(f(X)), elim.
A: X eq_s f(X).

Q: a eq b,  elim.
A: no.
*/


:- module(rationaltree_exist, [exists/1, eq/2, exists_s/1, eq_s/2, elim/0]).
:- use_module(library(chr)).

:- op(700,xfx,[eq,eq_f,eq_s]).

%% Deprecated syntax used for SICStus 3.x
%handler rationaltree_exist.
%constraints

%% Syntax for SWI / SICStus 4.x
:- chr_constraint

    exists/1,           % INPUT: varlist is existentially quantified
    eq/2,               % INPUT: equality between terms
    elim/0,             % INPUT: EOL, triggers reachability

    exists_s/1,         % OUTPUT: reachable exist. quantified var 
    eq_s/2,             % OUTPUT: reachable flat equality constraint

    eq_f/2,             % INTERNAL: flat equality constraint
    reach/1,            % INTERNAL: reachable variable
    id/2,               % INTERNAL: id(V,(Type,Nr)): Type={exist,free,func}
    nextid/1,           % INTERNAL: next number to use as variable id
    reg_var/2,          % INTERNAL: reg_var(V,exist) or reg_var(V,free)
    reg_func/1.         % INTERNAL: reg_func(F)


%%% MINIMAL FLATTENING
%% convert equations to flattened form
%% A eq B (possibly non-flat equation) converted to form A eq_f B (flat)

% Meanwhile variables and functions occuring in the equations are registered:
% reg_var(X,exist): register X as existentially quantified variable
% reg_var(X,free): register X as free variable (happens to most variables)
% reg_func(F): register flat function F, so form is either 'a' or 'f(A,B,C)'
% An exist. quantified var is always first registered as reg_var(X,exist),
% so a later registration of the same as reg_var(X,free) is ignored!

flat_vv @ X eq Y    <=> var(X), var(Y)          | 
                        reg_var(X,free), reg_var(Y,free), X eq_f Y.
flat_tv @ T eq X    <=> nonvar(T), var(X)       | 
                        X eq T.  
flat_tt @ T1 eq T2  <=> nonvar(T1), nonvar(T2)  | 
                        reg_var(X,exist), X eq T1, X eq T2.
flat_vt @ X eq T    <=> var(X), nonvar(T)       | 
                        reg_var(X,free), T=..[F|L], flatten_list(L,L0), 
                        T0=..[F|L0], reg_func(T0), X eq_f T0.


% flatten_list(L1,L2): flatten terms and introduce new vars and equations
% e.g., L1=[f(a),b,X] ==> L2=[_A,_B,X], _A eq f(a), _B eq b
flatten_list([],   []).
flatten_list([H|T],[H|T0]) :- 
    var(H), reg_var(H,free), flatten_list(T,T0).
flatten_list([H|T],[X|T0]) :- 
    nonvar(H), reg_var(X,exist), X eq H, flatten_list(T,T0).


%%% RATIONAL TREE
%% solve rational tree (flat equations)

reflexivity     @ X eq_f X                      <=> var(X) | true.
orientation     @ id(X,IX), id(T,IT) \ T eq_f X <=> var(X), lss(IX,IT) |
                                                    X eq_f T.
decomposition   @ T1 eq_f T2                    <=> nonvar(T1), nonvar(T2) |
                                                    same_functions(T1,T2).
confrontation   @ id(X,IX), id(T1,IT1), id(T2,IT2), X eq_f T1 \ X eq_f T2 <=>
                            var(X), lss(IX,IT1), leq(IT1,IT2) | T1 eq_f T2.


% functions must be equal
same_functions(T1,T2) :- 
    T1=..[F|L1], T2=..[F|L2], eq_list(L1,L2).

% two lists must have same length and elements must be equal
eq_list([],[]).
eq_list([H1|T1],[H2|T2]) :- H1 eq_f H2, eq_list(T1,T2).


%%% ELIMINATION OF UNREACHABLE VARIABLES AND EQUATIONS
%% reachable exist. quantified variables and equations are marked with _s 
%% (meaning solution: exist_s, eq_s); all others are removed
%% triggered by elim/0.

elim \ id(X,(free,_))      <=>             reach(X).
elim, reach(X) \ X eq_f T  <=> nonvar(T) | X eq_s T, T=..[_|L], reach_list(L).
elim, reach(X) \ X eq_f Y  <=> var(Y)    | X eq_s Y, reach(Y).
elim, reach(X) \ id(X,(exist,_)) <=>        exists_s(X).

%% cleanup
elim \ id(_,_)   <=> true.
elim \ _ eq_f _  <=> true.
elim \ reach(_)  <=> true.
elim \ nextid(_) <=> true.
elim <=> true.


% all vars in list are reachable
reach_list([]).
reach_list([H|T]) :- reach(H), reach_list(T).


%%% AUXILIARY
%% registering variables and functions, and testing for order

% For each variable and function from the flat equations one ID is created:
% id(X,ID): X is the term, ID=(Type,Nr), Type is one of the following 3 types:
% id(X,(exist,N)): X is an existentially quantified variable with number N
% id(X,(free,N)): X is a free variable with number N
% id(X,(func,N)): X is a function with termdepth N
% The variable numbers are unique and assigned sequentially.

% exists(Varlist): must be entered before equations, registers vars
exists([])                  <=> true.
exists([V|L])               <=> reg_var(V,exist), exists(L).

% register flat function: form is either 'a' or 'f(A,B)', so depth is 1 or 2
reg_func(T)                 <=> atom(T) | id(T,(func,1)).
reg_func(T)                 <=> compound(T) | id(T,(func,2)).

%% register var: exist. quantified (id(V,exist)) or free var (id(V,free))
% only first registration for each variable is effective, later is ignored;
% so e.g., exists(X), reg_var(X,free). ==> reg_var(X,exist).
id(V,_) \ reg_var(V,_)      <=> true.
% nextid(N) constraint determines number for variable
reg_var(V,T), nextid(N)     <=> id(V,(T,N)), N1 is N+1, nextid(N1).
% nextid/1 doesn't yet exist, take 0 as first var number
reg_var(V,T)                <=> id(V,(T,0)), nextid(1).


%% term order
%% to compare variables and functions the corresponding IDs 
%% must be passed to lss/2 or leq/2

% lss(X,Y): X is smaller than Y, iff
% type of X is 'smaller' than type of Y (exist < free < func) or
% X and Y have same type and number for X is smaller than number for Y
% (for variables the number is sequentially assigned to all used variables, 
% for functions the number is the term-depth)
lss((exist,_),(free,_)).
lss((exist,_),(func,_)).
lss((free,_),(func,_)).
lss((T,N), (T,M)) :- N < M.

% leq(X,Y): X is smaller or equal to Y, iff
% X is smaller than Y (lss(X,Y)) or
% IDs of X and Y are identical
leq(IX,IY) :- lss(IX,IY).
leq((T,N),(T,N)).

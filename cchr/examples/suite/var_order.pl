/*
var_order.pl: Stable variable order
(C) Christian Holzbaur, 1995
(C) Martin.Kaeser at uni-ulm.de, 2006/12/05
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Define a stable order on variables (Term/Var order changes under
put_atts, delay, etc.) as long as variables are not bound to other variables.

%% HOW TO USE
Similar to compare/3, variables can be compared with
    var_compare(Rel,X,Y):   compare vars X,Y using relation R (<,=,>)
Before comparing them, variables *need* to be globalized with
    globalize(T):           globalize all variables in term T

%% SAMPLE QUERIES
Q: globalize(a(X,b,Y)), var_compare(<,X,Y).
A: yes.
*/

%% stable variable order based on chr/ordering by C. Holzbaur
:- module(var_order, [globalize/1, var_compare/3]).
:- use_module(library(terms)).
:- use_module(library(atts)).
:- attribute id/1.

%% exception when attributed variable is bound
verify_attributes(_,_, []).

%% globalize all variables in term
globalize(Term) :-
    term_variables(Term, Vars), var_globalize(Vars).

var_globalize([]).
var_globalize([X|Xs]) :-
    ( get_atts(X,id(_)) -> true ; put_atts(X,id(_)) ),
    var_globalize(Xs).

var_id(X,IdX) :- var(X), 
    ( get_atts(X,id(IdX)) -> true ; raise_exception(not_globalized) ).

%% compare vars X,Y using relation R (<,=,>)
%% fail if no var, exception if var not globalized
var_compare(Rel,X,Y) :-
    var_id(X,IdX), var_id(Y,IdY), compare(Rel,IdX,IdY).

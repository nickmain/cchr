/*
db_family.pl: family relations (deductive database)
(C) Martin.Kaeser at uni-ulm.de, 2006/12/18
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
This is an example how CHR could be used as a deductive database, i.e., the
information is deduced from facts and rules.

%% HOW TO USE
The facts are explicitly created by the 'facts' constraint.
The database rules are formulated as CHR rules. This leads to the deduction of
additional data constraints.

%% SAMPLE QUERIES
Q: facts.
A: ancestor(lindsay,maeby),
    ancestor(lucille,buster),
    ancestor(lucille,george_michael),
    ancestor(lucille,lindsay),
    ancestor(lucille,maeby),
    ancestor(lucille,michael),
    ancestor(michael,george_michael),
    aunt(lindsay,george_michael),
    cousin(george_michael,maeby),
    cousin(maeby,george_michael),
    father(michael,george_michael),
    gender(buster,m),
    gender(george_michael,m),
    gender(lindsay,f),
    gender(lucille,f),
    gender(maeby,f),
    gender(michael,m),
    grandparent(lucille,george_michael),
    grandparent(lucille,maeby),
    mother(lindsay,maeby),
    mother(lucille,buster),
    mother(lucille,lindsay),
    mother(lucille,michael),
    parent(lindsay,maeby),
    parent(lucille,buster),
    parent(lucille,lindsay),
    parent(lucille,michael),
    parent(michael,george_michael),
    sibling(buster,lindsay),
    sibling(buster,michael),
    sibling(lindsay,buster),
    sibling(lindsay,michael),
    sibling(michael,buster),
    sibling(michael,lindsay),
    uncle(buster,george_michael),
    uncle(buster,maeby),
    uncle(michael,maeby).
*/

:- module(db_family, 
    [gender/2, parent/2, father/2, mother/2, grandparent/2, ancestor/2, 
    sibling/2, cousin/2, aunt/2, uncle/2, facts/0]).
:- use_module(library(chr)).

%% Deprecated syntax used for SICStus 3.x
%handler db_family.
%constraints

%% Syntax for SWI / SICStus 4.x
:- chr_constraint 

    %% FACTS
    gender/2, parent/2, 
    %% DEDUCED
    father/2, mother/2, grandparent/2, ancestor/2, 
    sibling/2, cousin/2, aunt/2, uncle/2, 
    %% OTHER
    facts/0.


%% FACTS

facts <=>
    gender(lucille,f), gender(michael,m), gender(lindsay,f),
    gender(george_michael,m), gender(maeby,f), gender(buster,m),
    parent(lucille, michael),
    parent(lucille, lindsay),
    parent(lucille, buster),
    parent(michael, george_michael), 
    parent(lindsay, maeby).


%% RULES

% remove duplicates
sibling(X,Y) \ sibling(X,Y) <=> true.
cousin(X,Y) \ cousin(X,Y) <=> true.

% only one mother/father per person
mother(M1,C) \ mother(M2,C) <=> M1=M2.
father(F1,C) \ father(F2,C) <=> F1=F2.

% female parent is mother, ...
parent(X,Y), gender(X,f) ==> mother(X,Y).
parent(X,Y), gender(X,m) ==> father(X,Y).

parent(G,P), parent(P,C) ==> grandparent(G,C).

% ancestor is transitive closure of parent
parent(X,Y) ==> ancestor(X,Y).
parent(X,Y), ancestor(Y,Z) ==> ancestor(X,Z). 

% siblings are people who have same parent
parent(P,X), parent(P,Y) ==> X\==Y | sibling(X,Y).

% cousins are people who have same grandparent
grandparent(G,X), grandparent(G,Y) ==> X\==Y | cousin(X,Y).

% aunt(A,C): A is female sibling of parent of C
gender(A,f), sibling(A,P), parent(P,C) ==> aunt(A,C).
% uncle(U,C): U is male sibling of parent of C
gender(U,m), sibling(U,P), parent(P,C) ==> uncle(U,C).

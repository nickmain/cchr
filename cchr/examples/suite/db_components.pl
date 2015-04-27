/*
db_family.pl: prices for computer components (deductive database)
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
A: vat(19),
    composed(pc,board),
    composed(pc,cpu),
    composed(pc,hdd),
    composed(pc,ram),
    composed(pc_complete,pc),
    composed(pc_complete,tft),
    gross_price(board,47.599999999999994),
    gross_price(cpu,95.19999999999999),
    gross_price(hdd,107.1),
    gross_price(pc,368.9),
    gross_price(pc_complete,725.9),
    gross_price(ram,119.0),
    gross_price(tft,357.0),
    price(board,40),
    price(cpu,80),
    price(hdd,90),
    price(pc,310),
    price(pc_complete,610),
    price(ram,100),
    price(tft,300).
*/

:- module(db_components, 
    [price/2, composed/2, vat/1, gross_price/2, facts/0]).
:- use_module(library(chr)).

%% Deprecated syntax used for SICStus 3.x
%handler db_components.
%constraints

%% Syntax for SWI / SICStus 4.x
:- chr_constraint 
    %% FACTS
    composed/2, 
    vat/1, 
    price/2,        % mixed, partly fact, partly deduced
    %% DEDUCED
    gross_price/2, 
    facts/0.


%% FACTS 

facts <=>
    price(cpu,80),
    price(board,40),
    price(ram,100),
    price(hdd,90),
    composed(pc,cpu), composed(pc,board), composed(pc,ram), composed(pc,hdd),
    price(tft,300),
    composed(pc_complete,pc), composed(pc_complete,tft),
    vat(19).


%% RULES

% calc price of composed objects
price(X,P1), price(X,P2) <=> P is P1+P2, price(X,P).
composed(X,Y), price(Y,P) ==> price(X,P).

% calc gross price
price(X,NP), vat(V) ==> GP is NP*(V/100+1), gross_price(X,GP).

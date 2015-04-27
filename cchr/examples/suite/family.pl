/*
family.pl: Inferring family relationships
(C) Slim.Abdennadher at guc.edu.eg, 2000/04/01
(C) Henning Christiansen, 2000/04/01
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Family relationships father, mother, parent, sibling and orphan are inferred.

%% HOW TO USE
The following constraints are defined:
    person(P,S):    sex of person P is S (S=male|female)
    father(F,C):    F is father of C
    mother(M,C):    M is mother of C
    parent(P,C):    P is parent of C
    orphan(P):      P is an orphan (no mother or father)
    sibling(A,B):   A and B are siblings
    diff(A,B):      A and B are different persons

%% SAMPLE QUERIES
Q: father(X,Y), mother(X,Y).
A: no.

Q: sibling(mary,jack).
A: diff(mary,jack), father(_A,mary), person(_A,male),
    person(mary,_B), father(_A,jack), person(jack,_C) ;
A: diff(mary,jack), mother(_A,mary), person(_A,female),
    person(mary,_B), mother(_A,jack), person(jack,_C).

Q: sibling(mary,jack), person(mary,female), person(jack,male).
A: diff(mary,jack), father(_A,mary), father(_A,jack), 
  person(_A,male), person(jack,male), person(mary,female) ;
A: diff(mary,jack), mother(_A,mary), mother(_A,jack),
  person(_A,female), person(jack,male), person(mary,female).

Q: sibling(mary,jack), father(joe,mary), mother(anne,jack),
  person(mary,female), person(jack,male).
A: diff(mary,jack), father(joe,jack), father(joe,mary), mother(anne,jack),
  person(anne,female), person(jack,male), person(joe,male), person(mary,female);
A: diff(mary,jack), father(joe,mary), mother(anne,jack), mother(anne,mary),
  person(anne,female), person(jack,male), person(joe,male), person(mary,female).

Q: sibling(X,Y), orphan(X).
A: no.
*/

:- module(family, 
[person/2, father/2, mother/2, orphan/1, parent/2, sibling/2, diff/2]).
:- use_module(library(chr)).

%% Deprecated syntax used for SICStus 3.x
%handler family.
%constraints

%% Syntax for SWI / SICStus 4.x
:- chr_constraint

  % extensional predicates:
     person/2, father/2, mother/2, orphan/1, 
  % intensional predicates:
     parent/2, sibling/2,
  % predefined:
     diff/2.


diff(X,X) ==> fail.


% Definition rules:

parent_def @
parent(P,C) <=> true | (father(P,C) ; mother(P,C)).

sibling_def @
sibling(C1,C2) <=> diff(C1,C2), parent(P,C1), parent(P,C2).


% ICs

ic_father_unique @
father(F1,C), father(F2,C) ==> F1=F2.

ic_mother_unique @
mother(M1,C), mother(M2,C) ==> M1=M2.

ic_gender_unique @
person(P,G1), person(P,G2) ==> G1=G2.

ic_father_persons @
father(F,C) ==> person(F,male), person(C,_).

ic_mother_persons @
mother(M,C) ==> person(M,female), person(C,_).


% Indirect def.

orphan1 @
orphan(C) ==>  person(C,_).

orphan2 @
orphan(C), father(_,C) ==> fail.

orphan3 @
orphan(C), mother(_,C) ==> fail.


% Remove duplicates

father(F,C) \ father(F,C) <=> true.
mother(M,C) \ mother(M,C) <=> true.
person(M,C) \ person(M,C) <=> true.
orphan(C) \ orphan(C) <=> true.

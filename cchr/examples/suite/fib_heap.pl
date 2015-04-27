/*
fib_heap.pl: Fibonacci heap priority queue
(C) Jon.Sneyers at cs.kuleuven.be, 2006
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Optimal Fibonacci heap implementation based on [Fredman & Tarjan, 
J. ACM 34(3), 1987].
Maintain a priority queue which contains item-priority pairs.
Items are integers > 0, priorities are any numeric type.
An item-priority pair can be added, its priority can be decreased,
and the item with the lowest priority can be extracted (find+remove).


%% HOW TO USE
The following operations are supported:
  insert(I,P):          add item I with priority P
  extract_min(I,P):     returns minimal priority pair I-P and removes it
  decr(I,NP):           decreases the priority of I to the new value NP (only 
                        if NP is smaller than the old priority)
  decr_or_ins(I,P):     decreases the priority of I, or inserts it if it is 
                        not in the queue                        
Data is represented internally as:
  item(I,P,Rank,Parent,Mark):   a node in the Fibonacci heap. Parent=0 for 
                        roots. See literature for Rank and Mark.
  min(I,P):             current minimal element
Internal operations are:
  findmin:              [extract_min] find the new minimal element
  ch2rt(I):             [extract_min] convert the children of I to roots
  decr(I,NP,R,P,M):     [decr] re-inserts an item in a decrease operation, 
                        respecting heap order
  mark(I):              [decr] mark I, and if necessary, cut it off and mark 
                        its parent

%% SEE ALSO
Fibonacci heaps are used in dijkstra.pl
Reference paper:
  Dijkstra's Algorithm with Fibonacci Heaps: An Executable Description in CHR.
  Jon Sneyers, Tom Schrijvers and Bart Demoen. WLP 2006.


%% SAMPLE QUERY
Q: insert(1,8), insert(2,4), insert(3,6), decr(1,5), extract_min(A1,A2), 
    extract_min(B1,B2), extract_min(C1,C2).
A: A1 = 2, A2 = 4,  B1 = 1, B2 = 5,  C1 = 3, C2 = 6.
*/

:- module(fib_heap, [insert/2, extract_min/2, decr/2, decr_or_ins/2]).
:- use_module(library(chr)).

%% Deprecated syntax used for SICStus 3.x
%handler fib.
%constraints
%    insert/2,
%    extract_min/2,
%    decr/2,
%    decr_or_ins/2,
%    % internal:
%    item/5,
%    min/2,
%    decr/5,
%    mark/1,
%    ch2rt/1,
%    findmin/0.

%% Syntax for SWI / SICStus 4.x
:- chr_constraint  
    insert(+int,+number),
    extract_min(?int,?number),
    decr(+int,+number),
    decr_or_ins(+int,+number),
    % internal:
    item(+node,+number,+int,+node,?mark),
    min(+int,+number),
    decr(+int,+number,+int,+int,+mark),             
    mark(+int),
    ch2rt(+int),
    findmin.

:- chr_type mark ---> m ; u.
%:- chr_type node == dense_int.          % efficient: arrays
:- chr_type node == int.                % less efficient: hashtables


insert @ insert(I,K) <=> item(I,K,0,0,u), min(I,K).

keep_min @ min(_,A) \ min(_,B) <=> A =< B | true.

extr       @ extract_min(X,Y), min(I,K), item(I,_,_,_,_) 
           <=> ch2rt(I), findmin, X=I, Y=K.
extr_empty @ extract_min(_,_) <=> fail.

c2r      @ ch2rt(I) \ item(C,K,R,I,_)#X <=> item(C,K,R,0,u) pragma passive(X).
c2r_done @ ch2rt(_) <=> true.

findmin  @ findmin, item(I,K,_,0,_) ==> min(I,K).
foundmin @ findmin <=> true.

same_rank @ item(I1,K1,R,0,_), item(I2,K2,R,0,_) 
          <=> R1 is R+1, (K1 < K2 -> item(I2,K2,R,I1,u), item(I1,K1,R1,0,u)
                              ;  item(I1,K1,R,I2,u), item(I2,K2,R1,0,u)).

decr     @ decr(I,K), item(I,O,R,P,M) <=> K < O | decr(I,K,R,P,M).
decr_nok @ decr(_,_) <=> fail.

doi-decr   @ item(I,O,R,P,M), decr_or_ins(I,K) <=> K < O | decr(I,K,R,P,M).
doi-nop    @ item(I,O,_,_,_) \ decr_or_ins(I,K) <=> K >= O | true.
doi-insert @ decr_or_ins(I,K) <=> insert(I,K).

d_min    @ decr(I,K,_,_,_) ==> min(I,K).
d_root   @ decr(I,K,R,0,_) <=> item(I,K,R,0,u).
d_noprob @ item(P,PK,_,_,_) \ decr(I,K,R,P,M) <=> K >= PK | item(I,K,R,P,M).
d_prob   @ decr(I,K,R,P,_) <=> item(I,K,R,0,u), mark(P).

m_rt @ mark(I), item(I,K,R,P,_) <=> P=0 | R1 is R-1, item(I,K,R1,0,u).
m_m  @ mark(I), item(I,K,R,P,M) <=> M=m | R1 is R-1, item(I,K,R1,0,u),mark(P).
m_u  @ mark(I), item(I,K,R,P,M) <=> M=u | R1 is R-1, item(I,K,R1,P,m).
m_er @ mark(_) <=> writeln(error_mark), fail.

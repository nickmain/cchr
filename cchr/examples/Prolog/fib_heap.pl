:- module(fib_heap,[insert/2,extract_min/2,decr/2,decr_or_ins/2]).
:- use_module(library(chr)).
:- chr_option(debug,off).
:- chr_option(optimize,full).
:- chr_option(check_guard_bindings,off).


:- constraints  
    insert(+int,+number),   extract_min(?int,?number),     mark(+int),
    decr(+int,+number),     decr_or_ins(+int,+number),    ch2rt(+int),
    decr(+int,+number,+int,+int,+mark),             min(+int,+number),
    item(+dense_int,+number,+int,+dense_int,+mark),           findmin.
:- chr_type mark ---> m ; u.


insert @ insert(I,K) <=> item(I,K,0,0,u), min(I,K).

keep_min @ min(_,A) \ min(_,B) <=> A =< B | true.

extr       @ extract_min(X,Y), min(I,K), item(I,_,_,_,_) 
           <=> ch2rt(I), findmin, X=I, Y=K.
extr_empty @ extract_min(_,_) <=> fail.

c2r      @ ch2rt(I) \ item(C,K,R,I,_)#X <=> item(C,K,R,0,u) pragma passive(X).
c2r_done @ ch2rt(I) <=> true.

findmin  @ findmin, item(I,K,_,0,_) ==> min(I,K).
foundmin @ findmin <=> true.

same_rank @ item(I1,K1,R,0,_), item(I2,K2,R,0,_) 
          <=> R1 is R+1, (K1 < K2 -> item(I2,K2,R,I1,u), item(I1,K1,R1,0,u)
	                          ;  item(I1,K1,R,I2,u), item(I2,K2,R1,0,u)).

decr     @ decr(I,K), item(I,O,R,P,M) <=> K < O | decr(I,K,R,P,M).
decr_nok @ decr(I,K) <=> fail.

doi-decr   @ item(I,O,R,P,M), decr_or_ins(I,K) <=> K < O | decr(I,K,R,P,M).
doi-nop    @ item(I,O,_,_,_) \ decr_or_ins(I,K) <=> K >= O | true.
doi-insert @ decr_or_ins(I,K) <=> insert(I,K).

d_min    @ decr(I,K,_,_,_) ==> min(I,K).
d_root   @ decr(I,K,R,0,_) <=> item(I,K,R,0,u).
d_noprob @ item(P,PK,_,_,_) \ decr(I,K,R,P,M) <=> K >= PK | item(I,K,R,P,M).
d_prob   @ decr(I,K,R,P,M) <=> item(I,K,R,0,u), mark(P).

m_rt @ mark(I), item(I,K,R,P,_) <=> P=0 | R1 is R-1, item(I,K,R1,0,u).
m_m  @ mark(I), item(I,K,R,P,M) <=> M=m | R1 is R-1, item(I,K,R1,0,u), mark(P).
m_u  @ mark(I), item(I,K,R,P,M) <=> M=u | R1 is R-1, item(I,K,R1,P,m).
m_er @ mark(I) <=> writeln(error_mark), fail.

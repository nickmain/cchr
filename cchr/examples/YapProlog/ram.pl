/*
ram_simulator.pl: Simulates a Random Access Machine
(C) Jon.Sneyers at cs.kuleuven.be
(C) Tom.Schrijvers at cs.kuleuven.be
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Simulates a Random Access Machine (RAM) that is an abstract machine consisting
of memory cells (mem), program instructions (prog) and a program instruction 
counter (prog_counter).
The instruction pointed to by the prog_counter is executed, thereby accessing 
the memory and updating the prog_counter.

%% HOW TO USE
The memory cell with label L and value V is represented as mem(L,V).
The program counter pointing to P is represented as prog_counter(P).
A program instruction has the form prog(L,L1, Instr, Dest): 
prog_counter(P) selects the instruction for which L=P.
After executing the instruction prog_counter is set to L1 by default.
Instr and Dest specify the instruction and destination.

The following instructions are supported:
* prog(L,L1,add(B),A): A:=A+B (add value of register B to register A)
* prog(L,L1,sub(B),A): A:=A-B (subtract value of register B from register A)
* prog(L,L1,mult(B),A): A:=A*B (multiply register A with value of register B)
* prog(L,L1,div(B),A): A:=A/B (divide register A by value of register B)
* prog(L,L1,move(B),A): A:=B (put the value in register B in register A)
* prog(L,L1,i_move(B),A): A:=[B] (put the value in register 
    <value in register B> in register A)
* prog(L,L1,move_i(B),A): [A]:=B (put the value in register B in register 
    <value in register A>)
* prog(L,L1,const(B),A): A:=#B (put the value B in register A)
* prog(L,_,jump,A): PC:=#A (unconditional jump to label A)
* prog(L,L1,cjump(R),A): if R=0 then PC:=#A else PC:=#L1 (jump to label A 
    if register R is zero, otherwise continue)
* prog(L,_,halt,_): PC:=undef (halt)


%% SEE ALSO
(1) John E. Savage. Models of Computation: Exploring the Power of Computing.
    Addisson-Wesley, ISBN 0-201-89539-0, 1998.
(2) http://en.wikipedia.org/wiki/Random_Access_Machine

%% SAMPLE QUERIES
Q: mem(1,1), mem(2,10000), mem(3,0), prog_counter(1),
    prog(1,2, add(1), 3),
    prog(2,3, sub(1), 2),
    prog(3,1, cjump(2), 4),
    prog(4,0, halt, 0).
A: mem(1,1), mem(2,0), mem(3,10000), ...

% emulate division
Q: mem(a,17), mem(b,3), mem(r,0), mem(c1,1), mem(t,0), prog_counter(0),
    prog(0,1, add(c1), a),
    prog(1,2, move(b), t),
    prog(2,3, sub(c1), t),
    prog(3,4, sub(c1), a),
    prog(4,5, cjump(a), end),
    prog(5,2, cjump(t), 6),
    prog(6,1, add(c1), r),
    prog(end,0, halt, 0).
A: mem(a,0), mem(b,3), mem(r,5),
    mem(c1,1), mem(t,0), ...
*/

:- module(ram_simulator, [mem/2, prog/4, prog_counter/1, test/1]).
:- use_module(library(chr)).

option(debug,off).                                                                                            
option(optimize,full).                                                                                        
option(check_guard_bindings,off).                                                                             

%% Deprecated syntax used for SICStus 3.x
%handler ram_simulator.
constraints mem/2, prog/4, prog_counter/1.

%% Syntax for SWI / SICStus 4.x
%:- chr_constraint 
%    mem(+int,?int), 
%    prog(+int,+int,+any,?int), 
%    prog_counter(?int).


% add value of register B to register A
prog(L,L1,add(B),A), mem(B,Y) \ mem(A,X), prog_counter(L) <=> 
    Z is X+Y, mem(A,Z), prog_counter(L1).

% subtract value of register B from register A
prog(L,L1,sub(B),A), mem(B,Y) \ mem(A,X), prog_counter(L) <=> 
    Z is X-Y, mem(A,Z), prog_counter(L1).

% multiply register A with value of register B
prog(L,L1,mult(B),A), mem(B,Y) \ mem(A,X), prog_counter(L) <=> 
    Z is X*Y, mem(A,Z), prog_counter(L1).

% divide register A by value of register B
prog(L,L1,div(B),A), mem(B,Y) \ mem(A,X), prog_counter(L) <=> 
    Z is X//Y, mem(A,Z), prog_counter(L1).


% put the value in register B in register A
prog(L,L1,move(B),A), mem(B,X) \ mem(A,_), prog_counter(L) <=> 
    mem(A,X), prog_counter(L1).

% put the value in register <value in register B> in register A
prog(L,L1,i_move(B),A), mem(B,C), mem(C,X) \ mem(A,_), prog_counter(L) <=> 
    mem(A,X), prog_counter(L1).

% put the value in register B in register <value in register A>
prog(L,L1,move_i(B),A), mem(B,X), mem(A,C) \ mem(C,_), prog_counter(L) <=> 
    mem(C,X), prog_counter(L1).

% put the value B in register A        -> redundant if consts are in init mem
prog(L,L1,const(B),A) \ mem(A,_), prog_counter(L) <=> 
    mem(A,B), prog_counter(L1).

% unconditional jump to label A
prog(L,_,jump,A) \ prog_counter(L) <=> 
    prog_counter(A).

% jump to label A if register R is zero, otherwise continue
prog(L,_,cjump(R),A), mem(R,X) \ prog_counter(L) <=> X == 0 |
    prog_counter(A).
prog(L,L1,cjump(R),_), mem(R,X) \ prog_counter(L) <=> X =\= 0 |
    prog_counter(L1).

% halt
prog(L,_,halt,_) \ prog_counter(L) <=> true.

test(N) :- mem(1,1), mem(2,N), mem(3,0), prog(1,2, add(1), 3), prog(2,3, sub(1), 2), prog(3,1, cjump(2), 4), prog(4,0, halt, 0), prog_counter(1).

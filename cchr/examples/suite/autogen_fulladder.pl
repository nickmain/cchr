/*
autogen_fulladder.pl: Auto-generated full-adder handler
(C) Slim.Abdennadher at guc.edu.eg, 2005
This program is distributed under the terms of the GNU General Public License:
http://www.gnu.org/licenses/gpl.html

%% DESCRIPTION
Automatically generated handler for a full-adder.

%% HOW TO USE
A full-adder sums three one-bit numbers A, B and Cin, and outputs a two-bit 
number with Cout as most-significant and S as least-significant bit, i.e., 
A+B+C = 2*Cout+S.
A full-adder with input bits A, B and carry Cin, and output bits sum S and 
carry Cout is represented as
  fulladder(A,B,Cin,S,Cout).

%% SEE ALSO
S Abdennadher, C Rigotti. Automatic generation of CHR constraint solvers. 
Theory and Practice of Logic Programming, 2005.

%% SAMPLE QUERIES
Q: fulladder(1,1,0,S,Cout).
A: S=0, Cout=1, fulladder(1,1,0,0,1).

Q: fulladder(1,1,Cin,S,0).
A: no.

Q: fulladder(A,B,Cin,1,1).
A: A=1, B=1, Cin=1, fulladder(1,1,1,1,1).

Q: fulladder(1,1,Cin,S,Cout).
A: Cin=S, Cout=1, fulladder(1,1,S,S,1).

Q: fulladder(1,0,Cin,S,Cout).
A: Cout=Cin, fulladder(1,0,Cin,S,Cin).

Q: fulladder(A,A,Cin,S,Cout).
A: Cin=S, Cout=A, fulladder(A,A,S,S,A).

Q: fulladder(A,B,0,C,C).
A: A=0, B=0, C=0, fulladder(0,0,0,0,0).

Q: fulladder(A0,B0,0,S0,C1), fulladder(A1,B1,C1,S1,C2), 
    fulladder(A2,B2,C2,S2,C3), fulladder(A3,B3,C3,S3,C), 
    A3=1,A2=0,A1=0,A0=1, B3=0,B2=1,B1=1,B0=1.
A: C = 1, S3 = 0, S2 = 0, S1 = 0, S0 = 0,   C1 = 1, C2 = 1, C3 = 1, 
    A0 = 1, A1 = 0, A2 = 0, A3 = 1, B0 = 1, B1 = 1, B2 = 1, B3 = 0,
    fulladder(0,1,1,0,1), fulladder(1,0,1,0,1), fulladder(1,1,0,0,1).
*/

:- module(autogen_fulladder, [fulladder/5]).
:- use_module( library(chr)).

%% Deprecated syntax used for SICStus 3.x
%handler autogen_fulladder.
%constraints fulladder/5.

%% Syntax for SWI / SICStus 4.x
:- chr_type bool ---> 0 ; 1.
:- chr_constraint fulladder(?bool,?bool,?bool,?bool,?bool).


fulladder(A,B,C,D,D)==>A=D,B=D,C=D.
fulladder(A,B,C,C,D)==>A=D,B=D.
fulladder(A,B,C,B,D)==>A=D,C=D.
fulladder(A,B,B,C,B)==>A=C.
fulladder(A,B,B,C,D)==>B=D.
fulladder(A,B,C,A,D)==>B=C.
fulladder(A,B,A,C,A)==>B=C.
fulladder(A,A,B,C,A)==>B=C.
fulladder(A,B,A,C,D)==>A=D.
fulladder(A,A,B,C,D)==>A=D.
fulladder(A,B,1,0,C)==>1=C.
fulladder(A,B,1,C,0)==>C=1.
fulladder(A,B,0,1,C)==>0=C.
fulladder(A,B,0,C,1)==>C=0.
fulladder(A,1,B,0,C)==>1=C.
fulladder(A,1,B,C,0)==>C=1.
fulladder(A,1,0,B,C)==>A=C.
fulladder(A,0,B,1,C)==>0=C.
fulladder(A,0,B,C,1)==>C=0.
fulladder(A,0,1,B,C)==>A=C.
fulladder(1,A,B,0,C)==>1=C.
fulladder(1,A,B,C,0)==>C=1.
fulladder(1,A,0,B,C)==>A=C.
fulladder(1,0,A,B,C)==>A=C.
fulladder(0,A,B,1,C)==>0=C.
fulladder(0,A,B,C,1)==>C=0.
fulladder(0,A,1,B,C)==>A=C.
fulladder(0,1,A,B,C)==>A=C.

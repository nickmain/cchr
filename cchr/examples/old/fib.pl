:- use_module(library(chr)).
:- chr_option(debug,off).
:- chr_option(optimize,full).
:- chr_option(check_guard_bindings,off).

:- chr_constraint init(+int).
:- chr_constraint fib(+int,+int).

begin @ init(_) ==> fib(0,1), fib(1,1).
calc @ init(Max),fib(N1,M1),fib(N2,M2) ==> N2 is N1+1, N2<Max | N3 is N2+1, M3 is M1+M2, fib(N3,M3).
fini @ fib(Max,_) \ init(Max) <=> true.

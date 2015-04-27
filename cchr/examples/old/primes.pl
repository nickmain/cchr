:- use_module(library(chr)).
:- chr_option(debug,off).
:- chr_option(optimize,full).
:- chr_option(check_guard_bindings,off).

:- chr_constraint candidate(+int).
:- chr_constraint prime(+int).

candidate(X) <=> X is 1 | true.
candidate(N) <=> prime(N), M is N-1, candidate(M).
prime(Y) \ prime(X) <=> 0 is X mod Y | true.

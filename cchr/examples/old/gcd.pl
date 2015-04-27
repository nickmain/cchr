%% 980202, 980311 Thom Fruehwirth, LMU
%% computes greatest common divisor of positive numbers written each as gcd(N)

:- use_module(library(chr)).
:- chr_option(debug,off).
:- chr_option(optimize,full).
:- chr_option(check_guard_bindings,off).

handler gcd.

:- chr_constraint gcd(+int).

gcd(0) <=> true.
gcd(N) \ gcd(M) <=> N=<M | L is M-N, gcd(L).

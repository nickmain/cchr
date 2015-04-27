all: compil.pdf overzicht.pdf bench-*.eps

compil.pdf: compil.eps
	epstopdf compil.eps

overzicht.pdf: overzicht.eps
	epstopdf overzicht.eps

bench-fib.eps: bench-fib.pdf
	pdftops -eps -level3 bench-fib.pdf bench-fib.eps

bench-gcd.eps: bench-gcd.pdf
	pdftops -eps -level3 bench-gcd.pdf bench-gcd.eps

bench-tak.eps: bench-tak.pdf
	pdftops -eps -level3 bench-tak.pdf bench-tak.eps

bench-primes.eps: bench-primes.pdf
	pdftops -eps -level3 bench-primes.pdf bench-primes.eps

bench-leq.eps: bench-leq.pdf
	pdftops -eps -level3 bench-leq.pdf bench-leq.eps

bench-ram.eps: bench-ram.pdf
	pdftops -eps -level3 bench-ram.pdf bench-ram.eps

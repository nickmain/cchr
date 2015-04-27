#!/bin/bash

cd YapProlog

echo $(echo -en "[$1].\nstatistics(cputime,[C1,_]),statistics(walltime,[R1,_]),test($2),statistics(cputime,[C2,_]),statistics(walltime,[R2,_]),C is C2-C1,R is R2-R1.\n" | yap 2>&1 | egrep '[CR] = ' | cut -d '=' -f 2 | cut -d ',' -f 1)



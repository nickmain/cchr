#!/bin/bash

cd Prolog

echo $(echo -en "[$1].\nstatistics(runtime,[C1,_]),statistics(system_time,[S1,_]),test($2),statistics(runtime,[C2,_]),statistics(system_time,[S2,_]),C is C2-C1,R is C2+S2-C1-S1.\n" | pl -O -nodebug -L0 -G0 -T0 -q 2>&1 | fgrep ' = ' | cut -d '=' -f 2 | cut -d , -f 1 | tail -n 2)



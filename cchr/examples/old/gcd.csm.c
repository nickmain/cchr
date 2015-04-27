#include <stdint.h>
#include <stdio.h>


#define CONSLIST(DEF,SEP) DEF(gcd_1)
#define ARGLIST_gcd_1(DEF,SEP) DEF(gcd_1,arg1,uint64_t)
#define RULELIST_gcd_1(DEF,SEP) DEF(gcd_1_rule1) SEP DEF(gcd_1_rule2) SEP DEF(gcd_1_rule3)

#define CODELIST_gcd_1_rule1 \
  CSM_IF(CSM_ARG(gcd_1,arg1)==0, \
    CSM_KILLSELF \
    CSM_END \
  ) \

#define CODELIST_gcd_1_rule2 \
  CSM_LOOP(gcd_1,I, \
    CSM_IF(CSM_DIFFSELF(I), \
      CSM_IF(CSM_ARG(gcd_1,arg1)>=CSM_LARG(gcd_1,I,arg1), \
        CSM_KILLSELF \
        CSM_ADD(gcd_1,CSM_ARG(gcd_1,arg1)-CSM_LARG(gcd_1,I,arg1)) \
        CSM_END \
      ) \
    ) \
  )

#define CODELIST_gcd_1_rule3 \
  CSM_MAKE(gcd_1) \
  CSM_LOOP(gcd_1,J, \
    CSM_IF(CSM_DIFFSELF(J), \
      CSM_IF(CSM_LARG(gcd_1,J,arg1) >= CSM_ARG(gcd_1,arg1), \
        CSM_SETLOCAL(uint64_t,J_arg1,CSM_LARG(gcd_1,J,arg1)) \
        CSM_KILL(J) \
	    CSM_NEEDSELF \
        CSM_ADD(gcd_1,CSM_GETLOCAL(J_arg1)-CSM_ARG(gcd_1,arg1)) \
	    CSM_IF(!CSM_ALIVESELF || CSM_REGENSELF,CSM_END) \
      ) \
    ) \
  )

#include "cchr_csm.h"

int main(void) {
  cchr_runtime_init();
  cchr_fire_gcd_1(DCLS_EMPTY_PID,10ULL);
  cchr_fire_gcd_1(DCLS_EMPTY_PID,17856535355ULL);
  printf("size=%u\n",(unsigned int)(sizeof(cchr_entry_t)));
  dcls_iter(_global_runtime.store,j,CCHR_CONS_TYPE_gcd_1,{
    cchr_entry_t *ent=dcls_ptr(_global_runtime.store,j);
    printf("gcd(%llu)\n",(unsigned long long)(ent->data.gcd_1.arg1));
  })
  return 0;
}

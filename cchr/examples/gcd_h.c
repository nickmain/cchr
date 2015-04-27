#line 1 "gcd.cchr"
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#line 7 "gcd.c"
#undef CONSLIST
#define CONSLIST(CB) CB##_D(gcd_1)

#undef ARGLIST_gcd_1
#define ARGLIST_gcd_1(CB,...) CB##_D(arg1,uint64_t,__VA_ARGS__)
#undef RULELIST_gcd_1
#define RULELIST_gcd_1(CB) CB##_D(gcd_1_triv_R1) CB##_S CB##_D(gcd_1_dec_R1) CB##_S CB##_D(gcd_1_dec_K1)
#undef RELATEDLIST_gcd_1
#define RELATEDLIST_gcd_1(CB) CS##_D(gcd_1)
#undef FORMAT_gcd_1
#define FORMAT_gcd_1 "gcd(%llu)"

#undef HASHLIST_gcd_1
#define HASHLIST_gcd_1(CB,...) CB##_D(hash1,__VA_ARGS__)

#undef HASHDEF_gcd_1_hash1
#define HASHDEF_gcd_1_hash1(CB,...) CB##_D(arg1,uint64_t,__VA_ARGS__)

#undef DESTRUCT_gcd_1
#define DESTRUCT_gcd_1(_arg_1) 
#undef RULEHOOKS_gcd_1
#define RULEHOOKS_gcd_1(CB,...) 

#undef CODELIST_gcd_1_triv_R1
#define CODELIST_gcd_1_triv_R1  \
  CSM_IMMLOCAL(uint64_t,_0,CSM_ARG(gcd_1,arg1)) \
  CSM_IF(CSM_LOCAL(_0) == ( 0ULL ), \
    CSM_KILLSELF(gcd_1) \
    CSM_DESTRUCT(gcd_1,CSM_LOCAL(_0)) \
    CSM_END \
  ) \


#undef CODELIST_gcd_1_dec_R1
#define CODELIST_gcd_1_dec_R1  \
  CSM_LOOP(gcd_1,K1, \
    CSM_IF(CSM_DIFFSELF(K1), \
      CSM_IMMLOCAL(uint64_t,M,CSM_ARG(gcd_1,arg1)) \
      CSM_IMMLOCAL(uint64_t,N,CSM_LARG(gcd_1,K1,arg1)) \
      CSM_IF(CSM_LOCAL(M) >= CSM_LOCAL(N), \
        CSM_KILLSELF(gcd_1) \
        CSM_DEFLOCAL(uint64_t,V,CSM_LOCAL(M) - CSM_LOCAL(N)) \
        CSM_DESTRUCT(gcd_1,CSM_LOCAL(M)) \
        CSM_ADD(gcd_1,CSM_LOCAL(V)) \
        CSM_END \
      ) \
    ) \
  ) \


#undef CODELIST_gcd_1_dec_K1
#define CODELIST_gcd_1_dec_K1  \
  CSM_MAKE(gcd_1) \
  CSM_LOOP(gcd_1,R1, \
    CSM_IF(CSM_DIFFSELF(R1), \
      CSM_IMMLOCAL(uint64_t,M,CSM_LARG(gcd_1,R1,arg1)) \
      CSM_IMMLOCAL(uint64_t,N,CSM_ARG(gcd_1,arg1)) \
      CSM_IF(CSM_LOCAL(M) >= CSM_LOCAL(N), \
        CSM_KILL(R1,gcd_1) \
        CSM_NEEDSELF(gcd_1) \
        CSM_DEFLOCAL(uint64_t,V,CSM_LOCAL(M) - CSM_LOCAL(N)) \
        CSM_DESTRUCT(gcd_1,CSM_LOCAL(M)) \
        CSM_ADD(gcd_1,CSM_LOCAL(V)) \
        CSM_IF(!CSM_ALIVESELF || CSM_REGENSELF, CSM_STROUT("abort") CSM_END) \
      ) \
    ) \
  ) \


#include "cchr_csm.h"
#line 10 "gcd.cchr"


int main(int argc, char **argv) {
  uint64_t a1 = (uint64_t)(argc>1 ? strtoull(argv[1],NULL,0) : 993);
  uint64_t a2 = (uint64_t)(argc>2 ? strtoull(argv[2],NULL,0) : 37);
  cchr_runtime_init();
  cchr_add_gcd_1(a1);
  cchr_add_gcd_1(a2);
  printf("psize=%u\n",(unsigned int)(sizeof(cchr_entry_t)));
  cchr_consloop(j,gcd_1,{printf("gcd(%llu,%llu)=%llu\n",(unsigned long long)a1,(unsigned long long)a2,(unsigned long long)cchr_consarg(j,gcd_1,1));});
  cchr_runtime_free();
  return 0;
}


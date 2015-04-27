#line 1 "fib.cchr"
#include <stdio.h>
#include <stdlib.h>

#ifdef USE_EFENCE
#include <efence.h>
#endif

#line 10 "fib.c"
#undef CONSLIST
#define CONSLIST(CB) CB##_D(fib_2) CB##_S CB##_D(init_1)

#undef PROPHIST_begin
#define PROPHIST_begin(CB,Pid1,...) CB##_I(Pid1,__VA_ARGS__,)
#undef PROPHIST_HOOK_begin
#define PROPHIST_HOOK_begin init_1
#undef RULE_KEPT_begin
#define RULE_KEPT_begin (1)
#undef RULE_REM_begin
#define RULE_REM_begin (0)
#undef PROPHIST_calc
#define PROPHIST_calc(CB,Pid1,Pid2,Pid3,...) CB##_I(Pid2,__VA_ARGS__,CB##_D(Pid1,Pid2,0,__VA_ARGS__) CB##_S(__VA_ARGS__) CB##_D(Pid3,Pid2,1,__VA_ARGS__))
#undef PROPHIST_HOOK_calc
#define PROPHIST_HOOK_calc fib_2
#undef RULE_KEPT_calc
#define RULE_KEPT_calc (3)
#undef RULE_REM_calc
#define RULE_REM_calc (0)
#undef ARGLIST_fib_2
#define ARGLIST_fib_2(CB,...) CB##_D(arg1,int,__VA_ARGS__) CB##_S CB##_D(arg2,uint64_t,__VA_ARGS__)
#undef RULELIST_fib_2
#define RULELIST_fib_2(CB) CB##_D(fib_2_calc_K2) CB##_S CB##_D(fib_2_calc_K3)
#undef RELATEDLIST_fib_2
#define RELATEDLIST_fib_2(CB) CS##_D(init_1) CB##_S CS##_D(fib_2)
#undef FORMAT_fib_2
#define FORMAT_fib_2 "fib(%i,%llu)"

#undef HASHLIST_fib_2
#define HASHLIST_fib_2(CB,...) CB##_D(hash1,__VA_ARGS__)

#undef HASHDEF_fib_2_hash1
#define HASHDEF_fib_2_hash1(CB,...) CB##_D(arg1,int,__VA_ARGS__)

#undef DESTRUCT_fib_2
#define DESTRUCT_fib_2(_arg_1,_arg_2) 
#undef RULEHOOKS_fib_2
#define RULEHOOKS_fib_2(CB,...) CB##_D(fib_2,calc,__VA_ARGS__)

#undef CODELIST_fib_2_calc_K2
#define CODELIST_fib_2_calc_K2  \
  CSM_MAKE(fib_2) \
  CSM_LOOP(init_1,K1, \
    CSM_DEFIDXVAR(fib_2,hash1,K3) \
    CSM_SETIDXVAR(fib_2,hash1,K3,arg1,CSM_ARG(fib_2,arg1)+1) \
    CSM_IDXUNILOOP(fib_2,hash1,K3, \
      CSM_IF(CSM_DIFFSELF(K3), \
        CSM_HISTCHECK(calc, \
          CSM_IMMLOCAL(int,Max,CSM_LARG(init_1,K1,arg1)) \
          CSM_IMMLOCAL(int,N,CSM_ARG(fib_2,arg1)) \
          CSM_IMMLOCAL(int,_0,CSM_LARG(fib_2,K3,arg1)) \
          CSM_IF((CSM_LOCAL(N) + 1 < CSM_LOCAL(Max)), \
            CSM_IMMLOCAL(uint64_t,M1,CSM_ARG(fib_2,arg2)) \
            CSM_IMMLOCAL(uint64_t,M2,CSM_LARG(fib_2,K3,arg2)) \
            CSM_NEEDSELF(fib_2) \
            CSM_HISTADD(calc,K1,self_,K3) \
            CSM_DEFLOCAL(uint64_t,M3,CSM_LOCAL(M1) + CSM_LOCAL(M2)) \
            CSM_ADD(fib_2,CSM_LOCAL(N) + 2,CSM_LOCAL(M3)) \
            CSM_IF(!CSM_ALIVESELF || CSM_REGENSELF, \
              CSM_STROUT("abort") \
	      CSM_IDXUNIEND(fib_2,hash1,K3) \
              CSM_END \
            ) \
          ) \
        ,K1,self_,K3) \
      ) \
    ) \
  ) \


#undef CODELIST_fib_2_calc_K3
#define CODELIST_fib_2_calc_K3  \
  CSM_MAKE(fib_2) \
  CSM_LOOP(init_1,K1, \
    CSM_DEFIDXVAR(fib_2,hash1,K2) \
    CSM_SETIDXVAR(fib_2,hash1,K2,arg1,CSM_ARG(fib_2,arg1)-1) \
    CSM_IDXUNILOOP(fib_2,hash1,K2, \
      CSM_IF(CSM_DIFFSELF(K2), \
        CSM_HISTCHECK(calc, \
          CSM_IMMLOCAL(int,Max,CSM_LARG(init_1,K1,arg1)) \
          CSM_IMMLOCAL(int,N,CSM_LARG(fib_2,K2,arg1)) \
          CSM_IMMLOCAL(int,_0,CSM_ARG(fib_2,arg1)) \
          CSM_IF((CSM_LOCAL(_0)-1 == ( CSM_LOCAL(N) )) && (CSM_LOCAL(N) + 1 < CSM_LOCAL(Max)), \
            CSM_IMMLOCAL(uint64_t,M1,CSM_LARG(fib_2,K2,arg2)) \
            CSM_IMMLOCAL(uint64_t,M2,CSM_ARG(fib_2,arg2)) \
            CSM_NEEDSELF(fib_2) \
            CSM_HISTADD(calc,K1,K2,self_) \
            CSM_DEFLOCAL(uint64_t,M3,CSM_LOCAL(M1) + CSM_LOCAL(M2)) \
            CSM_ADD(fib_2,CSM_LOCAL(N) + 2,CSM_LOCAL(M3)) \
            CSM_IF(!CSM_ALIVESELF || CSM_REGENSELF, CSM_STROUT("abort") CSM_IDXUNIEND(fib_2,hash1,K2) CSM_END) \
          ) \
        ,K1,K2,self_) \
      ) \
    ) \
  ) \


#undef ARGLIST_init_1
#define ARGLIST_init_1(CB,...) CB##_D(arg1,int,__VA_ARGS__)
#undef RULELIST_init_1
#define RULELIST_init_1(CB) CB##_D(init_1_begin_K1) CB##_S CB##_D(init_1_calc_K1) CB##_S CB##_D(init_1_fini_R1)
#undef RELATEDLIST_init_1
#define RELATEDLIST_init_1(CB) CS##_D(fib_2)
#undef FORMAT_init_1
#define FORMAT_init_1 "init(%i)"

#undef HASHLIST_init_1
#define HASHLIST_init_1(CB,...)

#undef DESTRUCT_init_1
#define DESTRUCT_init_1(_arg_1) 
#undef RULEHOOKS_init_1
#define RULEHOOKS_init_1(CB,...) CB##_D(init_1,begin,__VA_ARGS__)

#undef CODELIST_init_1_begin_K1
#define CODELIST_init_1_begin_K1  \
  CSM_MAKE(init_1) \
  CSM_HISTCHECK(begin, \
    CSM_NEEDSELF(init_1) \
    CSM_HISTADD(begin,self_) \
    CSM_ADD(fib_2,0,1ULL) \
    CSM_ADD(fib_2,1,1ULL) \
    CSM_IF(!CSM_ALIVESELF || CSM_REGENSELF, CSM_STROUT("abort") CSM_END) \
  ,self_) \


#undef CODELIST_init_1_calc_K1
#define CODELIST_init_1_calc_K1  \
  CSM_MAKE(init_1) \
  CSM_LOOP(fib_2,K2, \
    CSM_DEFIDXVAR(fib_2,hash1,K3) \
    CSM_SETIDXVAR(fib_2,hash1,K3,arg1,CSM_LARG(fib_2,K2,arg1)+1) \
    CSM_IDXUNILOOP(fib_2,hash1,K3, \
      CSM_IF(CSM_DIFF(K3,K2), \
        CSM_HISTCHECK(calc, \
          CSM_IMMLOCAL(int,Max,CSM_ARG(init_1,arg1)) \
          CSM_IMMLOCAL(int,N,CSM_LARG(fib_2,K2,arg1)) \
          CSM_IMMLOCAL(int,_0,CSM_LARG(fib_2,K3,arg1)) \
          CSM_IF((CSM_LOCAL(N) + 1 < CSM_LOCAL(Max)), \
            CSM_IMMLOCAL(uint64_t,M1,CSM_LARG(fib_2,K2,arg2)) \
            CSM_IMMLOCAL(uint64_t,M2,CSM_LARG(fib_2,K3,arg2)) \
            CSM_NEEDSELF(init_1) \
            CSM_HISTADD(calc,self_,K2,K3) \
            CSM_DEFLOCAL(uint64_t,M3,CSM_LOCAL(M1) + CSM_LOCAL(M2)) \
            CSM_ADD(fib_2,CSM_LOCAL(N) + 2,CSM_LOCAL(M3)) \
            CSM_IF(!CSM_ALIVESELF || CSM_REGENSELF, CSM_STROUT("abort") CSM_IDXUNIEND(fib_2,hash1,K3) CSM_END ) \
          ) \
        ,self_,K2,K3) \
      ) \
    ) \
  ) \


#undef CODELIST_init_1_fini_R1
#define CODELIST_init_1_fini_R1  \
  CSM_IMMLOCAL(int,_0,CSM_ARG(init_1,arg1)) \
  CSM_KILLSELF(init_1) \
  CSM_DESTRUCT(init_1,CSM_LOCAL(_0)) \
  CSM_END \


#undef HASHLIST_init_1
#define HASHLIST_init_1(CB,...)

#include "cchr_csm.h"
#line 15 "fib.cchr"


int main(int argc, char **argv) {
  cchr_runtime_init();
  int a=(argc>1 ? (int)strtol(argv[1],NULL,0) : 92);
  cchr_add_init_1(a);
  cchr_consloop(j,fib_2,{
    printf("fib(%i,%lu)\n",cchr_consarg(j,fib_2,1),(unsigned long)cchr_consarg(j,fib_2,2));
    break;
  });
  cchr_runtime_free();
  return 0;
}


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

#include "dcls.h"
#include "model.h"

enum cchr_cons_type { CCHR_CONS_TYPE_GCD, CCHR_CONS_COUNT };

typedef struct {
  uint64_t arg1;
} cchr_cons_gcd_t;

typedef struct {
  enum cchr_cons_type type;
  int id;
  int gen_num;
  union {
    cchr_cons_gcd_t gcd;
  } data;
} cchr_entry_t;

typedef struct {
  model_t model;
  dcls_declare(cchr_entry_t,store);
  int nextid;
} cchr_runtime_t;

cchr_runtime_t static _global_runtime;

void static inline cchr_fire_gcd(dcls_pid_t pid,uint64_t arg1);

void cchr_runtime_init() {
  dcls_init(_global_runtime.store,CCHR_CONS_COUNT);
  model_init(&(_global_runtime.model));
  _global_runtime.nextid=1;
}

/* either store in constraint store, or destroy */
dcls_pid_t static inline cchr_make_entry(enum cchr_cons_type type) {
  dcls_pid_t ret;
  dcls_alloc(_global_runtime.store,ret);
  dcls_get(_global_runtime.store,ret).id=_global_runtime.nextid++;
  dcls_get(_global_runtime.store,ret).gen_num=0;
  dcls_get(_global_runtime.store,ret).type=type;
  return ret;
}

void static inline cchr_store(dcls_pid_t pid) {
  dcls_add_begin(_global_runtime.store,pid,dcls_get(_global_runtime.store,pid).type);
}

void static inline cchr_kill(dcls_pid_t pid) {
  dcls_empty(_global_runtime.store,pid);
}

void static inline cchr_fire_gcd(dcls_pid_t pid,uint64_t arg1) {
  int doadd=(pid==DCLS_EMPTY_PID);
begin:
  /* <1> gcd(0) <=> true */
  {
    if (arg1==0) {
      //printf("rule 1 on (%llu)\n",(unsigned long long)arg1);
      if (!doadd) cchr_kill(pid);
      //printf("end rule 1 on (%llu)\n",(unsigned long long)arg1);
      return;
    }
  }
  /* <2> gcd(I) \ _gcd(J)_ <=> J >= I | gcd(J-I) */
  {
    dcls_iter(_global_runtime.store,pid2,CCHR_CONS_TYPE_GCD) {
      if ((pid2 != pid) && (arg1 >= dcls_get(_global_runtime.store,pid2).data.gcd.arg1)) {
	uint64_t pid2_arg1=dcls_get(_global_runtime.store,pid2).data.gcd.arg1;
	//printf("rule 2 on (%llu,%llu)\n",(unsigned long long)arg1,(unsigned long long)pid2_arg1);
        if (!doadd) cchr_kill(pid);
        /* staartrecursie */
        pid=DCLS_EMPTY_PID;
        arg1=arg1-pid2_arg1;
        doadd=1;
        goto begin;
        /*cchr_fire_gcd(DCLS_EMPTY_PID,arg1-pid2_arg1);*/
	//printf("end rule 2 on (%llu,%llu)\n",(unsigned long long)arg1,(unsigned long long)pid2_arg1);
	return;
      }
    }
  }
  /* make sure entry is created */
  if (doadd) {
    pid=cchr_make_entry(CCHR_CONS_TYPE_GCD);
    dcls_get(_global_runtime.store,pid).data.gcd.arg1=arg1;
  }
  /* <3> _gcd(I)_ \ gcd(J) <=> J>=I | gcd(J-I) */
  {
    dcls_iter(_global_runtime.store,pid2,CCHR_CONS_TYPE_GCD) {
      if ((pid2 != pid) && (dcls_get(_global_runtime.store,pid2).data.gcd.arg1>=arg1)) {
	uint64_t pid2_arg1=dcls_get(_global_runtime.store,pid2).data.gcd.arg1;
	//printf("rule 3 on (%llu,%llu)\n",(unsigned long long)pid2_arg1,(unsigned long long)arg1);
	cchr_kill(pid2);
	if (doadd) {cchr_store(pid); doadd=0;}
	int oldgen=dcls_get(_global_runtime.store,pid).gen_num;
	int oldid=dcls_get(_global_runtime.store,pid).id;
	cchr_fire_gcd(DCLS_EMPTY_PID,pid2_arg1 - arg1);
	//printf("end rule 3 on (%llu,%llu)\n",(unsigned long long)pid2_arg1,(unsigned long long)arg1);
	if (!dcls_used(_global_runtime.store,pid) ||
	  (dcls_get(_global_runtime.store,pid).id != oldid) ||
	  (dcls_get(_global_runtime.store,pid).gen_num != oldgen)) {
	  return;
	}
      }
    }
  }
  if (doadd) cchr_store(pid);
  //printf("end adding gcd(%llu)\n",(unsigned long long)arg1);
}

int main(void) {
  cchr_runtime_init();
  cchr_fire_gcd(DCLS_EMPTY_PID,10ULL);
  cchr_fire_gcd(DCLS_EMPTY_PID,25ULL);
  printf("size=%u\n",(unsigned int)(sizeof(cchr_entry_t)));
  dcls_iter(_global_runtime.store,j,CCHR_CONS_TYPE_GCD) {
    cchr_entry_t *ent=dcls_ptr(_global_runtime.store,j);
    printf("gcd(%llu)\n",(unsigned long long)(ent->data.gcd.arg1));
  }
  return 0;
}

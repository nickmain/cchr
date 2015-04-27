/****************************************************************************\
| CCHR - A CHR-in-C to C compiler                                            |
| dcls.h - dynamic circular lists, with multiple iterators                   |
| written by Pieter Wuille                                                   |
\****************************************************************************/ 

#ifndef _dcls_h_
#define _dcls_h_ 1

#include <stdint.h>
#ifdef USE_EFENCE
#include "efence.h"
#endif

typedef uint32_t dcls_pid_t;
#define DCLS_EMPTY_PID ((dcls_pid_t)(-1))

/**
 * _d : points to dynamic array
 * _s : size of the dynamic array
 * _fe: points to the first (youngest) empty cell (or DCLS_EMPTY_PID if there are no empty cells)
 * empty cells point to the next empty cell, and have DCLS_EMPTY_PID als _prev
 * the last empty cell points to DCLS_EMPTY_PID
 * cell pid=0 is the filled marker, and points to the first/last real filled (or itself if there are none)
 * things will break if you remove pid 0 from the filled set
 */
 
/* declare var to be a DCLS of type type */
#define dcls_declare(type,var) \
  struct { \
    dcls_pid_t _s,_fe,_id; \
    struct { \
      type _data; \
      dcls_pid_t _prev,_next,_id; \
      int _rc,_ic; \
    } *_d; \
  } var

/* initialize a DCLS var */
#define dcls_init(var,types) do {\
  (var)._s=(types); \
  (var)._d=malloc(sizeof((var)._d[0])*(types)); \
  (var)._id=1; \
  for (int j=0; j<(types); j++) { \
    (var)._d[j]._rc=1; \
    (var)._d[j]._ic=0; \
    (var)._d[j]._next=j; \
    (var)._d[j]._prev=j; \
    (var)._d[j]._id=((var)._id)++; \
  } \
  (var)._fe=DCLS_EMPTY_PID; \
} while(0);

#define dcls_id(var,pid) (((var)._d[(pid)])._id)
#define dcls_get(var,pid) (((var)._d[(pid)])._data)
#define dcls_ptr(var,pid) (&(dcls_get(var,pid)))

#define dcls_iter_first(var,type) ((var)._d[(type)]._next)
#define dcls_iter_isreal(var,pid,type) ((pid) != (type))
#define dcls_iter_rc(var,pid) ((var)._d[(pid)]._rc)
#define dcls_iter_ic(var,pid) ((var)._d[(pid)]._ic)
#define dcls_iter_alive(var,pid) (dcls_iter_rc(var,pid)==2)
#define dcls_iter_next(var,pid) ((var)._d[(pid)]._next)
#define dcls_iter_prev(var,pid) ((var)._d[(pid)]._prev)

#define dcls_iter(var,pid,type,code) { \
  dcls_pid_t pid; \
  dcls_pid_t iter; \
  dcls_iterx(var,pid,iter,type,code); \
}
#define dcls_iterx(var,pid,ns,type,code) { \
  (pid)=dcls_iter_first((var),(type)); \
  while (dcls_iter_isreal((var),(pid),(type))) { \
     if (dcls_iter_alive(var,pid)) { \
       dcls_incic_(var,pid); \
       { code }; \
       int _npid=dcls_iter_next(var,pid); \
       dcls_decic_(var,pid); \
       (pid)=_npid; \
     } else { \
       (pid)=dcls_iter_next(var,pid); \
     } \
  } \
}
#define dcls_iterx_end(var,pid) { \
  dcls_decic_(var,pid); \
}
#define dcls_dec_iterx(ns,CB,...)

#define dcls_uiter(var,pid,type,code) { \
  dcls_pid_t pid; \
  dcls_uiterx(var,pid,_,type,code); \
}
#define dcls_uiterx(var,pid,ns,type,code) { \
  pid=dcls_iter_first((var),(type)); \
  while (dcls_iter_isreal((var),(pid),(type))) { \
     if (dcls_iter_alive(var,pid)) { \
       code \
     } \
     (pid)=dcls_iter_next(var,pid); \
  } \
}
#define dcls_dec_uiterx(ns,CB,...)
  
#define dcls_used(var,pid) dcls_iter_alive(var,pid)

/* ensure a DCLS var has size positions (including the filled markers) */
#define dcls_ensure(var,size) do {\
  dcls_pid_t _ns=(size);\
  if ((var)._s<_ns) {\
    (var)._d=realloc((var)._d,sizeof((var)._d[0])*_ns);\
    int _kp=_ns; \
    while (_kp-- > (var)._s) { \
      (var)._d[_kp]._prev=DCLS_EMPTY_PID; \
      (var)._d[_kp]._next=(var)._fe; \
      (var)._d[_kp]._id=0; \
      (var)._d[_kp]._rc=0; \
      (var)._d[_kp]._ic=0; \
      (var)._fe=_kp; \
    }; \
    (var)._s=_ns; \
  } \
} while(0);

/* allocate a position in DCLS var, and put it in pid */
/* the result will neither be in a filled set, nor in the empty set */
#define dcls_alloc(var,pid) do { \
  if ((var)._fe==DCLS_EMPTY_PID) dcls_ensure(var,(((var)._s+1)*5)/4+3); \
  (pid)=(var)._fe; \
  (var)._fe=(var)._d[(pid)]._next; \
  (var)._d[(pid)]._prev=DCLS_EMPTY_PID; \
  (var)._d[(pid)]._id=((var)._id++); \
  (var)._d[(pid)]._rc=1; \
} while(0);

/* get a position out of the filled set (not added to free set, unless already there) */
#define dcls_remove(var,pid) do { \
  if (dcls_iter_rc(var,pid)==2) { \
    dcls_unadd_(var,pid); \
    dcls_iter_rc(var,pid)=1; \
  } \
} while(0);

#define dcls_unadd_(var,pid) do { \
  if (dcls_iter_ic(var,pid)==0) { \
    dcls_pid_t _prev=(var)._d[(pid)]._prev; \
    dcls_pid_t _next=(var)._d[(pid)]._next; \
    (var)._d[_prev]._next=_next;\
    (var)._d[_next]._prev=_prev;\
    (var)._d[(pid)]._prev=DCLS_EMPTY_PID;\
  } \
} while(0);

/* bring a position into the free set (and get it out of filled set if necessary) */
#define dcls_free(var,pid) do { \
  if (dcls_iter_rc(var,pid)>1) dcls_unadd_(var,pid); \
  if (dcls_iter_rc(var,pid)>0) dcls_release_(var,pid); \
  dcls_iter_rc(var,pid)=0; \
} while(0);

#define dcls_release_(var,pid) do { \
  if (dcls_iter_ic(var,pid)==0) { \
    (var)._d[(pid)]._next=(var)._fe;\
    (var)._fe=(pid);\
    (var)._d[(pid)]._id=0;\
  } \
} while(0);

#define dcls_decic_(var,pid) do { \
  dcls_iter_ic(var,pid)--; \
  if (dcls_iter_ic(var,pid)==0) { \
    if (dcls_iter_rc(var,pid)<2) dcls_unadd_(var,pid); \
    if (dcls_iter_rc(var,pid)<1) dcls_release_(var,pid); \
  } \
} while(0);

#define dcls_incic_(var,pid) do { \
  dcls_iter_ic(var,pid)++; \
} while(0);

/* bring a position into the filled set */
/* @pre: position was allocated (not in filled or empty set) */
#define dcls_add_begin(var,pid,type) do { \
  (var)._d[(pid)]._next=(var)._d[(type)]._next; \
  (var)._d[(pid)]._prev=(type); \
  (var)._d[(pid)]._rc=2; \
  (var)._d[(var)._d[(type)]._next]._prev=(pid); \
  (var)._d[(type)]._next=(pid); \
} while(0);

#define dcls_add_end(var,pid,type) do { \
  (var)._d[(pid)]._prev=(var)._d[(type)]._prev; \
  (var)._d[(pid)]._next=(type); \
  (var)._d[(pid)]._rc=2; \
  (var)._d[(var)._d[(type)]._prev]._next=(pid); \
  (var)._d[(type)]._prev=(pid); \
} while(0);

#define dcls_destruct(var) do { \
  free((var)._d); \
  (var)._d=NULL; \
  (var)._s=0; \
} while(0);

#endif

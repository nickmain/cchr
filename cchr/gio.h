#ifndef _GIO_H_
#define _GIO_H_ 

#include "semtree.h"

typedef enum {
  GIO_TYPE_ITER,
  GIO_TYPE_IDXITER,
  GIO_TYPE_LOGITER,
  GIO_TYPE_OUT,
  GIO_TYPE_DIFF,
  GIO_TYPE_VAR
} gio_type_t;

#define GIO_TYPE_ITER 0
#define GIO_TYPE_IDXITER 1
#define GIO_TYPE_LOGITER 2
#define GIO_TYPE_OUT 3
#define GIO_TYPE_DIFF 4
#define GIO_TYPE_VAR 5

typedef struct {
  gio_type_t type;
  union {
    struct {
      uint32_t cot; /* pos in rem/kept + (rem ? 1<<32 : 0) */
    } iter;
    struct {
      uint32_t cot;
      alist_declare(sem_expr_t*,args);
    } idxiter;
    struct {
      uint32_t cot;
      int pos;
      sem_expr_t arg;
    } logiter;
    int out;
    struct {
      uint32_t cot[2];
    } diff;
    int var;
  } data;
  int uni; /* might be universal instead of existential */
} gio_entry_t;

typedef struct {
  alist_declare(gio_entry_t,order);
} gio_t;

void gio_generate(sem_cchr_t *chr, sem_rule_t *rule, gio_t *gio, int activ);
void gio_destruct(gio_t *gio);

#endif

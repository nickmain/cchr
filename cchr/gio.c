/****************************************************************************\
| CCHR - A CHR-in-C to C compiler                                            |
| gio.c - guard iteration order optimizer                                    |
| written by Pieter Wuille                                                   |
\****************************************************************************/

#include <stdint.h>
#include <stdlib.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>

#include "gio.h"
#include "analyse.h"
#include "alist.h"

#ifndef NO_IDX
#define NO_IDX 0
#endif

int static gio_test_idxeq(sem_cchr_t *chr,sem_rule_t *rule, int cot, int rem, sem_expr_t *expr, gio_entry_t *gioe);

void static gio_entry_init(gio_entry_t *entry, gio_type_t type) {
  entry->type=type;
  if (type==GIO_TYPE_IDXITER) {
    alist_init(entry->data.idxiter.args);
  }
  if (type==GIO_TYPE_LOGITER) {
    sem_expr_init(&(entry->data.logiter.arg));
  }
}

void static gio_entry_destruct(gio_entry_t *entry) {
  if (entry->type==GIO_TYPE_IDXITER) {
    for (int j=0; j<alist_len(entry->data.idxiter.args); j++) {
      sem_expr_t *ex=alist_get(entry->data.idxiter.args,j);
      if (ex) {
        sem_expr_destruct(ex);
        free(ex);
      }
    }
    alist_free(entry->data.idxiter.args);
  }
  if (entry->type==GIO_TYPE_LOGITER) {
    sem_expr_destruct(&(entry->data.logiter.arg));
  }
}

void static gio_init(gio_t *gio) {
  alist_init(gio->order);
}

void gio_destruct(gio_t *gio) {
  for (int i=0; i<alist_len(gio->order); i++) {
    gio_entry_destruct(alist_ptr(gio->order,i));
  }
  alist_free(gio->order);
}

int static gio_checkcdep(uint32_t *order, int n, sem_cdeps_t *cdp) {
  for (int c=0; c<alist_len(cdp->co); c++) {
    int search=alist_get(cdp->co,c);
    int found=0;
    for (int k=0; k<n; k++) {
      if (search==order[k]) {found=1; break;}
    }
    if (!found) return 0;
  }
  return 1;
}

void static gio_genorder(sem_cchr_t *chr, sem_rule_t *rule, uint32_t *order, gio_t *out) {
  int size=alist_len(rule->head[SEM_RULE_LEVEL_REM])+alist_len(rule->head[SEM_RULE_LEVEL_KEPT]);
  int n=1;
  int nguards=alist_len(rule->out[0]);
  int *gd=malloc(sizeof(int)*nguards); /* 0=todo, 1=done, -1=doing */
  for (int i=0; i<nguards; i++) gd[i]=0;
  int uni=!(order[0] & (1<<31));
  do {
    for (int p=0; p<nguards; p++) {
      if (gd[p]==0) {
	if (gio_checkcdep(order,n,&(alist_get(rule->out[0],p).cdeps))) {
	  gd[p]=1;
	  gio_entry_t entry;
	  gio_entry_init(&entry,GIO_TYPE_OUT);
	  entry.data.out=p;
	  entry.uni=uni;
	  alist_add(out->order,entry);
	}
      }
    }
    if (n<size) {
      uint32_t cot=order[n];
      int rem=0;
      if (cot & (1<<31)) {
        cot ^= (1<<31);
	rem=1;
      }
      sem_conocc_t *co=alist_ptr(rule->head[rem ? SEM_RULE_LEVEL_REM : SEM_RULE_LEVEL_KEPT],cot);
      gio_entry_t entry;
      gio_entry_init(&entry,GIO_TYPE_IDXITER);
      entry.data.idxiter.cot=order[n];
      alist_ensure(entry.data.idxiter.args,alist_len(co->args));
      int haveidx=0;
      for (int i=0; i<alist_len(co->args); i++) alist_add(entry.data.idxiter.args,NULL);
#if NO_IDX == 0
      for (int g=0; g<nguards; g++) { /* loop over all guards */
        if (gd[g]==0) { /* if this particular guard is still to be checked */
          sem_out_t *sot=alist_ptr(rule->out[0],g);
          if (sot->type==SEM_OUT_TYPE_EXP) { /* if it is a real guard (not a var or stm) */
	    if (gio_checkcdep(order,n+1,&(sot->cdeps))) { /* and we have all prerequisites */
	      int ret=(gio_test_idxeq(chr,rule,cot,rem,&(sot->data.exp),&entry));
	      if (ret) {
	        if (ret>haveidx) { /* better type index created for this guard, discard previous matches */
		  for (int r=0; r<nguards; r++) {
		    if (gd[r]==-1) gd[r]=0;
		  }
		}
		gd[g]=-1;
		haveidx=ret;
	      }
	    }
	  }
        }
      }
      for (int r=0; r<nguards; r++) {
        if (gd[r]==-1) gd[r]=1;
      }
#endif
      if (!haveidx) { /* no indices used, use normal (linked list) iterator */
        gio_entry_destruct(&entry);
	gio_entry_init(&entry,GIO_TYPE_ITER);
	entry.data.iter.cot=order[n];
      }
      entry.uni=uni;
      alist_add(out->order,entry);
      for (int i=0; i<n; i++) { /* loop for DIFF's */
        uint32_t cot2=order[i];
	int rem2=0;
	if (cot2 & (1<<31)) {
	  cot2 ^= (1<<31);
	  rem2=1;
	}
	sem_conocc_t *co2=alist_ptr(rule->head[rem2 ? SEM_RULE_LEVEL_REM : SEM_RULE_LEVEL_KEPT],cot2);
	if (co->constr == co2->constr) {
	  gio_entry_t entry2;
	  gio_entry_init(&entry2,GIO_TYPE_DIFF);
	  entry2.data.diff.cot[0]=order[i];
	  entry2.data.diff.cot[1]=order[n];
	  entry2.uni=uni;
	  alist_add(out->order,entry2);
	}
      }
      for (int i=0; i<alist_len(co->args); i++) { /* so we go on, define variables from this conocc */
        gio_entry_t entry3;
	gio_entry_init(&entry3,GIO_TYPE_VAR),
	entry3.data.var=alist_get(co->args,i);
	entry3.uni=uni;
	alist_add(out->order,entry3);
      }
      if (rem) uni=0;
    }
    n++;
  } while(n<=size);
  free(gd);
}

/* returns: 0=no index generated, 1 or higher: this level of match has been generated */
int static gio_test_idxeq_var(sem_cchr_t *chr,int cot, int rem, gio_entry_t *gioe, sem_var_t *var, sem_expr_t *expr,int start,int stop) {
  if (gioe->type==GIO_TYPE_LOGITER) return 0; /* nothing can be added to a LOGITER (level 3) */
  if (!var->local && var->occ[SEM_RULE_LEVEL_REM]==rem && var->pos==cot) {
    if (alist_ptr(chr->types,var->type)->log_ground>=0) { /* this is a logical variable, create LOGITER (level 3) */
      int cot=gioe->data.idxiter.cot;
      gio_entry_destruct(gioe);
      gio_entry_init(gioe,GIO_TYPE_LOGITER);
      gioe->data.logiter.cot=cot;
      gioe->data.logiter.pos=var->poss;
      sem_expr_copy(expr,&(gioe->data.logiter.arg),start,stop);
      return 3;
    }
    if (alist_get(gioe->data.idxiter.args,var->poss)==NULL) { /* add to (existing) IDXITER (level 2) */
      sem_expr_t *nex=malloc(sizeof(sem_expr_t));
      sem_expr_copy(expr,nex,start,stop);
      alist_get(gioe->data.idxiter.args,var->poss)=nex;
      return 2;
    }
    /* level 1 (ITER) will not be generated by this function */
  }
  return 0;
}

/* returns: see gio_test_idxeq_var */
int static gio_test_idxeq(sem_cchr_t *chr,sem_rule_t *rule, int cot, int rem, sem_expr_t *expr, gio_entry_t *gioe) {
  if (alist_len(expr->parts)==1) {
    sem_exprpart_t *ep=alist_ptr(expr->parts,0);
    if (ep->type==SEM_EXPRPART_TYPE_FUN) {
      if (!strcmp(ep->data.fun.name,"alt")) {
        for (int i=0; i<alist_len(ep->data.fun.args); i++) {
	  int ret=(gio_test_idxeq(chr,rule,cot,rem,alist_ptr(ep->data.fun.args,i),gioe));
	  if (ret) return ret;
        }
        return 0;
      }
      if (alist_len(ep->data.fun.args)==2) {
        sem_expr_t *e[2];
        e[0]=alist_ptr(ep->data.fun.args,0);
        e[1]=alist_ptr(ep->data.fun.args,1);
        for (int k=0; k<2; k++) {
          if (alist_len(e[k]->parts)==1 && alist_get(e[k]->parts,0).type==SEM_EXPRPART_TYPE_VAR) {
	    int varid=alist_get(e[k]->parts,0).data.var;
	    sem_var_t *var=alist_ptr(rule->vt.vars,varid);
	    sem_vartype_t *vtype=alist_ptr(chr->types,var->type);
            char *lecf=vtype->equality;
            if (lecf == NULL) lecf="eq";
//            fprintf(stderr,"[eq test on type %s: expect %s, have %s]\n",vtype->name,lecf,ep->data.fun.name);
            if (lecf && !strcmp(ep->data.fun.name,lecf)) {
	      int ret=(gio_test_idxeq_var(chr,cot,rem,gioe,var,e[1-k],0,-1));
	      if (ret) return ret;
	    }
	  }
        }
      }
    }
  }
  if (alist_len(expr->parts)>=3) {
    sem_exprpart_t *ep1=alist_ptr(expr->parts,1);
    if (ep1->type==SEM_EXPRPART_TYPE_LIT && !strcmp(ep1->data.lit,"==")) {
      sem_exprpart_t *ep0=alist_ptr(expr->parts,0);
      if (ep0->type==SEM_EXPRPART_TYPE_VAR) {
        int ret=(gio_test_idxeq_var(chr,cot,rem,gioe,alist_ptr(rule->vt.vars,ep0->data.var),expr,2,-1));
	if (ret) return ret;
      }
    }
    sem_exprpart_t *el1=alist_ptr(expr->parts,alist_len(expr->parts)-2);
    if (el1->type==SEM_EXPRPART_TYPE_LIT && !strcmp(el1->data.lit,"==")) {
      sem_exprpart_t *el0=alist_ptr(expr->parts,alist_len(expr->parts)-1);
      if (el0->type==SEM_EXPRPART_TYPE_VAR) {
        int ret=(gio_test_idxeq_var(chr,cot,rem,gioe,alist_ptr(rule->vt.vars,el0->data.var),expr,0,alist_len(expr->parts)-2));
	if (ret) return ret;
      }
    }
  }
  return 0;
}

double static gio_score(sem_cchr_t *chr, sem_rule_t *rule, gio_t *gio) {
  double ret=0.01;
  double val=1.0;
  for (int i=0; i<alist_len(gio->order); i++) {
    gio_entry_t *entry=alist_ptr(gio->order,i);
    switch (entry->type) {
      case GIO_TYPE_ITER: {
        ret += val*0.5;
	if (entry->uni) {
	  val *= 10;
	} else {
	  val *= 5;
	}
	ret += val*0.3;
	break;
      }
      case GIO_TYPE_IDXITER: {
        int k=0;
	for (int l=0; l<alist_len(entry->data.idxiter.args); l++) {
	  if (alist_get(entry->data.idxiter.args,l)==NULL) k++;
	}
	if (entry->uni) {
          ret += val*5.0; /* universal idx lookups have a terrible constant-time overhead */
	  val *= pow(10.0,(double)k/alist_len(entry->data.idxiter.args));
	  ret += val*0.4;
	} else {
	  ret += val*1.0;
	  val *= pow(10.0,(double)k/alist_len(entry->data.idxiter.args))/2;
	  ret += val*0.4;
	}
	break;
      }
      case GIO_TYPE_LOGITER: {
        if (entry->uni) {
          ret += val*5.0;
	  val *= 3;
	  ret += val*0.4;
	} else {
	  ret += val*1.0;
	  val *= 1.5;
	  ret += val*0.4;
	}
	break;
      }
      case GIO_TYPE_OUT: {
        sem_out_t *out=alist_ptr(rule->out[0],entry->data.out);
	switch (out->type) {
	  case SEM_OUT_TYPE_CON: {
	    break;
	  }
	  case SEM_OUT_TYPE_VAR: {
	    ret += val*0.8;
	    break;
	  }
	  case SEM_OUT_TYPE_STM: {
	    ret += val*2;
	    break;
	  }
	  case SEM_OUT_TYPE_EXP: {
	    ret += val*0.4;
	    val *= 0.8;
	    break;
	  }
	}
	break;
      }
      case GIO_TYPE_DIFF: {
        ret += val*0.5;
	val=val-1;
	break;
      }
      case GIO_TYPE_VAR: {
        ret += val*0.2;
	break;
      }
    }
  }
  return ret;
}

void static gio_iterate(sem_cchr_t *chr, sem_rule_t *rule, uint32_t *order, int *used, int done, gio_t *out, double *score, int rnd) {
  int sizeK=alist_len(rule->head[SEM_RULE_LEVEL_KEPT]);
  int sizeR=alist_len(rule->head[SEM_RULE_LEVEL_REM]);
  if (done==sizeK+sizeR) {
    gio_t ngio;
    gio_init(&ngio);
    gio_genorder(chr,rule,order,&ngio);
    double nscr=gio_score(chr,rule,&ngio);
    if ((*score)==0.0 || nscr<(*score)) {
      gio_destruct(out);
      *out=ngio;
      *score=nscr;
    } else {
      gio_destruct(&ngio);
    }
    return;
  }
  if (rnd) {
    int max=sizeR+sizeK-done;
    int r=rand()%max;
    int p=0;
    do {
      while (p<sizeR+sizeK && used[p]) p++;
      if (r==0) break;
      p++; r--;
    } while(1);
    used[p]=1;
    order[done]=(p>=sizeK ? ((p-sizeK) | (1 << 31)) : p);
    gio_iterate(chr,rule,order,used,done+1,out,score,rnd);
    used[p]=0;
  } else {
    int k=0;
    for (int p=0; p<2; p++) {
      for (int s=0; s<(p?sizeR:sizeK); s++) {
        if (!used[k]) {
          used[k]=1;
	  order[done]=s+(p<<31);
	  gio_iterate(chr,rule,order,used,done+1,out,score,rnd);
	  used[k]=0;
        }
        k++;
      }
    }
  }
}

void gio_generate(sem_cchr_t *chr, sem_rule_t *rule, gio_t *gio, int activ) {
  int size=alist_len(rule->head[SEM_RULE_LEVEL_REM])+alist_len(rule->head[SEM_RULE_LEVEL_KEPT]);
  uint32_t *order=malloc(sizeof(uint32_t)*size);
  int *used=malloc(sizeof(int)*size);
  for (int i=0; i<size; i++) used[i]=0;
  int arem=0;
  int apos=activ;
  if (activ & 1<<31) {
    arem=1;
    apos = (apos ^ (1<<31)) + alist_len(rule->head[SEM_RULE_LEVEL_KEPT]);
  }
  used[apos]=1;
  order[0]=activ;
  double score=0.0;
  gio_init(gio);
  if (size>8) {
    for (int i=0; i<10000; i++) {
      gio_iterate(chr,rule,order,used,1,gio,&score,1);
    }
  } else {
    gio_iterate(chr,rule,order,used,1,gio,&score,0);
  }
  free(used);
  free(order);
  return;
}

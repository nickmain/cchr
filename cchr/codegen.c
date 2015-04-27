/****************************************************************************\
| CCHR - A CHR-in-C to C compiler                                            |
| codegen.c - Conversion of semantic tree to C output (using CSM)            |
| written by Pieter Wuille                                                   |
\****************************************************************************/ 

#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>

#include "analyse.h"
#include "semtree.h"
#include "alist.h"
#include "output.h"
#include "gio.h"
#include "sugar_log.h"

#ifdef USE_EFENCE
#include <efence.h>
#endif

char *csm_get_type(sem_cchr_t *chr, int var) {
  if (var<0) return "void /* ERROR */";
  return alist_get(chr->types,var).name;
}

typedef struct {
  char *code;
  int use;
  int cache;
} csm_varuse_t;

typedef struct {
  alist_declare(int,list);
} csm_hashdef_t;

typedef struct {
  char *var;
  char *type;
  char *call;
  char *arg1;
  char *arg2;
  int free; /* 1=var, 2=type, 4=call, 8=arg1, 16=arg2 */
} csm_nsent_t;

typedef struct {
  alist_declare(csm_nsent_t,list);
} csm_nsdef_t;

void static csm_nsdef_init(csm_nsdef_t *def) {
  alist_init(def->list);
}

void static csm_nsdef_destruct(csm_nsdef_t *def) {
  alist_free(def->list);
}

void static csm_hashdef_init(csm_hashdef_t *def) {
  alist_init(def->list);
}

void static csm_hashdef_destruct(csm_hashdef_t *def) {
  alist_free(def->list);
}

typedef struct {
  alist_declare(csm_hashdef_t,defs);
} csm_hashdefs_t;

typedef struct {
  int con;
  int arg;
} csm_logidxa_t;

typedef struct {
  int type;
  alist_declare(csm_logidxa_t,args); /* which args need a logindex */
} csm_logidx_t;

typedef struct {
  alist_declare(csm_logidx_t,list);
} csm_logidxs_t;

void static csm_logidxs_init(csm_logidxs_t *li) {
  alist_init(li->list);
}

void static csm_hashdefs_init(csm_hashdefs_t *hd) {
  alist_init(hd->defs);
}

void static csm_hashdefs_destroy(csm_hashdefs_t *hd) {
  for (int i=0; i<alist_len(hd->defs); i++) {
    csm_hashdef_destruct(alist_ptr(hd->defs,i));
  }
  alist_free(hd->defs);
}

/* will either destruct def, or add it to hd */
void static csm_hashdefs_add(csm_hashdefs_t *hd,csm_hashdef_t *def) {
  for (int k=0; k<alist_len(hd->defs); k++) {
    csm_hashdef_t *chk=alist_ptr(hd->defs,k);
    if (alist_len(chk->list) == alist_len(def->list)) {
      int ok=1;
      for (int l=0; l<alist_len(def->list); l++) {
        if (alist_get(chk->list,l) != alist_get(def->list,l)) { ok=0; break; }
      }
      if (ok) {
        csm_hashdef_destruct(def);
	return;
      }
    }
  }
  alist_add(hd->defs,*def);
}

char static *csm_hashdef_strify(csm_hashdef_t *def) {
  char *ret=malloc(4+(alist_len(def->list)+3)/4);
  strcpy(ret,"idx");
  char hex[]="0123456789ABCDEF";
  for (int i=0; i<(alist_len(def->list)+3)/4; i++) {
    int dig=0;
    for (int k=0; k<4; k++) {
      if (k+4*i<alist_len(def->list)) {
        dig|=((!!alist_get(def->list,k+4*i))<<(k));
      }
    }
    ret[i+3]=hex[dig];
    ret[i+4]=0;
  }
  return ret;
}

void static csm_logidxs_add(csm_logidxs_t *li, int type, int con, int arg) {
  csm_logidx_t *lix=NULL;
  csm_logidx_t lin;
  for (int j=0; j<alist_len(li->list); j++) {
    csm_logidx_t *lx=alist_ptr(li->list,j);
    if (lx->type==type) {
      lix=lx;
      break;
    }
  }
  if (lix==NULL) {
    alist_init(lin.args);
    lin.type=type;
    alist_add(li->list,lin);
    lix=alist_ptr(li->list,alist_len(li->list)-1);
  }
  for (int k=0; k<alist_len(lix->args); k++) {
    csm_logidxa_t *la=alist_ptr(lix->args,k);
    if (la->arg==arg && la->con==con) return;
  }
  csm_logidxa_t laa={.con=con,.arg=arg};
  alist_add(lix->args,laa);
}


/* get output name for a constraint (put it in 'out' buffer) */
int static csm_constr_getname(sem_cchr_t *cchr,int cons,char *out,int size) {
  sem_constr_t *ptr=alist_ptr(cchr->cons,cons);
  return snprintf(out,size,"%s_%i",ptr->name,alist_len(ptr->types));
}

/* get output name for a rule (put it in 'out' buffer) */
int static csm_rule_getname(sem_cchr_t *cchr,int rule,char *out,int size) {
  sem_rule_t *ptr=alist_ptr(cchr->rules,rule);
  if (ptr->name) {
    return snprintf(out,size,"%s",ptr->name);
  } else {
    return snprintf(out,size,"%iR",rule+1);
  }
}

/* get output name for a constraint occurence (put it in 'out' buffer) */
int static csm_conocc_getname(sem_cchr_t *cchr,int cons,int occ,char *out,int size) {
  sem_constr_t *ptr=alist_ptr(cchr->cons,cons);
  int tsize=0;
  sem_ruleocc_t *ro=alist_ptr(ptr->occ,occ);
  tsize+=csm_constr_getname(cchr,cons,out+tsize,size-tsize);
  tsize+=snprintf(out+tsize,size-tsize,"_");
  tsize+=csm_rule_getname(cchr,ro->rule,out+tsize,size-tsize);
  tsize+=snprintf(out+tsize,size-tsize,"_%s%i",ro->type==SEM_RULE_LEVEL_REM ? "R" : "K",ro->pos+1);
  return tsize;
}

csm_varuse_t static *csm_generate_vartable_constr(sem_cchr_t *chr,int coni) {
  sem_constr_t *co=alist_ptr(chr->cons,coni);
  char c[256];
  csm_constr_getname(chr,coni,c,256);
  csm_varuse_t *tbl=malloc(sizeof(csm_varuse_t)*(alist_len(co->types)+1));
  tbl[0].code=make_message("");
  tbl[0].use=0;
  tbl[0].cache=0;
  for (int i=1; i<=alist_len(co->types); i++) {
    tbl[i].code=make_message("arg%i",i);
    tbl[i].use=0;
    tbl[i].cache=0;
  }
  return tbl;
}

csm_varuse_t static *csm_generate_vartable_fmt(sem_cchr_t *chr,int coni) {
  sem_constr_t *co=alist_ptr(chr->cons,coni);
  char c[256];
  csm_constr_getname(chr,coni,c,256);
  csm_varuse_t *tbl=malloc(sizeof(csm_varuse_t)*(alist_len(co->types)+1));
  tbl[0].code=make_message("");
  tbl[0].use=0;
  tbl[0].cache=0;
  for (int i=1; i<=alist_len(co->types); i++) {
    tbl[i].code=make_message("CSM_ARG(%s,arg%i)",c,i);
    tbl[i].use=0;
    tbl[i].cache=0;
  }
  return tbl;
}

csm_varuse_t static *csm_generate_vartable_killadd(sem_cchr_t *chr,int coni) {
  sem_constr_t *co=alist_ptr(chr->cons,coni);
  char c[256];
  csm_constr_getname(chr,coni,c,256);
  csm_varuse_t *tbl=malloc(sizeof(csm_varuse_t)*(alist_len(co->types)+1));
  tbl[0].code=make_message("PID");
  tbl[0].use=0;
  tbl[0].cache=0;
  for (int i=1; i<=alist_len(co->types); i++) {
    tbl[i].code=make_message("CSM_PARG(%s,PID,arg%i)",c,i);
    tbl[i].use=0;
    tbl[i].cache=0;
  }
  return tbl;
}

/* generate variable-access table (array of char*'s, each pointing to the code to be used for accessing variable corresponding to array index) */ 
csm_varuse_t static *csm_generate_vartable_rule(sem_cchr_t *chr,sem_rule_t *rule,int arem,int aid, char *ns,int cached) {
  csm_varuse_t *tbl=malloc(sizeof(csm_varuse_t)*alist_len(rule->vt.vars));
  for (int i=0; i<alist_len(rule->vt.vars); i++) {
    sem_var_t *var=alist_ptr(rule->vt.vars,i);
    if (var->local) { /* local variables are accessed as LOCAL's in CSM */
      tbl[i].code=make_message("CSM_LOCAL(%s,%s)",var->name,ns);
      tbl[i].use=0;
      tbl[i].cache=0;
    } else { /* variable defined by head of rule */
      sem_rule_level_t isrem=var->occ[SEM_RULE_LEVEL_REM] ? SEM_RULE_LEVEL_REM : SEM_RULE_LEVEL_KEPT;
      int j=var->pos; /* what constraint occurrence in the rule defines it */
      sem_conocc_t *co=alist_ptr(rule->head[isrem],j);
      int k=var->poss; /* search which argument of that constraint defines it */
      char cona[256];
      csm_constr_getname(chr,co->constr,cona,256);
      if (arem==isrem && j==aid) { /* if this is the active rule, use ARG instead of LARG */
      	tbl[i].code=make_message("CSM_ARG(%s,arg%i)",cona,k+1);
        tbl[i].use=0;
        tbl[i].cache=0;
      } else {
        if (cached) {
          tbl[i].code=make_message("CSM_LOCAL(%s,%s)",var->name,ns);
          tbl[i].use=0;
          tbl[i].cache=0;
	} else {
          tbl[i].code=make_message("CSM_LARG(%s,%s%i,%s,arg%i)",cona,isrem ? "R" : "K",j+1,ns,k+1);
	  tbl[i].use=0;
	  tbl[i].cache=1;
        }
      } 
    }
  }
  return tbl;
}

/* nice destructor for the variable table */
void static csm_destruct_vartable_rule(sem_rule_t *rule,csm_varuse_t *tbl) {
  for (int u=0; u<alist_len(rule->vt.vars); u++) free(tbl[u].code);
  free(tbl);
}

/* nice destructor for the variable table */
void static csm_destruct_vartable_constr(sem_constr_t *con,csm_varuse_t *tbl) {
  for (int u=0; u<=alist_len(con->types); u++) free(tbl[u].code);
  free(tbl);
}

/* generate code for an expression (as a C expression) */
void static csm_generate_expr(sem_expr_t *expr,csm_varuse_t *tbl,output_t *out) {
  int dos=0;
  for (int t=0; t<alist_len(expr->parts); t++) {
    sem_exprpart_t *ep=alist_ptr(expr->parts,t);
    char *str=ep->data.lit;
    switch (ep->type) {
      case SEM_EXPRPART_TYPE_FUN: 
      str=ep->data.fun.name; 
      case SEM_EXPRPART_TYPE_LIT:
      {
        if (!strcmp(str,"}")) {output_string(out," \\\n"); output_unindent(out);dos=0;}
        if (dos && (isalnum(str[strlen(str)-1]) || str[strlen(str)-1]=='_')) output_fmt(out," ");
        dos=1;
	output_fmt(out,"%s",str);
	if (!strcmp(str,"}")) {output_string(out," \\\n");dos=0;}
	if (!strcmp(str,";")) {output_string(out," \\\n");dos=0;}
	if (!strcmp(str,"{")) {output_indent(out," \\","");dos=0;}
	if (!isalnum(str[strlen(str)-1]) && str[strlen(str)-1]!='_') dos=0;
	break;
      }
      case SEM_EXPRPART_TYPE_VAR: {
        if (dos && (isalnum(str[strlen(str)-1]) || str[strlen(str)-1]=='_')) output_fmt(out," ");
        dos=1;
        output_fmt(out,"%s",tbl[ep->data.var].code);
	tbl[ep->data.var].use++;
	break;
      }
    }
    if (ep->type==SEM_EXPRPART_TYPE_FUN) {
      output_string(out,"(");
      for (int i=0; i<alist_len(ep->data.fun.args); i++) {
        if (i) output_string(out,",");
        csm_generate_expr(alist_ptr(ep->data.fun.args,i),tbl,out);
      }
      output_string(out,")");
      dos=0;
    }
  }
}

void static csm_generate_out(sem_cchr_t *chr,sem_rule_t *rule,csm_varuse_t *tbl,output_t *out,sem_out_t *so,int tail, char *ns, char *con, csm_nsdef_t *nsd) {
  switch (so->type) {
    case SEM_OUT_TYPE_CON: {
      char coo[256];
      csm_constr_getname(chr,so->data.con.constr,coo,256);
      if (!tail) output_fmt(out,"CSM_SAVE(%s,%s) \\\n",con,ns);
      if (alist_len(so->data.con.args)==0) {
        output_fmt(out,tail ? "CSM_TADDE(%s) \\\n" : "CSM_ADDE(%s) \\\n",coo);
      } else {
        output_fmt(out,tail ? "CSM_TADD(%s" : "CSM_ADD(%s",coo);
        for (int j=0; j<alist_len(so->data.con.args); j++) {
          output_fmt(out,",");
          csm_generate_expr(alist_ptr(so->data.con.args,j),tbl,out);
        }
        output_fmt(out,") \\\n");
      }
      if (!tail) output_fmt(out,"CSM_LOAD(%s,%s) \\\n",con,ns);
      break;
    }
    case SEM_OUT_TYPE_EXP: {
      output_fmt(out,"CSM_IF(");
      csm_generate_expr(&(so->data.exp),tbl,out);
      output_indent(out,", \\",") \\");
      break;
    }
    case SEM_OUT_TYPE_STM: {
      output_indent(out,"CSM_NATIVE( { \\","} ) \\\n");
      csm_generate_expr(&(so->data.exp),tbl,out);
      output_unindent(out);
      break;
    }
    case SEM_OUT_TYPE_VAR: {
      sem_var_t *var=alist_ptr(rule->vt.vars,so->data.var);
      if (alist_len(var->def.parts)>0) {
        output_fmt(out,"CSM_DEFLOCAL(%s,%s,",var->name,ns);
        csm_generate_expr(&(var->def),tbl,out);
        output_fmt(out,") \\\n");
      }
      csm_nsent_t ent={.var=var->name,.type=csm_get_type(chr,var->type),.call="CSM_DECLOCAL",.free=0,.arg1=NULL,.arg2=NULL};
      alist_add(nsd->list,ent);
      break;
    }
  }
}

void static csm_generate_body(sem_cchr_t *chr,sem_rule_t *rule,csm_varuse_t *tbl,output_t *out, int rem, char *ns, char *con, csm_nsdef_t *nsd) {
  int l=0;
  int *conocc_td=malloc(sizeof(int)*alist_len(rule->head[SEM_RULE_LEVEL_REM]));
  int ntd=0;
  for (int j=0; j<alist_len(rule->head[SEM_RULE_LEVEL_REM]); j++) {
    sem_conocc_t *co=alist_ptr(rule->head[SEM_RULE_LEVEL_REM],j);
    sem_constr_t *con=alist_ptr(chr->cons,co->constr);
    int val=(alist_len(con->destr.parts)>0);
    conocc_td[j]=val;
    ntd+=val;
  }
  while(1) {
    for (int k=0; k<alist_len(rule->head[SEM_RULE_LEVEL_REM]); k++) { /* destruction of conocc's whose variables are completely used */
      sem_conocc_t *co=alist_ptr(rule->head[SEM_RULE_LEVEL_REM],k);
      char cc[256];
      csm_constr_getname(chr,co->constr,cc,256);
      int ok=conocc_td[k];
      int w=0;
      while (ok && w<alist_len(co->args)) {
        sem_var_t *var=alist_ptr(rule->vt.vars,alist_get(co->args,w));
        if (tbl[alist_get(co->args,w)].use != var->occ[SEM_RULE_LEVEL_BODY]) ok=0;
        w++;
      }
      if (ok) {
        output_fmt(out,alist_len(co->args) ? "CSM_DESTRUCT(%s" : "CSM_DESTRUCTE(%s",cc);
        for (int l=0; l<alist_len(co->args); l++) {
          output_fmt(out,",%s",tbl[alist_get(co->args,l)].code);
        }
        output_fmt(out,") \\\n");
        conocc_td[k]=0;
        ntd--;
      }
    }
    if (l==alist_len(rule->out[1])) break;
    sem_out_t *so=alist_ptr(rule->out[1],l);
    l++;
    csm_generate_out(chr,rule,tbl,out,so,rem && ntd==0 && alist_len(rule->out[1])==l,ns,con,nsd);
  }
  free(conocc_td);
}

typedef struct {
  int constr;
  int rem;
  int pos;
  int clean;
  char *clt;
} csm_loop_t;

/* generate codelist for a constraint occurence, using GIO */
void static csm_generate_code_gio(sem_cchr_t *cchr,int cons,int occ,output_t *out,csm_hashdefs_t *hd,csm_logidxs_t *ld) {
  csm_nsdef_t nsdef;
  char buf[256];
  csm_conocc_getname(cchr,cons,occ,buf,256);
  char buf2[256];
  csm_constr_getname(cchr,cons,buf2,256);
  sem_constr_t *con=alist_ptr(cchr->cons,cons);
  sem_ruleocc_t *ro=alist_ptr(con->occ,occ);
  int rem=ro->type==SEM_RULE_LEVEL_REM;
  sem_rule_t *ru=alist_ptr(cchr->rules,ro->rule);
  char buf3[256];
  csm_rule_getname(cchr,ro->rule,buf3,256);
  sem_conocc_t *co=alist_ptr(ru->head[rem],ro->pos);

  csm_nsdef_init(&nsdef);

  gio_t gio;
  gio_generate(cchr,ru,&gio,ro->pos | (rem << 31));
  output_fmt(out,"\n");
  output_fmt(out,"#undef NSPACE_%s\n",buf);
  output_fmt(out,"#define NSPACE_%s(VAR) CSM_SPACE_FIRE(%s,%s,VAR)\n",buf,buf,buf2);
  output_fmt(out,"#undef CODELIST_%s\n",buf);
  output_fmt(out,"#define CODELIST_%s ",buf);
  output_indent(out," \\","");
  if (!rem) {
    output_fmt(out,"CSM_MAKE(%s) \\\n",buf2);
  }
  csm_varuse_t *tbl=csm_generate_vartable_rule(cchr,ru,rem,ro->pos,buf,0);
  csm_varuse_t *tbl_c=csm_generate_vartable_rule(cchr,ru,rem,ro->pos,buf,1);
  alist_declare(csm_loop_t,clean);
  alist_init(clean);
  for (int i=0; i<alist_len(co->args); i++) {
    int vid=alist_get(co->args,i);
    if (tbl[vid].cache) {
      sem_var_t *var=alist_ptr(ru->vt.vars,vid);
      output_fmt(out,"CSM_DEFLOCAL(%s,%s,%s) \\\n",var->name,buf,tbl[vid].code);
      csm_nsent_t ent={.var=var->name, .type=csm_get_type(cchr,var->type),.call="CSM_DECLOCAL",.free=0,.arg1=NULL,.arg2=NULL};
      alist_add(nsdef.list,ent);
      tbl[vid].use=0;
      tbl_c[vid].use=0;
    }
  }
  for (int i=0; i<alist_len(gio.order); i++) {
    gio_entry_t *entry=alist_ptr(gio.order,i);
    switch (entry->type) {
      case GIO_TYPE_ITER: {
	uint32_t cid=entry->data.iter.cot & (~(1<<31));
	int ci_rem=!!(entry->data.iter.cot & (1<<31));
	int cicon=alist_get(ru->head[ci_rem],cid).constr;
	char cicon_name[256];
	csm_constr_getname(cchr,cicon,cicon_name,256);
	output_fmt(out,entry->uni ? "CSM_UNILOOP(%s,%s%i,%s," : "CSM_LOOP(%s,%s%i,%s,",cicon_name,ci_rem ? "R" : "K",cid+1,buf);
	output_indent(out," \\",") \\");
	csm_loop_t cl;
	cl.constr=cicon;
	cl.rem=ci_rem;
	cl.pos=cid;
	cl.clean=entry->uni;
        cl.clt="CSM_UNILOOPEND";
	alist_add(clean,cl);
        csm_nsent_t ent={.var=make_message("%s%i",ci_rem ? "R" : "K",cid+1),.type=NULL,.call=(entry->uni ? "CSM_DECUNILOOP" : "CSM_DECLOOP"),.free=1,.arg1=NULL,.arg2=NULL};
        alist_add(nsdef.list,ent);
	break;
      }
      case GIO_TYPE_DIFF: {
	uint32_t cid[2];
	cid[0]=entry->data.diff.cot[0] & (~(1<<31));
	cid[1]=entry->data.diff.cot[1] & (~(1<<31));
	int crem[2];
	crem[0]=(entry->data.diff.cot[0] & (1<<31))!=0;
	crem[1]=(entry->data.diff.cot[1] & (1<<31))!=0;
	int si=-1;
	if (cid[0]==ro->pos && crem[0]==rem) si=0;
	if (cid[1]==ro->pos && crem[1]==rem) si=1;
	if (si>=0) {
	  output_fmt(out,"CSM_IF(CSM_DIFFSELF(%s%i,%s),",crem[1-si] ? "R" : "K",cid[1-si]+1,buf);
	} else {
	  output_fmt(out,"CSM_IF(CSM_DIFF(%s%i,%s%i,%s),",crem[0] ? "R" : "K",cid[0]+1,crem[1] ? "R" : "K",cid[1]+1,buf);
	}
	output_indent(out," \\",") \\");
	break;
      }
      case GIO_TYPE_VAR: {
        int vid=entry->data.var;
	sem_var_t *var=alist_ptr(ru->vt.vars,vid);
	output_fmt(out,"CSM_DEFLOCAL(%s,%s,%s) \\\n",var->name,buf,tbl[vid].code);
        csm_nsent_t ent={.var=var->name, .type=csm_get_type(cchr,var->type), .call="CSM_DECLOCAL",.arg1=NULL,.arg2=NULL};
	alist_add(nsdef.list,ent);
	tbl[vid].use=0;
	tbl_c[vid].use=0;
        break;
      }
      case GIO_TYPE_OUT: {
        sem_out_t *so=alist_ptr(ru->out[0],entry->data.out);
	csm_generate_out(cchr,ru,tbl_c,out,so,0,buf,buf2,&nsdef);
	break;
      }
      case GIO_TYPE_IDXITER: {
	uint32_t cid=entry->data.idxiter.cot & (~(1<<31));
	int ci_rem=!!(entry->data.idxiter.cot & (1<<31));
	int cicon=alist_get(ru->head[ci_rem],cid).constr;
	char cicon_name[256];
	csm_constr_getname(cchr,cicon,cicon_name,256);
	csm_hashdef_t def;
	csm_hashdef_init(&def);
	for (int i=0; i<alist_len(entry->data.idxiter.args); i++) {
	  sem_expr_t *exp=alist_get(entry->data.idxiter.args,i);
	  int ea=(exp!=NULL);
	  alist_add(def.list,ea);
	}
	char *idxname=csm_hashdef_strify(&def);
	// output_fmt(out,"CSM_DEFIDXVAR(%s,%s,%s%i) \\\n",cicon_name,idxname,ci_rem ? "R" : "K",cid+1);
	for (int i=0; i<alist_len(entry->data.idxiter.args); i++) {
	  sem_expr_t *exp=alist_get(entry->data.idxiter.args,i);
	  if (exp) {
	    output_fmt(out,"CSM_IDXVAR(%s,%s,%s%i,%s,arg%i,",cicon_name,idxname,ci_rem ? "R" : "K",cid+1,buf,i+1);
	    csm_generate_expr(exp,tbl_c,out);
	    output_fmt(out,") \\\n");
	  }
	}
	output_fmt(out,"%s(%s,%s,%s%i,%s,",entry->uni ? "CSM_IDXUNILOOP" : "CSM_IDXLOOP",cicon_name,idxname,ci_rem ? "R" : "K",cid+1,buf);
	output_indent(out," \\",") \\");
	csm_loop_t cl;
	cl.constr=cicon;
	cl.rem=ci_rem;
	cl.pos=cid;
	cl.clean=entry->uni;
        cl.clt="CSM_IDXUNILOOPEND";
	alist_add(clean,cl);
	csm_hashdefs_add(&(hd[cicon]),&def);
        csm_nsent_t ent={.var=make_message("%s%i",ci_rem ? "R" : "K",cid+1),.type=NULL,.call=entry->uni ? "CSM_DECIDXUNILOOP" : "CSM_DECIDXLOOP",.free=25,.arg1=make_message("%s",cicon_name),.arg2=make_message("%s",idxname)};
        alist_add(nsdef.list,ent);
	free(idxname);
	break;
      }
      case GIO_TYPE_LOGITER: {
	uint32_t cid=entry->data.logiter.cot & (~(1<<31));
	int ci_rem=!!(entry->data.logiter.cot & (1<<31));
	int cicon=alist_get(ru->head[ci_rem],cid).constr;
	char cicon_name[256];
	csm_constr_getname(cchr,cicon,cicon_name,256);
	char *idxname=make_message("%s_arg%i",cicon_name,entry->data.logiter.pos+1);
	int vartype=alist_get(alist_get(cchr->cons,cicon).types,entry->data.logiter.pos);
	csm_logidxs_add(ld,vartype,cicon,entry->data.logiter.pos);
	sem_vartype_t *vt=alist_ptr(cchr->types,vartype);
	output_fmt(out,"%s(%s,%s%i,%s,%s%s,%s,",entry->uni ? "CSM_LOGUNILOOP" : "CSM_LOGLOOP",cicon_name,ci_rem ? "R" : "K",cid+1,buf,"",idxname,vt->name);
	csm_generate_expr(&(entry->data.logiter.arg),tbl_c,out);
	free(idxname);
	output_indent(out,", \\",") \\");
	csm_loop_t cl;
	cl.constr=cicon;
	cl.rem=ci_rem;
	cl.pos=cid;
	cl.clean=entry->uni;
        cl.clt="CSM_LOGUNILOOPEND";
	alist_add(clean,cl);
        csm_nsent_t ent={.var=make_message("%s%i",ci_rem ? "R" : "K",cid+1),.type=NULL,.call=entry->uni ? "CSM_DECLOGUNILOOP" : "CSM_DECLOGLOOP",.free=1,.arg1=NULL,.arg2=NULL};
        alist_add(nsdef.list,ent);
	break;
      }
    }
  }
  char *end=NULL;
  int ncons=alist_len(ru->head[SEM_RULE_LEVEL_KEPT])+alist_len(ru->head[SEM_RULE_LEVEL_REM]);
  if (ru->hook>=0) {
    output_fmt(out,"CSM_HISTCHECK(%s",buf3);
    end=make_message("");
    for (int ci=0; ci<ncons; ci++) {
      if (ci==ro->pos) {
        char *nend=make_message("%s,CSM_PID_SELF(%s)",end,buf); free(end); end=nend;
      } else {
        char *nend=make_message("%s,CSM_PID(K%i,%s)",end,ci+1,buf); free(end); end=nend;
      }
    }
    char *nend=make_message("%s) \\",end); free(end); end=nend;
    output_indent(out,", \\",end);
  }
  for (int k=0; k<alist_len(ru->head[SEM_RULE_LEVEL_REM]); k++) {
    sem_conocc_t *co=alist_ptr(ru->head[SEM_RULE_LEVEL_REM],k);
    char cc[256];
    csm_constr_getname(cchr,co->constr,cc,256);
    if (!rem || ro->pos!=k) {
      output_fmt(out,"CSM_KILL(R%i,%s,%s) \\\n",k+1,buf,cc);
    } else {
      output_fmt(out,"CSM_KILLSELF(%s) \\\n",cc);
    }
  }
  if (!rem) {
    output_fmt(out,"CSM_NEEDSELF(%s) \\\n",buf2);
  }
  if (ru->hook>=0) {
    output_fmt(out,"CSM_HISTADD(%s%s\n",buf3,end);
  }
  csm_destruct_vartable_rule(ru,tbl_c); /* destruct the vartable and recreate it, so the body generation starts with zeroed varuses */
  tbl_c=csm_generate_vartable_rule(cchr,ru,rem,ro->pos,buf,1);
  csm_generate_body(cchr,ru,tbl_c,out,rem,buf,buf2,&nsdef);
  
  int cond=1;
  if (cond) { /* check for active constraint to be alive */
    if (rem) {
      output_fmt(out,"CSM_END \\\n");
      cond=0;
    } else {
      output_fmt(out,"CSM_DEADSELF(%s,",buf2);
      output_indent(out," \\",") \\\n");
      for (int k=0; k<alist_len(clean); k++) {
        csm_loop_t *cl=alist_ptr(clean,k);
        if (cl->clean) {
          char chc[256];
          csm_constr_getname(cchr,cl->constr,chc,256);
          output_fmt(out,"%s(%s,%s%i,%s) \\\n",cl->clt,chc,cl->rem ? "R" : "K",cl->pos+1,buf);
        }
      }
      output_fmt(out,"CSM_END \\\n");
      output_unindent(out);
    }
  }
  int pp=0;
  while (cond && pp<alist_len(clean)-1) { /* check for other constraints to be alive */
    csm_loop_t *cl=alist_ptr(clean,pp);
    if (cl->rem) {
      output_fmt(out,"CSM_LOOPNEXT(%s%i,%s) \\\n","R",cl->pos+1,buf);
      cond=0;
    } else {
      output_fmt(out,"CSM_DEAD(%s%i,%s",cl->rem ? "R" : "K",cl->pos+1,buf);
      output_indent(out,", \\",") \\\n");
      for (int k2=pp+1; k2<alist_len(clean); k2++) {
        csm_loop_t *cl2=alist_ptr(clean,k2);
	if (cl2->clean) {
	  char chc[256];
          csm_constr_getname(cchr,cl2->constr,chc,256);
          output_fmt(out,"%s(%s,%s%i,%s) \\\n",cl2->clt,chc,cl2->rem ? "R" : "K",cl2->pos+1,buf);
	}
      }
      output_fmt(out,"CSM_LOOPNEXT(%s%i,%s) \\\n",cl->rem ? "R" : "K",cl->pos+1,buf);
      output_unindent(out);
    }
    pp++;
  }
  csm_destruct_vartable_rule(ru,tbl);
  csm_destruct_vartable_rule(ru,tbl_c);
  output_unindent_till(out,0);
  if (end) free(end);
  output_char(out,'\n');
  output_fmt(out,"#undef NSLIST_%s\n",buf);
  output_fmt(out,"#define NSLIST_%s(CB,...) ",buf);
  for (int i=0; i<alist_len(nsdef.list); i++) {
    output_string(out," CB##_S");
    csm_nsent_t *ent=alist_ptr(nsdef.list,i);
    if (ent->call) {
      if (ent->type) {
        output_fmt(out," %s(%s,%s,CB,",ent->call,ent->type,ent->var);
      } else {
        output_fmt(out," %s(%s,CB,",ent->call,ent->var);
      }
    } else {
      output_fmt(out," CB##_D(%s,%s,",ent->type,ent->var);
    }
    if (ent->arg1) output_fmt(out,"%s,",ent->arg1);
    if (ent->arg2) output_fmt(out,"%s,",ent->arg2);
    if (ent->free & 1) free(ent->var);
    if (ent->free & 2) free(ent->type);
    if (ent->free & 4) free(ent->call);
    if (ent->free & 8) free(ent->arg1);
    if (ent->free & 16) free(ent->arg2);
    output_string(out,"__VA_ARGS__) ");
  }
  output_char(out,'\n');
  gio_destruct(&gio);
  alist_free(clean);
  csm_nsdef_destruct(&nsdef);
}

/* generate code for a CCHR block (based on the semantic tree) */
void csm_generate(sem_cchr_t *in,output_t *out,output_t *header) {
	char buf[256];
	output_fmt(header,"#include \"cchr_csm.h\"\n\n");
	output_fmt(out,"#undef CONSLIST\n");
	output_fmt(out,"#define CONSLIST(CB) ");
	for (int i=0; i<alist_len(in->cons); i++) {
		if (i) output_fmt(out," CB##_S ");
		csm_constr_getname(in,i,buf,256);
		output_fmt(out,"CB##_D(%s)",buf);
	}
	output_fmt(out,"\n\n");
/*	for (int i=0; i<alist_len(in->cons); i++) {
		csm_constr_getname(in,i,buf,256);
		output_fmt(out,"#undef NSPACE_%s\n",buf);
		output_fmt(out,"#define NSPACE_%s(VAR) CSM_SPACE_CON(%s,VAR)\n\n",buf,buf);
	}*/
	for (int i=0; i<alist_len(in->rules); i++) {
		sem_rule_t *r=alist_ptr(in->rules,i);
		if (alist_len(r->head[SEM_RULE_LEVEL_REM])==0) {
			char buf2[256];
			csm_rule_getname(in,i,buf2,256);
			output_fmt(out,"#undef PROPHIST_%s\n",buf2);
			output_fmt(out,"#define PROPHIST_%s(CB",buf2);
			for (int j=0; j<alist_len(r->head[SEM_RULE_LEVEL_KEPT]); j++) {
				output_fmt(out,",Pid%i",j+1);
			}
			output_fmt(out,",...) CB##_I(Pid%i,__VA_ARGS__,",r->hook+1);
			int jj=0;
			for (int j=0; j<alist_len(r->head[SEM_RULE_LEVEL_KEPT]); j++) {
				if (j!=r->hook) {
					if (j) output_string(out," CB##_S(__VA_ARGS__) ");
					output_fmt(out,"CB##_D(Pid%i,Pid%i,%i,__VA_ARGS__)",j+1,r->hook+1,jj++);
				}
			}
			output_string(out,")\n");
			csm_constr_getname(in,alist_get(r->head[SEM_RULE_LEVEL_KEPT],r->hook).constr,buf,256);
			output_fmt(out,"#undef PROPHIST_HOOK_%s\n",buf2);
			output_fmt(out,"#define PROPHIST_HOOK_%s %s\n",buf2,buf);
			output_fmt(out,"#undef RULE_KEPT_%s\n",buf2);
			output_fmt(out,"#define RULE_KEPT_%s (%i)\n",buf2,alist_len(r->head[SEM_RULE_LEVEL_KEPT]));
			output_fmt(out,"#undef RULE_REM_%s\n",buf2);
			output_fmt(out,"#define RULE_REM_%s (%i)\n",buf2,alist_len(r->head[SEM_RULE_LEVEL_REM]));
		}
	}
	csm_hashdefs_t *hd=malloc(sizeof(csm_hashdefs_t)*alist_len(in->cons));
	csm_logidxs_t ld;
	csm_logidxs_init(&ld);
	for (int i=0; i<alist_len(in->cons); i++) {
	  csm_hashdefs_init(&(hd[i]));
	}
	for (int i=0; i<alist_len(in->cons); i++) {
		sem_constr_t *con=alist_ptr(in->cons,i);
		char conn[256];
		csm_constr_getname(in,i,conn,256);
		output_fmt(out,"#undef ARGLIST_%s\n",conn);
		output_fmt(out,"#define ARGLIST_%s(CB,...) ",conn);
		for (int j=0; j<alist_len(con->types); j++) {
			if (j) output_fmt(out," CB##_S ");
			output_fmt(out,"CB##_D(arg%i,%s,__VA_ARGS__)",j+1,csm_get_type(in,alist_get(con->types,j)));
		}
		output_fmt(out,"\n");
		output_fmt(out,"#undef RULELIST_%s\n",conn);
		output_fmt(out,"#define RULELIST_%s(CB) ",conn);
		int jj=0;
		for (int j=0; j<alist_len(con->occ); j++) {
			sem_ruleocc_t *cs=alist_ptr(con->occ,j);
			if (cs->type==SEM_RULE_LEVEL_KEPT || cs->type==SEM_RULE_LEVEL_REM) {
	    			sem_rule_t *ru=alist_ptr(in->rules,cs->rule);
				int rem=cs->type==SEM_RULE_LEVEL_REM;
				sem_conocc_t *co=alist_ptr(ru->head[rem],cs->pos);
				csm_conocc_getname(in,i,j,buf,256);
				if (!co->passive) {
				    if (jj) output_fmt(out," CB##_S ");
				    jj++;
				    output_fmt(out,"CB##_D(%s)",buf);
				} else {
				    output_fmt(out,"/* passive: %s */ ",buf);
				}
			}
		}
		output_string(out,"\n");
		output_fmt(out,"#undef RELATEDLIST_%s\n",conn);
		output_fmt(out,"#define RELATEDLIST_%s(CB) ",conn);
		for (int j=0; j<alist_len(con->related); j++) {
			if (j) output_fmt(out," CB##_S ");
			csm_constr_getname(in,alist_get(con->related,j),buf,256);
			output_fmt(out,"CS##_D(%s)",buf);
		}
		output_char(out,'\n');
		csm_varuse_t *vt=csm_generate_vartable_constr(in,i);
		csm_varuse_t *vtf=csm_generate_vartable_fmt(in,i);
		output_fmt(out,"#undef FORMAT_%s\n",conn);
		output_fmt(out,"#define FORMAT_%s ",conn);
		if (alist_len(con->fmt)>0) {
			csm_generate_expr(&(alist_get(con->fmt,0)),vtf,out);
		} else {
		        output_fmt(out,"\"%s()\"",conn);
		}
		output_string(out,"\n");
		output_fmt(out,"#undef FORMATARGS_%s\n",conn);
		output_fmt(out,"#define FORMATARGS_%s ",conn);
		for (int k=1; k<alist_len(con->fmt); k++) {
		  output_string(out,",");
		  csm_generate_expr(&(alist_get(con->fmt,k)),vtf,out);
		}
		output_string(out,"\n");
		output_char(out,'\n');
		output_fmt(out,"#undef DESTRUCT_%s\n",conn);
		output_fmt(out,"#define DESTRUCT_%s(",conn);
		for (int l=0; l<alist_len(con->types); l++) {
			if (l) output_fmt(out,",");
			output_fmt(out,"arg%i",l+1);
		}
		output_fmt(out,") ");
		if (alist_len(con->destr.parts)>0) {
			csm_generate_expr(&(con->destr),vt,out);
		}
		output_string(out,"\n");
		output_fmt(out,"#undef CONSTRUCT_%s\n",conn);
		output_fmt(out,"#define CONSTRUCT_%s ",conn);
		if (alist_len(con->init.parts)>0) {
			csm_generate_expr(&(con->init),vtf,out);
		}
		csm_varuse_t *vtak=csm_generate_vartable_killadd(in,i);
		output_string(out,"\n");
		output_fmt(out,"#undef ADD_%s\n",conn);
		output_fmt(out,"#define ADD_%s(PID) ",conn);
		if (alist_len(con->init.parts)>0) {
			csm_generate_expr(&(con->add),vtak,out);
		}
		output_string(out,"\n");
		output_fmt(out,"#undef KILL_%s\n",conn);
		output_fmt(out,"#define KILL_%s(PID) ",conn);
		if (alist_len(con->init.parts)>0) {
			csm_generate_expr(&(con->kill),vtak,out);
		}
		output_string(out,"\n");
		csm_destruct_vartable_constr(con,vtak);
		/*output_fmt(out,"#undef DESTRUCT_PID_%s\n",conn);
		output_fmt(out,"#define DESTRUCT_PID_%s(PID) DESTRUCT_%s(",conn,conn);
		for (int l=0; l<alist_len(con->types); l++) {
			if (l) output_fmt(out,",");
			output_fmt(out,"CSM_LARG(%s,PID,arg%i)",conn,l+1);
		}
		output_fmt(out,")\n");*/
		output_fmt(out,"#undef RULEHOOKS_%s\n",conn);
		output_fmt(out,"#define RULEHOOKS_%s(CB,...) ",conn);
		for (int g=0; g<alist_len(con->hooked); g++) {
			if (g) output_string(out,"CB##_S ");
			char bfk[256];
			csm_rule_getname(in,alist_get(con->hooked,g),bfk,256);
			output_fmt(out,"CB##_D(%s,%s,__VA_ARGS__)",conn,bfk);
		}
		output_string(out,"\n");
		for (int j=0; j<alist_len(con->occ); j++) {
			sem_ruleocc_t *cs=alist_ptr(con->occ,j);
			if (cs->type==SEM_RULE_LEVEL_KEPT || cs->type==SEM_RULE_LEVEL_REM) {
	    			sem_rule_t *ru=alist_ptr(in->rules,cs->rule);
				int rem=cs->type==SEM_RULE_LEVEL_REM;
				sem_conocc_t *co=alist_ptr(ru->head[rem],cs->pos);
				if (!co->passive) {
				    /*csm_generate_code(in,i,j,out);*/
				    csm_generate_code_gio(in,i,j,out,hd,&ld);
				}
			}
		}
		output_string(out,"\n");
		csm_destruct_vartable_constr(con,vt);
		csm_destruct_vartable_constr(con,vtf);
		output_fmt(out,"\n");
	}
	for (int i=0; i<alist_len(in->cons); i++) {
		sem_constr_t *con=alist_ptr(in->cons,i);
		char conn[256];
		csm_constr_getname(in,i,conn,256);
		output_fmt(out,"#undef HASHLIST_%s\n",conn);
		output_fmt(out,"#define HASHLIST_%s(CB,...) ",conn);
		for (int m=0; m<alist_len(hd[i].defs); m++) {
		  if (m) output_fmt(out,"CB##_S ");
		  csm_hashdef_t *def=alist_ptr(hd[i].defs,m);
		  char *dn=csm_hashdef_strify(def);
		  output_fmt(out,"CB##_D(%s,__VA_ARGS__) ",dn);
		  free(dn);
		}
		output_fmt(out,"\n");
		for (int m=0; m<alist_len(hd[i].defs); m++) {
		  csm_hashdef_t *def=alist_ptr(hd[i].defs,m);
		  char *dn=csm_hashdef_strify(def);
		  output_fmt(out,"#undef HASHDEF_%s_%s\n",conn,dn);
		  output_fmt(out,"#define HASHDEF_%s_%s(CB,...) ",conn,dn);
		  int nn=0;
		  for (int j=0; j<alist_len(def->list); j++) {
		    if (alist_get(def->list,j)) {
		      if (nn) output_fmt(out,"CB##_S ");
		      nn=1;
		      output_fmt(out,"CB##_D(arg%i,%s,__VA_ARGS__) ",j+1,csm_get_type(in,alist_get(con->types,j)));
		    }
		  }
		  output_fmt(out,"\n");
		  free(dn);
		}
		output_fmt(out,"\n");
		csm_hashdefs_destroy(&(hd[i]));
	}
	for (int i=0; i<alist_len(ld.list); i++) {
	  csm_logidx_t *li=alist_ptr(ld.list,i);
	  sem_vartype_t *vt=alist_ptr(in->types,li->type);
	  output_fmt(out,"#undef LOGLIST_%s\n",vt->name);
	  output_fmt(out,"#define LOGLIST_%s(CB,...) ",vt->name);
	  for (int m=0; m<alist_len(li->args); m++) {
	    if (m) output_fmt(out,"CB##_S ");
	  }
	  alist_free(li->args);
	  output_fmt(out,"\n");
	}
	output_fmt(out,"CSM_START\n");
	free(hd);
	sugar_log_codegen(in,out,header);
}

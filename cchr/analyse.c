/****************************************************************************\
| CCHR - A CHR-in-C to C compiler                                            |
| analyse.c - syntax tree to semantic tree conversion                        |
| written by Pieter Wuille                                                   |
\****************************************************************************/ 

#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <stdio.h>
#include <assert.h>

#include "alist.h"
#include "parsestr.h"
#include "semtree.h"
#include "analyse.h"

#include "sugar_log.h"

#ifdef USE_EFENCE
#include <efence.h>
#endif

/* do something sprintf-like, but put the output in a malloc'ed block */
char *make_message(const char *fmt, ...) {
  int n, size = 100;
  char *p, *np;
  va_list ap;
  if ((p = malloc (size)) == NULL) return NULL;
  while (1) {
    va_start(ap, fmt);
    n = vsnprintf (p, size, fmt, ap);
    va_end(ap);
    if (n > -1 && n < size) return p;
    if (n > -1) {
      size = n+1;
    } else {
      size *= 2;
    }
    if ((np = realloc (p, size)) == NULL) {
      free(p);
      return NULL;
    } else {
      p = np;
    }
  }
}

/* allocate and copy a string */
char *copy_string(const char *in) {
  if (in==NULL) return NULL;
  char *ret=malloc(strlen(in)+1);
  strcpy(ret,in);
  return ret;
}

void static sem_vartype_init(sem_vartype_t *vt,char *name) {
  vt->name=copy_string(name);
  vt->equality=NULL;
  vt->log_ground=-1;
  alist_init(vt->log_idx);
}

void static sem_vartype_destruct(sem_vartype_t *vt) {
  free(vt->name);
  vt->name=NULL;
  if (vt->equality) {
    free(vt->equality);
    vt->equality=NULL;
  }
  alist_free(vt->log_idx);
}

int sem_cchr_gettype(sem_cchr_t *cchr,char *type) {
  for (int i=0; i<alist_len(cchr->types); i++) {
    if (!strcmp(alist_get(cchr->types,i).name,type)) {
      return i;
    }
  }
  sem_vartype_t nw;
  sem_vartype_init(&nw,type);
  alist_add(cchr->types,nw);
  return alist_len(cchr->types)-1;
}

void sem_cchr_init_base_types(sem_cchr_t *cchr) {
  static char *basetypes[]={"int","signed int","unsigned int","short int","unsigned short int","signed short int","short","unsigned short","signed short","long int","unsigned long int","signed long int","long","unsigned long","signed long","size_t","void*","void *","int *","int*","char","unsigned char","signed char","char*","char *"};
  for (int i=0; i<sizeof(basetypes)/sizeof(basetypes[0]); i++) {
    int t=sem_cchr_gettype(cchr,basetypes[i]);
    alist_ptr(cchr->types,t)->equality=copy_string("eq_primitive");
  }
}

/* initialize a sem_constr_t */
void static sem_constr_init(sem_constr_t* con,char *name) {
  alist_init(con->types);
  alist_init(con->occ);
  alist_init(con->hooked);
  alist_init(con->related);
  alist_init(con->fmt);
  sem_expr_init(&(con->destr));
  sem_expr_init(&(con->init));
  con->name=name;
}

/* destruct a sem_constr_t */
void static sem_constr_destruct(sem_constr_t *con) {
  free(con->name);
  alist_free(con->occ);
  alist_free(con->hooked);
  alist_free(con->related);
  alist_free(con->types);
  for (int i=0; i<alist_len(con->fmt); i++) {
    sem_expr_destruct(alist_ptr(con->fmt,i));
  }
  alist_free(con->fmt);
  sem_expr_destruct(&(con->destr));
  sem_expr_destruct(&(con->init));
}

/* initialize a sem_exprpart_t as a variable reference */
void sem_exprpart_init_var(sem_exprpart_t *exprp, int var) {
  exprp->type=SEM_EXPRPART_TYPE_VAR;
  exprp->data.var=var;
}

/* initialize a sem_expr_part_t as a literal string */
void sem_exprpart_init_lit(sem_exprpart_t *exprp, char *str) {
  exprp->type=SEM_EXPRPART_TYPE_LIT;
  exprp->data.lit=str;
}

/* initialize a sem_expr_part_t as a literal string */
void sem_exprpart_init_fun(sem_exprpart_t *exprp, char *str) {
  exprp->type=SEM_EXPRPART_TYPE_FUN;
  exprp->data.fun.name=str;
  alist_init(exprp->data.fun.args);
}

/* destruct a sem_exprpart_t */
void static sem_exprpart_destruct(sem_exprpart_t *exprp) {
  switch (exprp->type) {
    case SEM_EXPRPART_TYPE_LIT: {
      free(exprp->data.lit);
      break;
    }
    case SEM_EXPRPART_TYPE_VAR: {
      break;
    }
    case SEM_EXPRPART_TYPE_FUN: {
    	free(exprp->data.fun.name);
    	for (int i=0; i<alist_len(exprp->data.fun.args); i++) {
    		sem_expr_destruct(alist_ptr(exprp->data.fun.args,i));
    	}
    	alist_free(exprp->data.fun.args);
    	break;
    }
  }
}

/* initialize a sem_expr_t */
void sem_expr_init(sem_expr_t *expr) {
  alist_init(expr->parts);
}

/* destruct a sem_expr_t */
void sem_expr_destruct(sem_expr_t *expr) {
  for (int i=0; i<alist_len(expr->parts); i++) sem_exprpart_destruct(alist_ptr(expr->parts,i));
  alist_free(expr->parts);
}

/* initialize a sem_macro_t */
void sem_macro_init(sem_macro_t *macro) {
  macro->name=NULL;
  alist_init(macro->types);
  sem_expr_init(&(macro->def));
}

/* destuct a sem_macro_t */
void static sem_macro_destruct(sem_macro_t *macro) {
  free(macro->name);
  alist_free(macro->types);
  sem_expr_destruct(&(macro->def));
}

/* initialize a sem_conocc_t */
void static sem_conocc_init(sem_conocc_t *occ,int constr) {
  alist_init(occ->args);
  occ->constr=constr;
  occ->occn=NULL;
  occ->passive=0;
}

/* destruct a sem_conocc_t */
void static sem_conocc_destruct(sem_conocc_t *con) {
  alist_free(con->args);
  if (con->occn) free(con->occn);
}

/* initialize a sem_conocc_t */
void static sem_conoccout_init(sem_conoccout_t *occ, int constr) {
  alist_init(occ->args);
  occ->constr=constr;
}

/* destruct a sem_conocc_t */
void static sem_conoccout_destruct(sem_conoccout_t *con) {
  for (int i=0; i<alist_len(con->args); i++) {
    sem_expr_destruct(&(alist_get(con->args,i)));
  }
  alist_free(con->args);
}

void static sem_out_init_con(sem_out_t *out, sem_conoccout_t *con) {
	out->type=SEM_OUT_TYPE_CON;
	out->data.con=*con;
	alist_init(out->cdeps.co);
}

void static sem_out_init_expr(sem_out_t *out, sem_expr_t *exp,int stmt) {
	out->type=(stmt ? SEM_OUT_TYPE_STM : SEM_OUT_TYPE_EXP);
	out->data.exp=*exp;
	alist_init(out->cdeps.co);
}

void static sem_out_init_var(sem_out_t *out, int var) {
	out->type=SEM_OUT_TYPE_VAR;
	out->data.var=var;
	alist_init(out->cdeps.co);
}

void static sem_out_destruct(sem_out_t *out) {
	switch (out->type) {
		case SEM_OUT_TYPE_CON:
			sem_conoccout_destruct(&(out->data.con));
			break;
		case SEM_OUT_TYPE_STM:
		case SEM_OUT_TYPE_EXP:
			sem_expr_destruct(&(out->data.exp));
			break;
		case SEM_OUT_TYPE_VAR:
			break;
	}
	alist_free(out->cdeps.co);
}

/* initialize a sem_var_t */
void static sem_var_init(sem_var_t *var, char *name, int type, int anon) {
  var->name=name;
  var->type=type;
  var->local=0;
  var->pos=-1;
  var->poss=-1;
  var->anon=anon;
  var->occ[SEM_RULE_LEVEL_KEPT]=0;
  var->occ[SEM_RULE_LEVEL_REM]=0;
  var->occ[SEM_RULE_LEVEL_GUARD]=0;
  var->occ[SEM_RULE_LEVEL_BODY]=0;
  alist_init(var->cdeps.co);
  sem_expr_init(&(var->def));
}

/* initialize a variable as local */
void static sem_var_init_local(sem_var_t *var, char *name, int type, int body) {
	sem_var_init(var,name,type,0);
	var->local=body+1;
	sem_expr_init(&(var->def));
}

/* destruct a sem_var_t */
void static sem_var_destruct(sem_var_t *var) {
  free(var->name);
  alist_free(var->cdeps.co);
  if (var->local) sem_expr_destruct(&(var->def));
}

/* initialize a variable table */
void static sem_vartable_init(sem_vartable_t *vt) {
  alist_init(vt->vars);
}

void sem_expr_copy(sem_expr_t *src,sem_expr_t *dst,int start,int stop) {
  alist_init(dst->parts);
  alist_ensure(dst->parts,(stop<0 ? alist_len(src->parts) : stop)-start);
  for (int i=start; i<(stop<0 ? alist_len(src->parts) : stop); i++) {
    sem_exprpart_t np={.type=0};
    sem_exprpart_t *op=alist_ptr(src->parts,i);
    switch (op->type) {
      case SEM_EXPRPART_TYPE_LIT: {
        sem_exprpart_init_lit(&np,copy_string(op->data.lit));
	break;
      }
      case SEM_EXPRPART_TYPE_VAR: {
        sem_exprpart_init_var(&np,op->data.var);
	break;
      }
      case SEM_EXPRPART_TYPE_FUN: {
        sem_exprpart_init_fun(&np,copy_string(op->data.fun.name));
        for (int k=0; k<alist_len(op->data.fun.args); k++) {
	  sem_expr_t nex;
	  sem_expr_copy(alist_ptr(op->data.fun.args,k),&nex,0,-1);
	  alist_add(np.data.fun.args,nex);
	}
	break;
      }
    }
    alist_add(dst->parts,np);
  }
}

/* initialize a vartable to have only $%i vars */
void static sem_vartable_init_args(sem_vartable_t *vt, int from, int to) {
	sem_vartable_init(vt);
	for (int i=0; i<to-from; i++) {
		char c[16];
		snprintf(c,16,"$%i",i+from);
		sem_var_t var;
		sem_var_init(&var,copy_string(c),-1,0);
		alist_add(vt->vars,var);
	}
}

void static sem_vartable_init_constr(sem_vartable_t *vt, sem_constr_t *con) {
	sem_vartable_init_args(vt,0,alist_len(con->types)+1);
	for (int i=0; i<alist_len(con->types); i++) {
		alist_get(vt->vars,i+1).type=alist_get(con->types,i);
	}
}


/* destruct a sem_vartable_t */
void static sem_vartable_destruct(sem_vartable_t *vr) {
  for (int i=0; i<alist_len(vr->vars); i++) sem_var_destruct(alist_ptr(vr->vars,i));
  alist_free(vr->vars);
}

/* initialize a sem_rule_t */
void static sem_rule_init(sem_rule_t *rule, char *name) {
  rule->name=name;
  sem_vartable_init(&(rule->vt));
  alist_init(rule->head[SEM_RULE_LEVEL_KEPT]);
  alist_init(rule->head[SEM_RULE_LEVEL_REM]);
  alist_init(rule->out[0]);
  alist_init(rule->out[1]);
  rule->hook=-1;
}

/* destruct a sem_rule_t */
void static sem_rule_destruct(sem_rule_t *rule) {
  free(rule->name);
  sem_vartable_destruct(&(rule->vt));
  for (int i=0; i<alist_len(rule->head[SEM_RULE_LEVEL_KEPT]); i++) sem_conocc_destruct(alist_ptr(rule->head[SEM_RULE_LEVEL_KEPT],i));
  alist_free(rule->head[SEM_RULE_LEVEL_KEPT]);
  for (int i=0; i<alist_len(rule->head[SEM_RULE_LEVEL_REM]); i++) sem_conocc_destruct(alist_ptr(rule->head[SEM_RULE_LEVEL_REM],i));
  alist_free(rule->head[SEM_RULE_LEVEL_REM]);
  for (int i=0; i<alist_len(rule->out[0]); i++) sem_out_destruct(alist_ptr(rule->out[0],i));
  alist_free(rule->out[0]);
  for (int i=0; i<alist_len(rule->out[1]); i++) sem_out_destruct(alist_ptr(rule->out[1],i));
  alist_free(rule->out[1]);
}

/* initialize a sem_cchr_t */
void sem_cchr_init(sem_cchr_t *cchr) {
  alist_init(cchr->types);
  alist_init(cchr->rules);
  alist_init(cchr->macros);
  alist_init(cchr->cons);
  alist_init(cchr->exts);
}

/* destruct a sem_cchr_t */
void sem_cchr_destruct(sem_cchr_t *cchr) {
  for (int i=0; i<alist_len(cchr->types); i++) sem_vartype_destruct(alist_ptr(cchr->types,i));
  alist_free(cchr->types);
  for (int i=0; i<alist_len(cchr->rules); i++) sem_rule_destruct(alist_ptr(cchr->rules,i));
  alist_free(cchr->rules);
  for (int i=0; i<alist_len(cchr->cons); i++) sem_constr_destruct(alist_ptr(cchr->cons,i));
  alist_free(cchr->cons);
  for (int i=0; i<alist_len(cchr->macros); i++) sem_macro_destruct(alist_ptr(cchr->macros,i));
  alist_free(cchr->macros);
  for (int i=0; i<alist_len(cchr->exts); i++) free(alist_get(cchr->exts,i));
  alist_free(cchr->exts);
}

/* generate a new (unused) variable in a rule, and return its id */
int static sem_var_generate_anon(sem_vartable_t *vt) {
	int j=0;
	char test[32];
	do {
		snprintf(test,32,"_%i",j);
		int found=0;
		for (j=0; j<alist_len(vt->vars); j++) {
			if (!strcmp(alist_get(vt->vars,j).name,test)) {
				found=1;
				break;
			}
		}
		if (!found) break;
		j++;
	} while(1);
	sem_var_t var;
	sem_var_init(&var,copy_string(test),-1,1);
	int ret=alist_len(vt->vars);
	alist_add(vt->vars,var);
	return ret;
}

struct {
	char *name;
	int ar;
} sem_stmsym;

/* store<0: not, >=0: in that place in var->occ[] */
/* generate a sem_expr_t from a expr_t */
int static sem_expr_generate(sem_expr_t *expr,sem_vartable_t *vt,sem_cchr_t *cchr,expr_t *in,char *desc,int pos, int stmt) {
	int ok=1;
	for (int j=pos; j<alist_len(in->list); j++) {
		token_t *tok=alist_ptr(in->list,j);
		int pa=0; /* parse arguments */
		switch (tok->type) {
			case TOKEN_TYPE_LIT: {
				sem_exprpart_t se;
				sem_exprpart_init_lit(&se,copy_string(tok->data));
				alist_add(expr->parts,se);
				break;
			}
			case TOKEN_TYPE_FUNC: 
			 	pa=1; /* a function-call is actually a type of symbol */
			case TOKEN_TYPE_SYMB: if (tok->data) {
				sem_exprpart_t se;
				int ie=0;
				for (int i=0; i<alist_len(cchr->exts); i++) { /* an external */
					if (!strcmp(tok->data,alist_get(cchr->exts,i))) {
						sem_exprpart_init_lit(&se,copy_string(tok->data));
						alist_add(expr->parts,se);
						ie=1;
						break;
					}
				}
				if (ie) break;
				if (!stmt && vt && tok->data[0]=='_') { /* anonymous variable */
					int anvar=sem_var_generate_anon(vt);
					sem_exprpart_init_var(&se,anvar);
					alist_add(expr->parts,se);
					ie=1;
					break;
				}
				if (vt) { 
					for (int i=0; i<alist_len(vt->vars); i++) { /* look for existing var */
						if (!strcmp(tok->data,alist_get(vt->vars,i).name)) {
							sem_exprpart_init_var(&se,i);
							alist_add(expr->parts,se);
							ie=1;
							break;
						}
					}
				}
				if (ie) break;
				if (stmt || !isupper(tok->data[0])) { /* statement: unknown things translated literally */
					sem_exprpart_init_lit(&se,copy_string(tok->data));
					alist_add(expr->parts,se);
					ie=1;
					break;
				} else {
					if (vt) {
						int nvp=alist_len(vt->vars);
						sem_var_t nv;
						sem_var_init(&nv,copy_string(tok->data),-1,0);
						alist_add(vt->vars,nv);
						sem_exprpart_init_var(&se,nvp);
						alist_add(expr->parts,se);
						ie=1;
					}
				}
				break;
			}
		}
		if (pa && ok) {
			if (alist_len(expr->parts)>0) {
				sem_exprpart_t *lp=alist_ptr(expr->parts,alist_len(expr->parts)-1);
				if (lp->type==SEM_EXPRPART_TYPE_LIT) {
					sem_exprpart_t nw;
					sem_exprpart_init_fun(&nw,copy_string(lp->data.lit));
					for (int l=0; l<alist_len(tok->args); l++) {
						sem_expr_t se;
						sem_expr_init(&se);
						if (!sem_expr_generate(&se,vt,cchr,alist_ptr(tok->args,l),desc,0,stmt)) ok=0;
						alist_add(nw.data.fun.args,se);
					}
					sem_exprpart_destruct(lp);
					*lp=nw;
					pa=0;
				}
			}
		}
		if (pa && ok) {
			sem_exprpart_t se;
			sem_exprpart_init_lit(&se,copy_string("("));
			alist_add(expr->parts,se);
			for (int l=0; l<alist_len(tok->args); l++) {
				if (l) {
					sem_exprpart_init_lit(&se,copy_string(","));
					alist_add(expr->parts,se);
				}
				if (!sem_expr_generate(expr,vt,cchr,alist_ptr(tok->args,l),desc,0,stmt)) ok=0;
			}
			sem_exprpart_init_lit(&se,copy_string(")"));
			alist_add(expr->parts,se);
		}
	}
	if (!ok) sem_expr_destruct(expr);
	return ok;
}

int static sem_localstm_generate(sem_cchr_t *cchr,sem_rule_t *rule,expr_t *expr,int body) {
	if (alist_len(expr->list)<3) return 0;
	if (alist_get(expr->list,0).type != TOKEN_TYPE_LIT || strcmp(alist_get(expr->list,0).data,"{")) return 0;
	token_t b;
	alist_pop(expr->list,b);
	if (b.type != TOKEN_TYPE_LIT || strcmp(b.data,"}")) {alist_add(expr->list,b); return 0;}
	destruct_token_t(&b);
	sem_expr_t se;
	sem_expr_init(&se);
	if (!sem_expr_generate(&se,&(rule->vt),cchr,expr,rule->name,1,1)) return 0;
	sem_out_t out;
	sem_out_init_expr(&out,&se,1);
	alist_add(rule->out[body],out);
	return 1;
}

/* assign an expression to a vartable (update occurrence counts in variables) */
void static sem_expr_assign(sem_expr_t *expr,sem_vartable_t *vt, int type) {
	for (int j=0; j<alist_len(expr->parts); j++) {
		sem_exprpart_t *ep=alist_ptr(expr->parts,j);
		if (ep->type==SEM_EXPRPART_TYPE_VAR) {
			alist_get(vt->vars,ep->data.var).occ[type]++;
		}
		if (ep->type==SEM_EXPRPART_TYPE_FUN) {
			for (int l=0; l<alist_len(ep->data.fun.args); l++) {
				sem_expr_assign(alist_ptr(ep->data.fun.args,l),vt,type);
			}
		}
	}
}

/* assign a rule to its vartable (update occurrence counts in variables) */
void static sem_rule_assign(sem_cchr_t *cchr,sem_rule_t *rule) {
	for (int var=0; var<alist_len(rule->vt.vars); var++) {
		alist_get(rule->vt.vars,var).occ[SEM_RULE_LEVEL_KEPT]=0;
		alist_get(rule->vt.vars,var).occ[SEM_RULE_LEVEL_REM]=0;
		alist_get(rule->vt.vars,var).occ[SEM_RULE_LEVEL_GUARD]=0;
		alist_get(rule->vt.vars,var).occ[SEM_RULE_LEVEL_BODY]=0;
	}
	for (int type=0; type<2; type++) {
		for (int j=0; j<alist_len(rule->head[type]); j++) {
			sem_conocc_t *co=alist_ptr(rule->head[type],j);
			for (int k=0; k<alist_len(co->args); k++) {
				sem_var_t *var=alist_ptr(rule->vt.vars,alist_get(co->args,k));
				var->occ[type]++;
				if (var->pos<0) {
					var->pos=j;
					var->poss=k;
				}
				if (var->type == -1) {
					var->type=alist_get(alist_get(cchr->cons,co->constr).types,k);
				}
			}
		}
	}
	for (int body=0; body<2; body++) {
		for (int j=0; j<alist_len(rule->out[body]); j++) {
			sem_out_t *co=alist_ptr(rule->out[body],j);
			switch (co->type) {
				case SEM_OUT_TYPE_CON: {
					for (int k=0; k<alist_len(co->data.con.args); k++) {
						sem_expr_assign(alist_ptr(co->data.con.args,k),&(rule->vt),body ? SEM_RULE_LEVEL_BODY : SEM_RULE_LEVEL_GUARD);
					}
					break;
				}
				case SEM_OUT_TYPE_EXP:{
					sem_expr_assign(&(co->data.exp),&(rule->vt),body ? SEM_RULE_LEVEL_BODY : SEM_RULE_LEVEL_GUARD);
					break;
				}
				case SEM_OUT_TYPE_STM: {
					sem_expr_assign(&(co->data.exp),&(rule->vt),body ? SEM_RULE_LEVEL_BODY : SEM_RULE_LEVEL_GUARD);
					break;
				}
				case SEM_OUT_TYPE_VAR: {
					sem_var_t *var=alist_ptr(rule->vt.vars,co->data.var);
					var->occ[body ? SEM_RULE_LEVEL_BODY : SEM_RULE_LEVEL_GUARD]++;
					sem_expr_assign(&(var->def),&(rule->vt),body ? SEM_RULE_LEVEL_BODY : SEM_RULE_LEVEL_GUARD);
					break;
				}
			}
		}
	}
}

int static sem_localvar_generate(sem_cchr_t *cchr,sem_rule_t *rule,expr_t *expr, int body) {
	if (alist_len(expr->list)<3) return 0;
	int j=0;
	int pl=0;
	while (j<alist_len(expr->list)) {
		token_t *ep=alist_ptr(expr->list,j);
		if (ep->type==TOKEN_TYPE_LIT && !strcmp(ep->data,"=")) {
			if (j<2) return 0;
			token_t *pep=alist_ptr(expr->list,j-1);
			if (pep->type==TOKEN_TYPE_SYMB) {
				char *type=malloc(pl+1);
				type[0]=0;
				for (int t=0; t<j-1; t++) {
					if (t) strcat(type," ");
					strcat(type,alist_get(expr->list,t).data);
				}
				sem_var_t *var;
				alist_new(rule->vt.vars,var);
				int type_id=sem_cchr_gettype(cchr,type);
				free(type);
				sem_var_init_local(var,copy_string(pep->data),type_id,body);
				sem_out_t out;
				sem_out_init_var(&out,alist_len(rule->vt.vars)-1);
				alist_add(rule->out[body],out);
				if (j<alist_len(expr->list)-1) {
					if (!sem_expr_generate(&(var->def),&(rule->vt),cchr,expr,rule->name,j+1,1)) return 0;
				}
				return 1;
			}
			return 0;
		}
		if (ep->type==TOKEN_TYPE_FUNC) return 0;
		pl+=strlen(ep->data)+1;
		j++;
	}
	return 0;
}

/* generate a conocc arg (for in kept or removed constraints) given its sem_expr_t as argument */
int static sem_conocc_arg_generate(sem_rule_t *rule,sem_cchr_t *cchr,sem_expr_t *expr) {
	if (alist_len(expr->parts)==1 && alist_get(expr->parts,0).type==SEM_EXPRPART_TYPE_VAR) {
		int v=alist_get(expr->parts,0).data.var;
		sem_expr_destruct(expr);
		return v;
	}
	int var=sem_var_generate_anon(&(rule->vt));
	sem_expr_t nex;
	sem_expr_init(&nex);
	sem_exprpart_t nexp;
	sem_exprpart_init_var(&nexp,var);
	alist_add(nex.parts,nexp);
	sem_exprpart_init_lit(&nexp,copy_string("=="));
	alist_add(nex.parts,nexp);
	sem_exprpart_init_lit(&nexp,copy_string("("));
	alist_add(nex.parts,nexp);
	alist_addall(nex.parts,expr->parts);
	alist_free(expr->parts);
	sem_exprpart_init_lit(&nexp,copy_string(")"));
	alist_add(nex.parts,nexp);
	sem_out_t out;
	sem_out_init_expr(&out,&nex,0);
	alist_add(rule->out[0],out);
	return var;
}

/* return 0 if no constraint occurrence can be derived from the expression 'in' */
/* generate a conocc (given its functional form in a expr_t) */
/* type cannot be SEM_RULE_LEVEL_GUARD (no conocc's there) */
int static sem_conocc_generate(sem_rule_t *rule,sem_cchr_t *cchr,expr_t *in,int type) {
	int ok=1;
	if (alist_len(in->list) != 1) return 0;
	token_t *tok=alist_ptr(in->list,0);
	if (tok->type != TOKEN_TYPE_FUNC) return 0;
	for (int j=0; j<alist_len(cchr->cons); j++) {
		sem_constr_t *cons=alist_ptr(cchr->cons,j);
		if ((alist_len(tok->args) == alist_len(cons->types)) && !strcmp(tok->data,cons->name)) {
			sem_conocc_t n1;
			sem_conocc_init(&n1,j); /* no warning about uninitialized variable */
			sem_conoccout_t n2;
			sem_conoccout_init(&n2,j); /* same here */
			sem_ruleocc_t r;
			r.rule=alist_len(cchr->rules);
			r.type=type;
			if (type==SEM_RULE_LEVEL_BODY) {
				r.pos=alist_len(rule->out[1]);
			} else {
				r.pos=alist_len(rule->head[type]);
				if (in->occn) {
				    if (!strcmp(in->occn,"passive")) {
					n1.passive=1;
					n1.occn=NULL;
				    } else {
					n1.occn=copy_string(in->occn);
				    }
				} else {
				    n1.occn=NULL;
				}
			}
			alist_add(cons->occ,r);
			for (int s=0; s<alist_len(tok->args); s++) {
				expr_t *expr=alist_ptr(tok->args,s);
				sem_expr_t se;
				sem_expr_init(&se);
				if (!sem_expr_generate(&se,&(rule->vt),cchr,expr,rule->name,0,type==SEM_RULE_LEVEL_BODY)) ok=0;
				if (type==SEM_RULE_LEVEL_BODY) {
					alist_add(n2.args,se);
				} else {
				  	int var=sem_conocc_arg_generate(rule,cchr,&se);
				  	alist_add(n1.args,var);
				}
			}
			if (type==SEM_RULE_LEVEL_BODY) {
				sem_out_t out;
				sem_out_init_con(&out,&n2);
				alist_add(rule->out[1],out);
			} else {
				alist_add(rule->head[type],n1);
			}
			return ok;
		}
	}
	return 0;
}

/* complete the HNF form (splitting duplicate variables) */
/* requires assign to have completed */
int static sem_rule_hnf(sem_cchr_t *cchr,sem_rule_t *rule) {
	for (int j=0; j<alist_len(rule->vt.vars); j++) {
		sem_var_t *var=alist_ptr(rule->vt.vars,j);
		int bnd=var->occ[SEM_RULE_LEVEL_KEPT]+var->occ[SEM_RULE_LEVEL_REM];
		int occ=bnd+var->occ[SEM_RULE_LEVEL_GUARD]+var->occ[SEM_RULE_LEVEL_BODY];
		if (bnd==0 && !var->local) {
			fprintf(stderr,"error: in rule '%s': variable '%s' is unbound\n",rule->name ? rule->name : "(unk)",var->name);
			return 0;
		}
		if (occ==1 && !var->anon) {
			fprintf(stderr,"warning: in rule '%s': variable '%s' is singleton\n",rule->name ? rule->name : "(unk)",var->name);
		} 
		if (bnd>1) { // split variable
			int oc=0;
			for (int p=SEM_RULE_LEVEL_KEPT; p<=SEM_RULE_LEVEL_REM; p++) { // KEPT & REM
				for (int k=0; k<alist_len(rule->head[p]); k++) { // all conocc's
					sem_conocc_t *cot=alist_ptr(rule->head[p],k);
					for (int l=0; l<alist_len(cot->args); l++) { // all args of conocc
						int kv=alist_get(cot->args,l);
						if (kv==j) {
							if (oc) {
								int nv=sem_var_generate_anon(&(rule->vt));
								alist_get(cot->args,l)=nv;
								sem_expr_t ne;
								sem_expr_init(&ne);
								sem_exprpart_t nep;
                                                                char *fn=alist_ptr(cchr->types,var->type)->equality;
								if (fn==NULL) fn="eq";
								sem_exprpart_init_fun(&nep,copy_string(fn));
								sem_expr_t ne2;
								sem_exprpart_t nep2;
								sem_expr_init(&ne2);
								sem_exprpart_init_var(&nep2,nv);
								alist_add(ne2.parts,nep2);
								alist_add(nep.data.fun.args,ne2);
								sem_expr_init(&ne2);
								sem_exprpart_init_var(&nep2,j);
								alist_add(ne2.parts,nep2);
								alist_add(nep.data.fun.args,ne2);
								alist_add(ne.parts,nep);
                                                                sem_out_t out;
                                                                sem_out_init_expr(&out,&ne,0);
								alist_add(rule->out[0],out);
								alist_get(rule->vt.vars,nv).occ[p]++; /* replacement variable instead of double one */
								alist_get(rule->vt.vars,j).occ[p]--;
								alist_get(rule->vt.vars,j).occ[SEM_RULE_LEVEL_GUARD]++;
								alist_get(rule->vt.vars,nv).occ[SEM_RULE_LEVEL_GUARD]++; /* replacement also occurs in guard */
								alist_get(rule->vt.vars,nv).pos=k; /* where replacement var is definied */
								alist_get(rule->vt.vars,nv).poss=l; /* idem */
								alist_get(rule->vt.vars,nv).type=alist_get(rule->vt.vars,kv).type; /* new var is same type as (replaced) double var */
							}
							oc=1;
						}
					}
				}
			}
		}
	}
	return 1;
}

int sem_expr_fill_deps(sem_cchr_t *out,sem_rule_t *rule,sem_cdeps_t *cdeps,sem_expr_t *expr) {
  int ret=0;
  for (int k=0; k<alist_len(expr->parts); k++) {
    sem_exprpart_t *sep=alist_ptr(expr->parts,k);
    switch (sep->type) {
      case SEM_EXPRPART_TYPE_LIT:
        break;
      case SEM_EXPRPART_TYPE_VAR: {
        sem_var_t *vt=alist_ptr(rule->vt.vars,sep->data.var);
        for (int l=0; l<alist_len(vt->cdeps.co); l++) {
	  int vv=alist_get(vt->cdeps.co,l);
	  int fnd=0;
	  for (int s=0; s<alist_len(cdeps->co); s++) {
	    if (alist_get(cdeps->co,s)==vv) {fnd=1;break;}
	  }
	  if (!fnd) {alist_add(cdeps->co,vv);ret=1;}
	}
	break;
      }
      case SEM_EXPRPART_TYPE_FUN: {
        for (int p=0; p<alist_len(sep->data.fun.args); p++) {
	  ret+=sem_expr_fill_deps(out,rule,cdeps,alist_ptr(sep->data.fun.args,p));
	}
	break;
      }
    }
  }
  return ret;
}

void sem_rule_fill_deps(sem_cchr_t *out,sem_rule_t *rule) {
  for (int p=SEM_RULE_LEVEL_KEPT; p<=SEM_RULE_LEVEL_REM; p++) {
    for (int k=0; k<alist_len(rule->head[p]); k++) {
      sem_conocc_t *cot=alist_ptr(rule->head[p],k);
      for (int j=0; j<alist_len(cot->args); j++) {
        int vv=alist_get(cot->args,j);
	sem_var_t *var=alist_ptr(rule->vt.vars,vv);
	int fnd=0;
	for (int s=0; s<alist_len(var->cdeps.co); s++) {
	  if (alist_get(var->cdeps.co,s)==k+(p<<31)) {fnd=1; break;}
	}
	if (!fnd) alist_add(var->cdeps.co,k+(p<<31));
      }
    }
  }
  do {
    int cont=0;
    for (int j=0; j<alist_len(rule->vt.vars); j++) {
      sem_var_t *var=alist_ptr(rule->vt.vars,j);
      if (var->local) {
        cont+=sem_expr_fill_deps(out,rule,&(var->cdeps),&(var->def));
      }
    }
    if (!cont) break;
  } while(1);
  for (int f=0; f<alist_len(rule->out[0]); f++) {
    sem_out_t *ott=alist_ptr(rule->out[0],f);
    if (ott->type==SEM_OUT_TYPE_STM || ott->type==SEM_OUT_TYPE_EXP) {
      sem_expr_fill_deps(out,rule,&(ott->cdeps),&(ott->data.exp));
      /* TODO: set cdeps of all referred variables to ott->cdeps */
    }
    if (ott->type==SEM_OUT_TYPE_VAR) {
      sem_var_t *rv=alist_ptr(rule->vt.vars,ott->data.var);
      alist_addall(ott->cdeps.co,rv->cdeps.co);
    }
  }
}

void static sem_rule_hook(sem_cchr_t *out,int rulenum) {
	sem_rule_t *rule=alist_ptr(out->rules,rulenum);
	if (rule->hook<0 && alist_len(rule->head[SEM_RULE_LEVEL_REM])==0) {
		int bestpos=-1; /* what conocc in KEPT is best (-1: none yet) */
		int bestcon=0; /* what constraint is best */
		int bestnum=0; /* how many hooks that contraint already has */
		for (int j=0; j<alist_len(rule->head[SEM_RULE_LEVEL_KEPT]); j++) {
			sem_conocc_t *co=alist_ptr(rule->head[SEM_RULE_LEVEL_KEPT],j);
			int num=alist_len(alist_get(out->cons,co->constr).hooked);
			if (bestpos==-1 || num<bestnum) {
				bestpos=j;
				bestcon=co->constr;
				bestnum=num;
			}
		}
		alist_add(alist_get(out->cons,bestcon).hooked,rulenum);
		rule->hook=bestpos;
	}
}

void static sem_rule_related(sem_cchr_t *out,sem_rule_t *rule) {
	for (int i=0; i<alist_len(rule->head[SEM_RULE_LEVEL_REM])+alist_len(rule->head[SEM_RULE_LEVEL_KEPT]); i++) {
		int ik=(i>=alist_len(rule->head[SEM_RULE_LEVEL_REM]));
		int in=i-ik*alist_len(rule->head[SEM_RULE_LEVEL_REM]);
		sem_constr_t *cons=alist_ptr(out->cons,alist_get(rule->head[ik ? SEM_RULE_LEVEL_KEPT : SEM_RULE_LEVEL_REM],in).constr);
		for (int j=0; j<alist_len(rule->head[SEM_RULE_LEVEL_REM])+alist_len(rule->head[SEM_RULE_LEVEL_KEPT]); j++) {
			if (i!=j) {
				int jk=(j>=alist_len(rule->head[SEM_RULE_LEVEL_REM]));
				int jn=j-jk*alist_len(rule->head[SEM_RULE_LEVEL_REM]);
				int conid=alist_get(rule->head[jk ? SEM_RULE_LEVEL_KEPT : SEM_RULE_LEVEL_REM],jn).constr;
				int k=0;
				while (k<alist_len(cons->related)) {
					if (alist_get(cons->related,k) == conid) break;
					k++;
				}
				if (k==alist_len(cons->related)) {
					alist_add(cons->related,conid);
				}
			}
		}
	}
}

int static sem_expr_gettype(sem_cchr_t *chr,sem_vartable_t *vt,sem_expr_t *expr) {
	if (alist_len(expr->parts)!=1) return -1;
	sem_exprpart_t *sep=alist_ptr(expr->parts,0);
	if (sep->type==SEM_EXPRPART_TYPE_VAR) {
		sem_var_t *var=alist_ptr(vt->vars,sep->data.var);
		if (var->type>=0) return var->type;
	}
	return -1;
}

void static sem_expr_expand(sem_cchr_t *chr,sem_vartable_t *vt,sem_expr_t *in,sem_expr_t *out,sem_fun_t *fun) {
	int replace=0;
	sem_expr_t outn;
	sem_expr_init(&outn);
	if (out==NULL) {
		out=&outn;
		replace=1;
	}
	for (int i=0; i<alist_len(in->parts); i++) {
		sem_exprpart_t *se=alist_ptr(in->parts,i);
		switch (se->type) {
			case SEM_EXPRPART_TYPE_FUN: {
	    		int tr=0;
	    		if (chr) { for (int k=0; k<alist_len(chr->macros); k++) {
	    			sem_macro_t *mac=alist_ptr(chr->macros,k);
	    			if (alist_len(se->data.fun.args) != alist_len(mac->types)) continue;
	    			if (strcmp(mac->name,se->data.fun.name)) continue;
	    			int l=0;
	    			while (l<alist_len(mac->types)) {
	    				int type=vt ? sem_expr_gettype(chr,vt,alist_ptr(se->data.fun.args,l)) : -1;
	    				if (alist_get(mac->types,l)>=0 && (type<0 || alist_get(mac->types,l)!=type)) {
	    					/*fprintf(stderr,"[expand of %s: arg %i of type %i (macro needs %i)]\n",mac->name,l+1,type,alist_get(mac->types,l));*/
	    					break;
	    				}
	    				l++;
	    			}
	    			if (l==alist_len(mac->types)) { /* found match! do replacement! */
					if (fun) {
						sem_fun_t prep;
						alist_init(prep.args);
						prep.name=se->data.fun.name;
						for (int i=0; i<alist_len(se->data.fun.args); i++) {
							sem_expr_t bla;
							sem_expr_init(&bla);
							sem_expr_expand(NULL,vt,alist_ptr(se->data.fun.args,i),&bla,fun);
							alist_add(prep.args,bla);
						}
						sem_expr_expand(chr,vt,&(mac->def),out,&prep);
						for (int i=0; i<alist_len(prep.args); i++) {
							sem_expr_destruct(alist_ptr(prep.args,i));
						}
					} else {
						sem_expr_expand(chr,vt,&(mac->def),out,&(se->data.fun));
					}
	    				tr=1;
	    				break;
	    			}
	    		} }
	    		if (!tr) { /* no matches found */
	    			sem_exprpart_t ne;
	    			sem_exprpart_init_fun(&ne,copy_string(se->data.fun.name));
	    			for (int k=0; k<alist_len(se->data.fun.args); k++) {
					sem_expr_t kwak;
					sem_expr_init(&kwak);
	    				sem_expr_expand(chr,vt,alist_ptr(se->data.fun.args,k),&kwak,fun);
					alist_add(ne.data.fun.args,kwak);
	    			}
	    			alist_add(out->parts,ne);
	    		}
	    		break;
			}
			case SEM_EXPRPART_TYPE_LIT: {
				sem_exprpart_t ne;
				sem_exprpart_init_lit(&ne,copy_string(se->data.lit));
				alist_add(out->parts,ne);
				break;
			}
			case SEM_EXPRPART_TYPE_VAR: {
				if (fun) {
					sem_expr_expand(chr,vt,alist_ptr(fun->args,se->data.var),out,NULL);
				} else {
					sem_exprpart_t ne;
					sem_exprpart_init_var(&ne,se->data.var);
					alist_add(out->parts,ne);
				}
				break;
			}
	    }
	}
	if (replace) {
		sem_expr_destruct(in);
		*in=*out;
	}
}

void static sem_rule_expand(sem_cchr_t *out,sem_rule_t *rule) {
	for (int j=0; j<2; j++) {
		for (int l=0; l<alist_len(rule->out[j]); l++) {
			sem_out_t *coo=alist_ptr(rule->out[j],l);
			switch (coo->type) {
				case SEM_OUT_TYPE_CON: {
					for (int k=0; k<alist_len(coo->data.con.args); k++) {
						sem_expr_expand(out,&(rule->vt),alist_ptr(coo->data.con.args,k),NULL,NULL);
					}
					break;
				}
				case SEM_OUT_TYPE_EXP:
				case SEM_OUT_TYPE_STM: { 
					sem_expr_expand(out,&(rule->vt),&(coo->data.exp),NULL,NULL);
					break;
				}
				case SEM_OUT_TYPE_VAR: {
					sem_var_t *var=alist_ptr(rule->vt.vars,coo->data.var);
					sem_expr_expand(out,&(rule->vt),&(var->def),NULL,NULL);
					break;
				}
			}
		}
	}
}

/* generate a new rule in an output sem_cchr_t */
int static sem_rule_generate(sem_cchr_t *out,rule_t *in) {
	sem_rule_t n;
	sem_rule_init(&n,copy_string(in->name));
	int doret=1;
	for (int i=0; i<alist_len(in->removed.list); i++) {
		if (!sem_conocc_generate(&n,out,alist_ptr(in->removed.list,i),SEM_RULE_LEVEL_REM)) doret=0;
	}
	for (int i=0; i<alist_len(in->kept.list); i++) {
		if (!sem_conocc_generate(&n,out,alist_ptr(in->kept.list,i),SEM_RULE_LEVEL_KEPT)) doret=0;
	}
	for (int i=0; i<alist_len(in->guard.list); i++) {
		int ok=0;
		if (!ok) ok=sem_localvar_generate(out,&n,alist_ptr(in->guard.list,i),0);
		if (!ok) ok=sem_localstm_generate(out,&n,alist_ptr(in->guard.list,i),0);
		if (!ok) {
			sem_expr_t expr;
			sem_expr_init(&expr);
			if (!sem_expr_generate(&expr,&(n.vt),out,alist_ptr(in->guard.list,i),n.name,0,0)) doret=0;
			sem_out_t ot;
			sem_out_init_expr(&ot,&expr,0);
			alist_add(n.out[0],ot);
		}
	}
	for (int i=0; i<alist_len(in->body.list); i++) {
		int ok=0;
		if (!ok) ok=sem_conocc_generate(&n,out,alist_ptr(in->body.list,i),SEM_RULE_LEVEL_BODY);
		if (!ok) ok=sem_localvar_generate(out,&n,alist_ptr(in->body.list,i),1);
		if (!ok) ok=sem_localstm_generate(out,&n,alist_ptr(in->body.list,i),1);
		if (!ok) {fprintf(stderr,"In rule %s: unable to parse body part %i\n",n.name ? n.name : "<anonymous>",i+1);doret=0;}
	}
	sem_rule_assign(out,&n);
	sem_rule_expand(out,&n);
	sem_rule_assign(out,&n);
	if (doret && sem_rule_hnf(out,&n)) {
		sem_rule_related(out,&n);
		alist_add(out->rules,n);
		sem_rule_fill_deps(out,&n);
		sem_rule_hook(out,alist_len(out->rules)-1);
		return 1;
	} else {
		fprintf(stderr,"Rule '%s' failed\n",n.name ? n.name : "<anonymous>");
		sem_rule_destruct(&n);
		return 0;
	}
}

/* generate a new constraint in an output sem_cchr_t */
void static sem_cons_generate(sem_cchr_t *out,constr_t *in) {
  sem_constr_t *n;
  alist_new(out->cons,n);
  sem_constr_init(n,copy_string(in->name));
  for (int i=0; i<alist_len(in->list); i++) {
  	alist_add(n->types,sem_cchr_gettype(out,alist_get(in->list,i)));
  }
  for (int i=0; i<alist_len(in->args); i++) {
  	expr_t *e=alist_ptr(in->args,i);
  	token_t *t=alist_ptr(e->list,0);
	sem_vartable_t svt;
	sem_vartable_init_constr(&svt,n);
  	int ok=0;
  	if (t->type==TOKEN_TYPE_FUNC && !strcmp(t->data,"option")) {
		char *optname=NULL;
		if (alist_len(t->args)>1 && alist_len(alist_get(t->args,0).list)==1 && alist_get(alist_get(t->args,0).list,0).type==SEM_EXPRPART_TYPE_LIT) {
		  optname=alist_get(alist_get(t->args,0).list,0).data;
		} else {
			fprintf(stderr,"warning: name of option should be literal\n");
			ok=1;
		}
  		if (!ok && !strcmp(optname,"fmt")) {
			for (int l=1; l<alist_len(t->args); l++) {
			    sem_expr_t fa;
			    sem_expr_init(&fa);
  			    sem_expr_generate(&(fa),&svt,out,alist_ptr(t->args,l),n->name,0,1);
  			    sem_expr_expand(out,&svt,&(fa),NULL,NULL);
			    alist_add(n->fmt,fa);
			}
  			ok=1;
  		}
		int par=0;
		if (!strcmp(optname,"destr")) par=1;
		if (!strcmp(optname,"init")) par=2;
		if (!strcmp(optname,"kill")) par=3;
		if (!strcmp(optname,"add")) par=4;
  		if (!ok && par) {
			sem_expr_t res;
  			sem_expr_init(&res);
  			sem_expr_generate(&res,&svt,out,alist_ptr(t->args,1),n->name,0,1);
  			sem_expr_expand(out,&svt,&res,NULL,NULL);
  			ok=1;  			
			switch (par) {
			  case 1: {alist_addall(n->destr.parts,res.parts); break; }
			  case 2: {alist_addall(n->init.parts,res.parts); break; }
			  case 3: {alist_addall(n->kill.parts,res.parts); break; }
			  case 4: {alist_addall(n->add.parts,res.parts); break; }
			}
			alist_free(res.parts);
  		}
  		if (!ok) {
  			fprintf(stderr,"warning: unknown constraint option '%s' ignored\n",optname);
			ok=1;
  		}
  	}
	if (!ok) {
	  fprintf(stderr,"warning: ignoring '%s' after constraint definition\n",t->data);
	}
	sem_vartable_destruct(&svt);
  }
}

void static sem_macro_generate(sem_cchr_t *out,macro_t *in) {
  sem_macro_t n;
  sem_macro_init(&n);
  n.name=copy_string(in->name.name);
  for (int i=0; i<alist_len(in->name.list); i++) {
  	int type=(!strcmp(alist_get(in->name.list,i),"_")) ? -1 : sem_cchr_gettype(out,alist_get(in->name.list,i));
  	alist_add(n.types,type);
  }
  sem_vartable_t vt;
  sem_vartable_init_args(&vt,1,alist_len(in->name.list)+1);
  sem_expr_generate(&(n.def),&vt,out,&(in->def),n.name,0,1);
  sem_expr_expand(out,NULL,&(n.def),NULL,NULL);
  sem_vartable_destruct(&vt);
  alist_add(out->macros,n);
}

void static sem_logicals(sem_cchr_t *out,logical_t *log) {
  int vid=sem_cchr_gettype(out,log->name);
  sem_vartype_t *vt=alist_ptr(out->types,vid);
  vt->log_ground=sem_cchr_gettype(out,log->cb);
}

void static analysis_neverstored(sem_cchr_t *cchr,int con) {
  sem_constr_t *cons=alist_ptr(cchr->cons,con);
//  printf("- doing never stored for %s/%i\n",cons->name,alist_len(cons->types));
  for (int i=0; i<alist_len(cons->occ); i++) {
    sem_ruleocc_t *ro=alist_ptr(cons->occ,i);
    sem_rule_t *r=alist_ptr(cchr->rules,ro->rule);
//    printf("  - occ %i: rule %s, occ %i (%s)\n",i,r->name,ro->pos,ro->type == SEM_RULE_LEVEL_KEPT ? "kept" : "rem");
    if (ro->type == SEM_RULE_LEVEL_KEPT) return;
    if (ro->type == SEM_RULE_LEVEL_REM) {
      if (alist_len(r->out[0])==0) {
        /* found a never stored, make all partner-constraints passive */
//        printf("(never stored: rule %s, %s constraint %i)\n",r->name,ro->type==SEM_RULE_LEVEL_KEPT ? "kept" : "removed",ro->pos);
        for (int j=0; j<alist_len(cons->occ); j++) {
          sem_ruleocc_t *pro=alist_ptr(cons->occ,j);
          sem_rule_t *pr=alist_ptr(cchr->rules,pro->rule);
          if (pro->type == SEM_RULE_LEVEL_KEPT || pro->type == SEM_RULE_LEVEL_REM) {
            for (int h=0; h<2; h++) {
              for (int k=0; k<alist_len(pr->head[h]); k++) {
                sem_conocc_t *pco=alist_ptr(pr->head[h],k);
                if (pco->constr != con) {
                  pco->passive=1;
//                  printf("  (never stored: making passive in: rule %s, %s constraint %i)\n",pr->name,h==SEM_RULE_LEVEL_KEPT ? "kept" : "removed",k);
                }
              }
            }
          }
        }
        return;
      }
    }
  }
}

/* generate a semantic form (sem_cchr_t) of the syntax tree (cchr_t) */
int sem_generate_cchr(sem_cchr_t *out,cchr_t *in) {
  sem_cchr_init(out);
  sem_cchr_init_base_types(out);
  int ok=1;
  for (int i=0; i<alist_len(in->logicals); i++) {
	sem_logicals(out,alist_ptr(in->logicals,i));
  }
  sugar_log_pre_analyse(out);
  for (int i=0; i<alist_len(in->exts); i++) {
    alist_add(out->exts,copy_string(alist_get(in->exts,i)));
  }
  for (int i=0; i<alist_len(in->macros); i++) {
  	sem_macro_generate(out,alist_ptr(in->macros,i));
  }
  for (int i=0; i<alist_len(in->constrs); i++) {
  	sem_cons_generate(out,alist_ptr(in->constrs,i));
  }
  for (int i=0; i<alist_len(in->rules); i++) {
  	if (!sem_rule_generate(out,alist_ptr(in->rules,i))) ok=0;
  }
  /* analyses */
  sugar_log_post_analyse(out);
  for (int i=0; i<alist_len(out->cons); i++) {
  	analysis_neverstored(out,i);
  }
  return ok;
}

/****************************************************************************\
| CCHR - A CHR-in-C to C compiler                                            |
| semtree.h - Definition of semantic tree data structure                     |
| written by Pieter Wuille                                                   |
\****************************************************************************/ 

#ifndef _semtree_h_
#define _semtree_h_ 1

#include "alist.h"
#include <stdint.h>

/* 4 types of rule expressions */
typedef enum enum_sem_rule_level {
  SEM_RULE_LEVEL_KEPT=0,
  SEM_RULE_LEVEL_REM=1,
  SEM_RULE_LEVEL_GUARD=2,
  SEM_RULE_LEVEL_BODY=3
} sem_rule_level_t;

#define SEM_RULE_LEVEL_KEPT 0
#define SEM_RULE_LEVEL_REM 1
#define SEM_RULE_LEVEL_GUARD 2
#define SEM_RULE_LEVEL_BODY 3

/* a rule occurrence, listing in what rule and where a specific constraint occurs */
typedef struct {
	int rule; /* what rule? */
	sem_rule_level_t type; /* in which clause of that rule */
	int pos; /* position in that rule */
} sem_ruleocc_t;

typedef struct _sem_expr_t_struct sem_expr_t;

/* the types of expression parts */
typedef enum enum_sem_exprpart_type {
  SEM_EXPRPART_TYPE_VAR=1, /* a variable reference */
  SEM_EXPRPART_TYPE_LIT=2, /* a literal piece of code */
  SEM_EXPRPART_TYPE_FUN=3, /* a functional form (resolved after analysis) */
} sem_exprpart_type;

#define SEM_EXPRPART_TYPE_VAR 1
#define SEM_EXPRPART_TYPE_LIT 2
#define SEM_EXPRPART_TYPE_FUN 3

typedef struct {
    char *name;
    alist_declare(sem_expr_t,args);
} sem_fun_t;
	
/* a part of an expression, being either a var ref or a lit piece of code */
typedef struct {
  sem_exprpart_type type;
  union {
    int var; /* when type==SEM_EXPRPART_TYPE_VAR */
    char *lit; /* when type==SEM_EXPRPART_TYPE_LIT */
    sem_fun_t fun; /* when type==SEM_EXPRPART_TYPE_FUN */
  } data;
} sem_exprpart_t;

/* an expression being a list of expression parts */
struct _sem_expr_t_struct {
  alist_declare(sem_exprpart_t,parts);
};

/* an occurrence of a type */
typedef struct {
  int con; /* in which constraint */
  int arg; /* what argument of that constraint */
} sem_typeocc_t;

/* a (variable) type definition */
typedef struct {
  char *name;
  char *equality; /* implicit equality check function; eq(X,Y) is default */
  int log_ground; /* ground type */
  alist_declare(sem_typeocc_t,log_idx);
} sem_vartype_t;

/* a constraint, having a name, a list of types, and a list of rule occurences */
typedef struct {
  char *name;
  alist_declare(int,types);
  alist_declare(int,hooked);
  alist_declare(int,related); /* list of constr id's that are related to this one */
  alist_declare(sem_ruleocc_t,occ);
  sem_expr_t destr,init,add,kill; /* format specification, destructor, initializer */
  alist_declare(sem_expr_t,fmt);
  /*alist_declare(sem_expr_t,fmtargs);*/
} sem_constr_t;

/* a constraint occurrence, with a number referring to a constraint, and a list of arguments */
typedef struct {
  int constr;
  alist_declare(int,args); /* in KEPT & REM: just one var ID */
  char *occn;
  int passive;
} sem_conocc_t;

typedef struct {
  alist_declare(uint32_t,co);
} sem_cdeps_t;

/* a constraint occurrence in a body */
typedef struct {
  int constr;
  alist_declare(sem_expr_t,args);
} sem_conoccout_t;

/* a variable, with a name, a type (unused ftm), and occurence counts in both head constraints of rules */
typedef struct {
  char *name;
  int type;
  int occ[4]; /* occurrences in removed,kept,guard,body */
  int local; /* whether is variable is local in the body; 1=guard, 2=body */
  int pos; /* (if local==0: position in removed or kept where var is defined) */
  int poss; /* (if local==0: argument number of that constraint that defines the variable) */
  int anon;
  sem_expr_t def; /* only when local==1, this variable's definition */ 
  sem_cdeps_t cdeps;
} sem_var_t;

/* variable table */
typedef struct {
  alist_declare(sem_var_t,vars);
} sem_vartable_t;

typedef enum _sem_out_type_t_enum {
	SEM_OUT_TYPE_CON, /* an added constraint (not for guard) */
	SEM_OUT_TYPE_VAR, /* a local variable definition */
	SEM_OUT_TYPE_STM, /* a local statement */
	SEM_OUT_TYPE_EXP  /* an expression (only guard) */
} sem_out_type_t;

#define SEM_OUT_TYPE_CON 0
#define SEM_OUT_TYPE_VAR 1
#define SEM_OUT_TYPE_STM 2
#define SEM_OUT_TYPE_EXP 3

typedef struct {
	sem_out_type_t type;
	union {
		sem_conoccout_t con;
		int var;
		sem_expr_t exp;
	} data;
	sem_cdeps_t cdeps;
} sem_out_t;

/* a rule, having a (optional) name, a list of variables, a list of constraint occurences (in head & body), and a guard */
typedef struct {
  char *name;
  sem_vartable_t vt;
  alist_declare(sem_conocc_t,head[2]);
  alist_declare(sem_out_t,out[2]);
  int hook; /* index into con[SEM_RULE_LEVEL_KEPT], denoting what conocc this
               rule is hooked to */
} sem_rule_t;

typedef struct {
  char *name;
  alist_declare(int,types);
  sem_expr_t def;
} sem_macro_t;

/* a semantic tree, which is hardly a tree anymore */
typedef struct {
  alist_declare(sem_vartype_t,types);
  alist_declare(sem_rule_t,rules);
  alist_declare(char*,exts);
  alist_declare(sem_constr_t,cons);
  alist_declare(sem_macro_t,macros);
} sem_cchr_t;

#endif

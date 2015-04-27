/****************************************************************************\
| CCHR - A CHR-in-C to C compiler                                            |
| parsestr.h - definition of syntax tree data structure                      |
| written by Pieter Wuille                                                   |
\****************************************************************************/ 

#ifndef _parsestr_h_
#define _parsestr_h_ 1

#include <stdlib.h>
#include <string.h>

#include "alist.h"

/* the token types */
typedef enum enum_token_type_t {
  TOKEN_TYPE_LIT=1, /* a literal string */
  TOKEN_TYPE_SYMB=2, /* a symbol, possibly a variable name */
  TOKEN_TYPE_FUNC=3 /* a functional form like A(B,C,...) */
} token_type_t;

#define TOKEN_TYPE_LIT 1
#define TOKEN_TYPE_SYMB 2
#define TOKEN_TYPE_FUNC 3

typedef struct _expr_t_struct expr_t;

/* a token, the basic building block of an expression */
typedef struct {
  token_type_t type;
  char *data;
  alist_declare(expr_t,args); /* only when type==TOKEN_TYPE_FUNC */
} token_t;

/* an expression, being a sequence of tokens */
struct _expr_t_struct {
  alist_declare(token_t,list);
  char *occn;
};
 
/* a constraint having a name and a list of arguments (the types) */
typedef struct {
  char *name;
  alist_declare(char*,list);
  alist_declare(expr_t,args);
  char *occn;
} constr_t;

/* a list of expressions */
typedef struct {
  alist_declare(expr_t,list);
} exprlist_t;

/* a rule, having a name, 3 expression lists and a guard */
typedef struct {
  char *name;
  exprlist_t kept;
  exprlist_t removed;
  exprlist_t body;
  exprlist_t guard;
} rule_t;

typedef struct {
  constr_t name;
  expr_t def;
} macro_t;

typedef struct {
  char *name;
  char *cb;
} logical_t;

/* a syntax tree, having constraints, rules, and a list of external symbols */
typedef struct {
  alist_declare(constr_t,constrs);
  alist_declare(rule_t,rules);
  alist_declare(macro_t,macros);
  alist_declare(logical_t,logicals);
  alist_declare(char*,exts);
} cchr_t;

/* declaration of destructors */
void destruct_cchr_t (cchr_t *cchr);
void destruct_rule_t (rule_t *rule);
void destruct_exprlist_t (exprlist_t *exprl);
void destruct_constr_t (constr_t *constr);
void destruct_expr_t (expr_t *expr);
void destruct_token_t (token_t *tok);


#endif

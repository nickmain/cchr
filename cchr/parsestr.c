/****************************************************************************\
| CCHR - A CHR-in-C to C compiler                                            |
| parsestr.c - some helper routines for syntax tree data structure           |
| written by Pieter Wuille                                                   |
\****************************************************************************/ 

#include "parsestr.h"

#ifdef USE_EFENCE
#include <efence.h>
#endif

void destruct_token_t (token_t *tok) {
  free(tok->data);
  if (tok->type == TOKEN_TYPE_FUNC) {
    for (int i=0; i<alist_len(tok->args); i++) destruct_expr_t(alist_ptr(tok->args,i));
    alist_free(tok->args);
  }
}

void destruct_expr_t (expr_t *expr) {
  for (int i=0; i<alist_len(expr->list); i++) destruct_token_t(alist_ptr(expr->list,i));
  alist_free(expr->list);
  if (expr->occn) free(expr->occn);
  expr->occn=NULL;
}

void destruct_constr_t (constr_t *constr) {
  free(constr->name);
  for (int i=0; i<alist_len(constr->list); i++) free(alist_get(constr->list,i));
  alist_free(constr->list);
  for (int i=0; i<alist_len(constr->args); i++) destruct_expr_t(alist_ptr(constr->args,i));
  alist_free(constr->args);
}

void destruct_exprlist_t (exprlist_t *exprl) {
  for (int i=0; i<alist_len(exprl->list); i++) destruct_expr_t(alist_ptr(exprl->list,i));
  alist_free(exprl->list);
}

void destruct_rule_t (rule_t *rule) {
  free(rule->name);
  destruct_exprlist_t(&(rule->kept));
  destruct_exprlist_t(&(rule->removed));
  destruct_exprlist_t(&(rule->body));
  destruct_exprlist_t(&(rule->guard));
}

void destruct_macro_t (macro_t *macro) {
  destruct_constr_t(&(macro->name));
  destruct_expr_t(&(macro->def));
}

void destruct_cchr_t (cchr_t *cchr) {
  for (int i=0; i<alist_len(cchr->constrs); i++) destruct_constr_t(alist_ptr(cchr->constrs,i));
  alist_free(cchr->constrs);
  for (int i=0; i<alist_len(cchr->rules); i++) destruct_rule_t(alist_ptr(cchr->rules,i));
  alist_free(cchr->rules);
  for (int i=0; i<alist_len(cchr->macros); i++) destruct_macro_t(alist_ptr(cchr->macros,i));
  alist_free(cchr->macros);
  for (int i=0; i<alist_len(cchr->exts); i++) free(alist_get(cchr->exts,i));
  alist_free(cchr->exts);
}

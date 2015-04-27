/****************************************************************************\
| CCHR - A CHR-in-C to C compiler                                            |
| abs2sem.h - header for syntax tree to semantic tree conversion             |
| written by Pieter Wuille                                                   |
\****************************************************************************/ 


#ifndef ANALYSE_H_
#define ANALYSE_H_

#include "semtree.h"
#include "parsestr.h"

void sem_expr_destruct(sem_expr_t *expr);
void sem_expr_init(sem_expr_t *expr);
void sem_expr_copy(sem_expr_t *src,sem_expr_t *dst,int start,int stop);

void sem_exprpart_init_var(sem_exprpart_t *exprp, int var);
void sem_exprpart_init_lit(sem_exprpart_t *exprp, char *str);
void sem_exprpart_init_fun(sem_exprpart_t *exprp, char *str);

void sem_cchr_destruct(sem_cchr_t* cchr);

void sem_cchr_init(sem_cchr_t* cchr);

void sem_macro_init(sem_macro_t *mac);

int sem_generate_cchr(sem_cchr_t* out,cchr_t* in);

char *make_message(const char *fmt, ...);
char *copy_string(const char *str);

#endif /*ANALYSE_H_*/

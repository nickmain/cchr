%{
/****************************************************************************\
| CCHR - A CHR-in-C to C compiler                                            |
| cchr.y - YACC parser for CHR-in-C code                                     |
| written by Pieter Wuille                                                   |
\****************************************************************************/ 

#ifndef _cchr_y_
#define _cchr_y_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "parsestr.h"
#include "cchr.tab.h"

#ifdef USE_EFENCE
#include <efence.h>
#endif

typedef void *yyscan_t;

int static yyerror(YYLTYPE *loc,yyscan_t scanner,cchr_t *output,char *msg);
int yylex ( YYSTYPE * lvalp, YYLTYPE * llocp, yyscan_t scanner );

void cchr_init(cchr_t *cchr);
void cchr_merge(cchr_t *out,cchr_t *in);
void cchr_genrule(cchr_t *cchr,char *name,exprlist_t *kept,exprlist_t *removed,
                  exprlist_t *guard,exprlist_t *body);

#endif

#define YYLEX_PARAM scanner

%}

%locations
%pure-parser
%error-verbose

%parse-param { yyscan_t scanner }
%parse-param { cchr_t *output }

%union {
  char *lit;
  expr_t expr;
  token_t token;
  constr_t constr;
  cchr_t cchr;
  exprlist_t elist;
}

%token <lit> TOK_CONSTRAINT TOK_TRUE TOK_LCBRAC TOK_RCBRAC TOK_SEMI TOK_COMMA TOK_AT TOK_SIMP TOK_PROP TOK_SPIPE TOK_BSLASH TOK_LRBRAC	     TOK_RRBRAC TOK_FUNC TOK_SYMBAT TOK_CONST TOK_SYMB TOK_OP TOK_EXTERN TOK_BSTRING TOK_STRING TOK_ESTRING TOK_MACRO TOK_ASTER TOK_BCHAR TOK_CHAR TOK_ECHAR TOK_LOGICAL TOK_HASH

%token TOK_ERROR

%type  <lit> literal type rname string stringparts char charparts

%destructor { free($$); } TOK_CONSTRAINT TOK_TRUE TOK_LCBRAC TOK_RCBRAC TOK_SEMI TOK_COMMA TOK_AT TOK_SIMP TOK_PROP TOK_SPIPE TOK_BSLASH TOK_LRBRAC TOK_RRBRAC TOK_FUNC TOK_SYMBAT TOK_CONST TOK_SYMB TOK_OP literal type rname TOK_EXTERN TOK_STRING TOK_BSTRING TOK_ESTRING TOK_ASTER TOK_BCHAR TOK_CHAR TOK_ECHAR TOK_LOGICAL TOK_HASH

%type <constr> typelistc typelist constr carglist
%destructor { destruct_constr_t(&$$); } typelistc typelist constr carglist

%type <expr> tokenlist token etokenlist etoken stoken stokenlist functio
%destructor { destruct_expr_t(&$$); } tokenlist token etokenlist etoken stoken stokenlist functio

%type <token> arglist
%destructor { destruct_token_t(&$$); } arglist

%type <cchr> constrlist stmt input rule extlist
%destructor { destruct_cchr_t(&$$); } constrlist stmt input rule extlist

%type <elist> exprlist
%destructor { destruct_exprlist_t(&$$); } exprlist

%left TOK_COMMA
%left PRE_ENDALIST
%left PRE_EC
%left PRE_ELIST

%left PRE_ETLIST
%left TOK_RRBRAC

%start main

%%

main : input { *output=$1; }
	 | input TOK_RCBRAC { *output=$1; 
	   free($2); /* dumpCHR(output, 1); */ 
	   YYACCEPT; }
     ;

input : { cchr_init(&$$); }
        | input stmt { $$=$1; cchr_merge(&$$,&$2); }
        ;

tokenlist : tokenlist token { $$.list=$1.list; $$.occn=$1.occn; alist_addall($$.list,$2.list); alist_free($2.list); }
          | %prec PRE_ETLIST { alist_init($$.list); $$.occn=NULL; }
          ;

etokenlist : etokenlist etoken { $$.list=$1.list; alist_addall($$.list,$2.list); alist_free($2.list); $$.occn=NULL; }
           | %prec PRE_ETLIST { alist_init($$.list); $$.occn=NULL; }
           ;

stokenlist : stokenlist stoken { $$.list=$1.list; alist_addall($$.list,$2.list); alist_free($2.list); $$.occn=NULL; }
           | %prec PRE_ETLIST { alist_init($$.list); $$.occn=NULL; }
           ;

functio : TOK_FUNC arglist TOK_RRBRAC { 
    		alist_init($$.list);
		    $2.data=$1;
		    alist_add($$.list,$2);
		    free($3);
		    $$.occn=NULL;
		}
		| TOK_FUNC TOK_RRBRAC { 
   		    alist_init($$.list);
		    token_t *tok;
		    alist_new($$.list,tok);
		    alist_init(tok->args);
		    tok->type=TOKEN_TYPE_FUNC;
		    tok->data=$1;
		    free($2);
		    $$.occn=NULL;
		}

token : literal { alist_init($$.list); token_t *tok; alist_new($$.list,tok); tok->data=$1; tok->type=TOKEN_TYPE_LIT; alist_init(tok->args); $$.occn=NULL; }
	  | functio
	  | string  { alist_init($$.list); token_t *tok; alist_new($$.list,tok); tok->data=$1; tok->type=TOKEN_TYPE_LIT; alist_init(tok->args); $$.occn=NULL; }
	  | char  { alist_init($$.list); token_t *tok; alist_new($$.list,tok); tok->data=$1; tok->type=TOKEN_TYPE_LIT; alist_init(tok->args); $$.occn=NULL; }
	  | TOK_LCBRAC stokenlist TOK_RCBRAC { alist_init($$.list); token_t *tok; alist_new($$.list,tok); tok->data=$1; tok->type=TOKEN_TYPE_LIT; alist_addall($$.list,$2.list); alist_new($$.list,tok); tok->data=$3; tok->type=TOKEN_TYPE_LIT; alist_free($2.list); $$.occn=NULL; }
      | TOK_SYMB  { 
   		    alist_init($$.list);
		    token_t *tok;
		    alist_new($$.list,tok);
		    tok->data=$1;
		    tok->type=TOKEN_TYPE_SYMB;
		    alist_init(tok->args);
		    $$.occn=NULL; 
		  }
	  | TOK_LRBRAC TOK_RRBRAC {
   		    alist_init($$.list);
		    token_t *tok;
		    alist_new($$.list,tok);
		    alist_init(tok->args);
		    tok->type=TOKEN_TYPE_FUNC;
		    tok->data=NULL;
		    free($2);
		    free($1);
		    $$.occn=NULL; 
	  }
      | TOK_LRBRAC arglist TOK_RRBRAC { 
    		alist_init($$.list);
		    $2.data=NULL;
		    alist_add($$.list,$2);
		    free($3);
		    free($1);
		    $$.occn=NULL; 
		}
      ;

etoken : token
	   | TOK_SPIPE { alist_init($$.list); token_t *tok; alist_new($$.list,tok); tok->data=$1; tok->type=TOKEN_TYPE_LIT; $$.occn=NULL; }
	   | TOK_SYMBAT { alist_init($$.list); token_t *tok; alist_new($$.list,tok); tok->data=$1; tok->type=TOKEN_TYPE_SYMB; alist_new($$.list,tok); tok->data=malloc(2); strcpy(tok->data,"@"); tok->type=TOKEN_TYPE_LIT; $$.occn=NULL; }
	   ;

stoken : etoken
       | TOK_SEMI { alist_init($$.list); token_t *tok; alist_new($$.list,tok); tok->data=$1; tok->type=TOKEN_TYPE_LIT; $$.occn=NULL; }
	;

exprlist : TOK_TRUE { free($1); alist_init($$.list); }
		 | tokenlist %prec PRE_ELIST { $1.occn=NULL; alist_init($$.list); alist_add($$.list,$1); }
		 | tokenlist TOK_HASH TOK_SYMB %prec PRE_ELIST { $1.occn=$3; alist_init($$.list); alist_add($$.list,$1); free($2); }
		 | exprlist TOK_COMMA tokenlist { $3.occn=NULL; $$=$1; alist_add($$.list,$3); free($2); } ;
		 | exprlist TOK_COMMA tokenlist TOK_HASH TOK_SYMB { $3.occn=$5; $$=$1; alist_add($$.list,$3); free($2); free($4); } ;

string : TOK_BSTRING stringparts TOK_ESTRING { $$=malloc(strlen($1)+strlen($2)+strlen($3)+1); strcpy($$,$1); strcat($$,$2); strcat($$,$3); free($1); free($2); free($3); }
	   ;

char : TOK_BCHAR charparts TOK_ECHAR { $$=malloc(strlen($1)+strlen($2)+strlen($3)+1); strcpy($$,$1); strcat($$,$2); strcat($$,$3); free($1); free($2); free($3); }
	   ;

stringparts : { $$=malloc(1); $$[0]=0; }
			| stringparts TOK_STRING { 
				$$=malloc(strlen($1)+strlen($2)+1);
				strcpy($$,$1);
				strcat($$,$2);
				free($1);
				free($2);
			  }
			;

charparts : { $$=malloc(1); $$[0]=0; }
			| charparts TOK_CHAR { 
				$$=malloc(strlen($1)+strlen($2)+1);
				strcpy($$,$1);
				strcat($$,$2);
				free($1);
				free($2);
			  }
			;

arglist : etokenlist %prec PRE_ENDALIST { $$.type = TOKEN_TYPE_FUNC; alist_init($$.args); $$.data=NULL; alist_add($$.args,$1); }
		 | arglist TOK_COMMA etokenlist { $$=$1; alist_add($$.args,$3); free($2); }

extlist : TOK_SYMB { cchr_init(&$$); alist_add($$.exts,$1); }
         | extlist TOK_COMMA TOK_SYMB { $$=$1; alist_add($$.exts,$3); free($2); }
	 
literal : TOK_COMMA
        | TOK_CONST
        | TOK_OP
        | TOK_AT
	| TOK_ASTER
        ;

rname : TOK_SYMBAT { $$=$1; }
      | { $$=NULL; }
      ;

rule : rname exprlist TOK_BSLASH exprlist TOK_SIMP exprlist TOK_SPIPE exprlist TOK_SEMI { cchr_genrule(&$$,$1,&$2,&$4,&$6,&$8); free($3); free($5); free($7); free($9); }
     | rname exprlist TOK_SIMP exprlist TOK_SPIPE exprlist TOK_SEMI { cchr_genrule(&$$,$1,NULL,&$2,&$4,&$6); free($3); free($5); free($7); }
     | rname exprlist TOK_PROP exprlist TOK_SPIPE exprlist TOK_SEMI { cchr_genrule(&$$,$1,&$2,NULL,&$4,&$6); free($3); free($5); free($7); }
     | rname exprlist TOK_BSLASH exprlist TOK_SIMP exprlist TOK_SEMI { cchr_genrule(&$$,$1,&$2,&$4,NULL,&$6); free($3); free($5); free($7); }
     | rname exprlist TOK_SIMP exprlist TOK_SEMI { cchr_genrule(&$$,$1,NULL,&$2,NULL,&$4); free($3); free($5); }
     | rname exprlist TOK_PROP exprlist TOK_SEMI { cchr_genrule(&$$,$1,&$2,NULL,NULL,&$4); free($3); free($5); }
     ;


stmt : TOK_CONSTRAINT constrlist TOK_SEMI { $$=$2; free($1); free($3); }
     | TOK_EXTERN extlist TOK_SEMI { $$=$2; free($1); free($3); }
     | TOK_MACRO constr etokenlist TOK_SEMI { cchr_init(&$$); macro_t *nw; alist_new($$.macros,nw); nw->name=$2; nw->def=$3; free($1); free($4); }
     | TOK_LOGICAL type TOK_SYMB TOK_SEMI { cchr_init(&$$); logical_t *nw; alist_new($$.logicals,nw); nw->name=$2; nw->cb=$3; free($1); free($4); }
     | rule
     ;


constrlist : constr carglist { cchr_init(&$$); $1.args=$2.args; alist_add($$.constrs,$1); }
		   | constrlist TOK_COMMA constr carglist { $$=$1; $3.args=$4.args; alist_add($$.constrs,$3); free($2); }
		   ;

carglist : { alist_init($$.args); alist_init($$.list); $$.name=NULL; }
		 | carglist functio { alist_add($1.args,$2); $$=$1; }
		 ;

constr : TOK_FUNC typelist TOK_RRBRAC { $$=$2; $$.name=$1; free($3); alist_init($$.args); }


typelist :	{ $$.name=NULL; alist_init($$.list); }
		 | typelistc    { $$ = $1; }
		 ;

typelistc : type		{ $$.name=NULL; alist_init($$.list); alist_add($$.list,$1); }
		  | typelistc TOK_COMMA type { $$=$1; alist_add($$.list,$3); free($2); }
		  ;

type : TOK_SYMB { $$ = $1; }
     | type TOK_SYMB { $$ = realloc($1,strlen($1)+strlen($2)+2); strcat($$," "); strcat($$,$2); free($2); }
     | type TOK_ASTER { $$ = realloc($1,strlen($1)+strlen($2)+2); strcat($$," "); strcat($$,$2); free($2); }
     ;

%%

void cchr_init(cchr_t *cchr) {
  alist_init(cchr->constrs);
  alist_init(cchr->rules);
  alist_init(cchr->exts);
  alist_init(cchr->macros);
  alist_init(cchr->logicals);
}

void cchr_merge(cchr_t *out,cchr_t *in) {
  alist_addall(out->constrs,in->constrs);
  alist_addall(out->rules,in->rules);
  alist_addall(out->exts,in->exts);
  alist_addall(out->macros,in->macros);
  alist_addall(out->logicals,in->logicals);
  alist_free(in->constrs);
  alist_free(in->rules);
  alist_free(in->exts);
  alist_free(in->macros);
  alist_free(in->logicals);
}

void cchr_genrule(cchr_t *cchr,char *name,exprlist_t *kept,exprlist_t *removed,exprlist_t *guard,exprlist_t *body) {
  cchr_init(cchr);
  rule_t *rule; alist_new(cchr->rules,rule);
  rule->name=name;
  if (kept) {
    rule->kept = *kept;
  } else {
    alist_init(rule->kept.list);
  }
  if (removed) {
    rule->removed = *removed;
  } else {
    alist_init(rule->removed.list);
  }
  if (body) {
    rule->body = *body;
  } else {
    alist_init(rule->body.list);
  }
  if (guard) {
    rule->guard= *guard;
  } else {
    alist_init(rule->guard.list);
  }
}

int static yyerror(YYLTYPE *loc,yyscan_t scanner,cchr_t *output,char *msg) {
  fprintf(stderr,"Parse error on line %i: %s\n",loc->last_line,msg);
  return 1;
}



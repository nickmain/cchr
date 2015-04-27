/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     TOK_CONSTRAINT = 258,
     TOK_TRUE = 259,
     TOK_LCBRAC = 260,
     TOK_RCBRAC = 261,
     TOK_SEMI = 262,
     TOK_COMMA = 263,
     TOK_AT = 264,
     TOK_SIMP = 265,
     TOK_PROP = 266,
     TOK_SPIPE = 267,
     TOK_BSLASH = 268,
     TOK_LRBRAC = 269,
     TOK_RRBRAC = 270,
     TOK_FUNC = 271,
     TOK_SYMBAT = 272,
     TOK_CONST = 273,
     TOK_SYMB = 274,
     TOK_OP = 275,
     TOK_EXTERN = 276,
     TOK_BSTRING = 277,
     TOK_STRING = 278,
     TOK_ESTRING = 279,
     TOK_MACRO = 280,
     TOK_ASTER = 281,
     TOK_BCHAR = 282,
     TOK_CHAR = 283,
     TOK_ECHAR = 284,
     TOK_LOGICAL = 285,
     TOK_HASH = 286,
     TOK_ERROR = 287,
     PRE_ENDALIST = 288,
     PRE_EC = 289,
     PRE_ELIST = 290,
     PRE_ETLIST = 291
   };
#endif
/* Tokens.  */
#define TOK_CONSTRAINT 258
#define TOK_TRUE 259
#define TOK_LCBRAC 260
#define TOK_RCBRAC 261
#define TOK_SEMI 262
#define TOK_COMMA 263
#define TOK_AT 264
#define TOK_SIMP 265
#define TOK_PROP 266
#define TOK_SPIPE 267
#define TOK_BSLASH 268
#define TOK_LRBRAC 269
#define TOK_RRBRAC 270
#define TOK_FUNC 271
#define TOK_SYMBAT 272
#define TOK_CONST 273
#define TOK_SYMB 274
#define TOK_OP 275
#define TOK_EXTERN 276
#define TOK_BSTRING 277
#define TOK_STRING 278
#define TOK_ESTRING 279
#define TOK_MACRO 280
#define TOK_ASTER 281
#define TOK_BCHAR 282
#define TOK_CHAR 283
#define TOK_ECHAR 284
#define TOK_LOGICAL 285
#define TOK_HASH 286
#define TOK_ERROR 287
#define PRE_ENDALIST 288
#define PRE_EC 289
#define PRE_ELIST 290
#define PRE_ETLIST 291




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 45 "cchr.y"
{
  char *lit;
  expr_t expr;
  token_t token;
  constr_t constr;
  cchr_t cchr;
  exprlist_t elist;
}
/* Line 1489 of yacc.c.  */
#line 130 "intermediate/cchr.tab.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} YYLTYPE;
# define yyltype YYLTYPE /* obsolescent; will be withdrawn */
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif



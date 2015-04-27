/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Using locations.  */
#define YYLSP_NEEDED 1



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




/* Copy the first part of user declarations.  */
#line 1 "cchr.y"

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



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

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
/* Line 187 of yacc.c.  */
#line 214 "intermediate/cchr.tab.c"
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


/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 239 "intermediate/cchr.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
	     && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
    YYLTYPE yyls;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE) + sizeof (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   141

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  37
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  27
/* YYNRULES -- Number of rules.  */
#define YYNRULES  71
/* YYNRULES -- Number of states.  */
#define YYNSTATES  111

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   291

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint8 yyprhs[] =
{
       0,     0,     3,     5,     8,     9,    12,    15,    16,    19,
      20,    23,    24,    28,    31,    33,    35,    37,    39,    43,
      45,    48,    52,    54,    56,    58,    60,    62,    64,    66,
      70,    74,    80,    84,    88,    89,    92,    93,    96,    98,
     102,   104,   108,   110,   112,   114,   116,   118,   120,   121,
     131,   139,   147,   155,   161,   167,   171,   175,   180,   185,
     187,   190,   195,   196,   199,   203,   204,   206,   208,   212,
     214,   217
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      38,     0,    -1,    39,    -1,    39,     6,    -1,    -1,    39,
      57,    -1,    40,    44,    -1,    -1,    41,    45,    -1,    -1,
      42,    46,    -1,    -1,    16,    52,    15,    -1,    16,    15,
      -1,    54,    -1,    43,    -1,    48,    -1,    49,    -1,     5,
      42,     6,    -1,    19,    -1,    14,    15,    -1,    14,    52,
      15,    -1,    44,    -1,    12,    -1,    17,    -1,    45,    -1,
       7,    -1,     4,    -1,    40,    -1,    40,    31,    19,    -1,
      47,     8,    40,    -1,    47,     8,    40,    31,    19,    -1,
      22,    50,    24,    -1,    27,    51,    29,    -1,    -1,    50,
      23,    -1,    -1,    51,    28,    -1,    41,    -1,    52,     8,
      41,    -1,    19,    -1,    53,     8,    19,    -1,     8,    -1,
      18,    -1,    20,    -1,     9,    -1,    26,    -1,    17,    -1,
      -1,    55,    47,    13,    47,    10,    47,    12,    47,     7,
      -1,    55,    47,    10,    47,    12,    47,     7,    -1,    55,
      47,    11,    47,    12,    47,     7,    -1,    55,    47,    13,
      47,    10,    47,     7,    -1,    55,    47,    10,    47,     7,
      -1,    55,    47,    11,    47,     7,    -1,     3,    58,     7,
      -1,    21,    53,     7,    -1,    25,    60,    41,     7,    -1,
      30,    63,    19,     7,    -1,    56,    -1,    60,    59,    -1,
      58,     8,    60,    59,    -1,    -1,    59,    43,    -1,    16,
      61,    15,    -1,    -1,    62,    -1,    63,    -1,    62,     8,
      63,    -1,    19,    -1,    63,    19,    -1,    63,    26,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    89,    89,    90,    95,    96,    99,   100,   103,   104,
     107,   108,   111,   118,   129,   130,   131,   132,   133,   134,
     143,   154,   164,   165,   166,   169,   170,   173,   174,   175,
     176,   177,   179,   182,   185,   186,   195,   196,   205,   206,
     208,   209,   211,   212,   213,   214,   215,   218,   219,   222,
     223,   224,   225,   226,   227,   231,   232,   233,   234,   235,
     239,   240,   243,   244,   247,   250,   251,   254,   255,   258,
     259,   260
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "TOK_CONSTRAINT", "TOK_TRUE",
  "TOK_LCBRAC", "TOK_RCBRAC", "TOK_SEMI", "TOK_COMMA", "TOK_AT",
  "TOK_SIMP", "TOK_PROP", "TOK_SPIPE", "TOK_BSLASH", "TOK_LRBRAC",
  "TOK_RRBRAC", "TOK_FUNC", "TOK_SYMBAT", "TOK_CONST", "TOK_SYMB",
  "TOK_OP", "TOK_EXTERN", "TOK_BSTRING", "TOK_STRING", "TOK_ESTRING",
  "TOK_MACRO", "TOK_ASTER", "TOK_BCHAR", "TOK_CHAR", "TOK_ECHAR",
  "TOK_LOGICAL", "TOK_HASH", "TOK_ERROR", "PRE_ENDALIST", "PRE_EC",
  "PRE_ELIST", "PRE_ETLIST", "$accept", "main", "input", "tokenlist",
  "etokenlist", "stokenlist", "functio", "token", "etoken", "stoken",
  "exprlist", "string", "char", "stringparts", "charparts", "arglist",
  "extlist", "literal", "rname", "rule", "stmt", "constrlist", "carglist",
  "constr", "typelist", "typelistc", "type", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    37,    38,    38,    39,    39,    40,    40,    41,    41,
      42,    42,    43,    43,    44,    44,    44,    44,    44,    44,
      44,    44,    45,    45,    45,    46,    46,    47,    47,    47,
      47,    47,    48,    49,    50,    50,    51,    51,    52,    52,
      53,    53,    54,    54,    54,    54,    54,    55,    55,    56,
      56,    56,    56,    56,    56,    57,    57,    57,    57,    57,
      58,    58,    59,    59,    60,    61,    61,    62,    62,    63,
      63,    63
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     2,     0,     2,     2,     0,     2,     0,
       2,     0,     3,     2,     1,     1,     1,     1,     3,     1,
       2,     3,     1,     1,     1,     1,     1,     1,     1,     3,
       3,     5,     3,     3,     0,     2,     0,     2,     1,     3,
       1,     3,     1,     1,     1,     1,     1,     1,     0,     9,
       7,     7,     7,     5,     5,     3,     3,     4,     4,     1,
       2,     4,     0,     2,     3,     0,     1,     1,     3,     1,
       2,     2
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       4,     0,    48,     1,     0,     3,    47,     0,     0,     0,
       7,    59,     5,    65,     0,    62,    40,     0,     9,    69,
       0,    27,    28,     0,     0,    66,    67,    55,     0,    60,
      56,     0,     0,    70,    71,    11,    42,    45,     9,     9,
      43,    19,    44,    34,    46,    36,     0,    15,     6,    16,
      17,    14,     7,     7,     7,     7,    64,     0,    70,    62,
      63,    41,    57,    23,    24,    22,     8,    58,     0,    20,
      38,     0,    13,     0,     0,     0,    29,    30,     0,     0,
       0,    68,    61,    18,    26,    25,    10,     9,    21,    12,
      35,    32,    37,    33,     0,    53,     7,    54,     7,     7,
      39,    31,     0,     0,     0,    50,    51,    52,     7,     0,
      49
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     1,     2,    22,    70,    68,    47,    65,    66,    86,
      23,    49,    50,    74,    75,    71,    17,    51,    10,    11,
      12,    14,    29,    15,    24,    25,    20
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -53
static const yytype_int8 yypact[] =
{
     -53,     9,     7,   -53,    19,   -53,   -53,    39,    19,    56,
      75,   -53,   -53,    56,    44,   -53,   -53,    47,   -53,   -53,
      -8,   -53,     3,    32,    79,    88,     1,   -53,    19,    94,
     -53,    93,    97,   114,   -53,   -53,   -53,   -53,   105,   111,
     -53,   -53,   -53,   -53,   -53,   -53,   109,   -53,   -53,   -53,
     -53,   -53,   -53,    75,    75,    75,   -53,    56,   -53,   -53,
     -53,   -53,   -53,   -53,   -53,   -53,   -53,   -53,    81,   -53,
     113,    18,   -53,    23,    -9,    33,   -53,    54,    41,    59,
      31,     1,    94,   -53,   -53,   -53,   -53,   -53,   -53,   -53,
     -53,   -53,   -53,   -53,   115,   -53,    75,   -53,    75,    75,
     113,   -53,    57,    76,    70,   -53,   -53,   -53,    75,    84,
     -53
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -53,   -53,   -53,    85,   -18,   -53,   -25,   -17,    68,   -53,
     -52,   -53,   -53,   -53,   -53,    99,   -53,   -53,   -53,   -53,
     -53,   -53,    82,     8,   -53,   -53,    -7
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -3
static const yytype_int8 yytable[] =
{
      32,    78,    79,    80,    60,    48,    26,    -2,    35,     3,
       4,    33,    37,     5,    90,    91,    18,    38,    34,    39,
      58,    40,    41,    42,     6,    43,    87,    34,     7,    44,
      45,    87,     8,    88,    46,    13,    59,     9,    89,    52,
      52,    99,    53,    54,   102,    55,   103,   104,    95,    52,
      81,    27,    28,    96,    30,    31,   109,    60,    16,    35,
      48,    92,    93,    37,   105,    52,    97,    52,    38,   100,
      39,    98,    40,    41,    42,    19,    43,   107,    52,    21,
      44,    45,   108,   106,    52,    94,    35,    83,    84,    36,
      37,   110,    52,    63,    56,    38,    57,    39,    64,    40,
      41,    42,    35,    43,    62,    36,    37,    44,    45,    63,
      39,    38,    61,    39,    64,    40,    41,    42,    35,    43,
      69,    67,    37,    44,    45,    63,    72,    38,    76,    39,
      64,    40,    41,    42,   101,    43,    85,    77,    73,    44,
      45,    82
};

static const yytype_uint8 yycheck[] =
{
      18,    53,    54,    55,    29,    22,    13,     0,     5,     0,
       3,    19,     9,     6,    23,    24,     8,    14,    26,    16,
      19,    18,    19,    20,    17,    22,     8,    26,    21,    26,
      27,     8,    25,    15,    31,    16,    28,    30,    15,     8,
       8,    10,    10,    11,    96,    13,    98,    99,     7,     8,
      57,     7,     8,    12,     7,     8,   108,    82,    19,     5,
      77,    28,    29,     9,     7,     8,     7,     8,    14,    87,
      16,    12,    18,    19,    20,    19,    22,     7,     8,     4,
      26,    27,    12,     7,     8,    31,     5,     6,     7,     8,
       9,     7,     8,    12,    15,    14,     8,    16,    17,    18,
      19,    20,     5,    22,     7,     8,     9,    26,    27,    12,
      16,    14,    19,    16,    17,    18,    19,    20,     5,    22,
      15,     7,     9,    26,    27,    12,    15,    14,    19,    16,
      17,    18,    19,    20,    19,    22,    68,    52,    39,    26,
      27,    59
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    38,    39,     0,     3,     6,    17,    21,    25,    30,
      55,    56,    57,    16,    58,    60,    19,    53,    60,    19,
      63,     4,    40,    47,    61,    62,    63,     7,     8,    59,
       7,     8,    41,    19,    26,     5,     8,     9,    14,    16,
      18,    19,    20,    22,    26,    27,    31,    43,    44,    48,
      49,    54,     8,    10,    11,    13,    15,     8,    19,    60,
      43,    19,     7,    12,    17,    44,    45,     7,    42,    15,
      41,    52,    15,    52,    50,    51,    19,    40,    47,    47,
      47,    63,    59,     6,     7,    45,    46,     8,    15,    15,
      23,    24,    28,    29,    31,     7,    12,     7,    12,    10,
      41,    19,    47,    47,    47,     7,     7,     7,    12,    47,
       7
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (&yylloc, scanner, output, YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (&yylval, &yylloc, YYLEX_PARAM)
#else
# define YYLEX yylex (&yylval, &yylloc)
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value, Location, scanner, output); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, yyscan_t scanner, cchr_t *output)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep, yylocationp, scanner, output)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
    YYLTYPE const * const yylocationp;
    yyscan_t scanner;
    cchr_t *output;
#endif
{
  if (!yyvaluep)
    return;
  YYUSE (yylocationp);
  YYUSE (scanner);
  YYUSE (output);
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, yyscan_t scanner, cchr_t *output)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep, yylocationp, scanner, output)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
    YYLTYPE const * const yylocationp;
    yyscan_t scanner;
    cchr_t *output;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  YY_LOCATION_PRINT (yyoutput, *yylocationp);
  YYFPRINTF (yyoutput, ": ");
  yy_symbol_value_print (yyoutput, yytype, yyvaluep, yylocationp, scanner, output);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, YYLTYPE *yylsp, int yyrule, yyscan_t scanner, cchr_t *output)
#else
static void
yy_reduce_print (yyvsp, yylsp, yyrule, scanner, output)
    YYSTYPE *yyvsp;
    YYLTYPE *yylsp;
    int yyrule;
    yyscan_t scanner;
    cchr_t *output;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       , &(yylsp[(yyi + 1) - (yynrhs)])		       , scanner, output);
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, yylsp, Rule, scanner, output); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, YYLTYPE *yylocationp, yyscan_t scanner, cchr_t *output)
#else
static void
yydestruct (yymsg, yytype, yyvaluep, yylocationp, scanner, output)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
    YYLTYPE *yylocationp;
    yyscan_t scanner;
    cchr_t *output;
#endif
{
  YYUSE (yyvaluep);
  YYUSE (yylocationp);
  YYUSE (scanner);
  YYUSE (output);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {
      case 3: /* "TOK_CONSTRAINT" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1266 "intermediate/cchr.tab.c"
	break;
      case 4: /* "TOK_TRUE" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1271 "intermediate/cchr.tab.c"
	break;
      case 5: /* "TOK_LCBRAC" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1276 "intermediate/cchr.tab.c"
	break;
      case 6: /* "TOK_RCBRAC" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1281 "intermediate/cchr.tab.c"
	break;
      case 7: /* "TOK_SEMI" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1286 "intermediate/cchr.tab.c"
	break;
      case 8: /* "TOK_COMMA" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1291 "intermediate/cchr.tab.c"
	break;
      case 9: /* "TOK_AT" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1296 "intermediate/cchr.tab.c"
	break;
      case 10: /* "TOK_SIMP" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1301 "intermediate/cchr.tab.c"
	break;
      case 11: /* "TOK_PROP" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1306 "intermediate/cchr.tab.c"
	break;
      case 12: /* "TOK_SPIPE" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1311 "intermediate/cchr.tab.c"
	break;
      case 13: /* "TOK_BSLASH" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1316 "intermediate/cchr.tab.c"
	break;
      case 14: /* "TOK_LRBRAC" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1321 "intermediate/cchr.tab.c"
	break;
      case 15: /* "TOK_RRBRAC" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1326 "intermediate/cchr.tab.c"
	break;
      case 16: /* "TOK_FUNC" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1331 "intermediate/cchr.tab.c"
	break;
      case 17: /* "TOK_SYMBAT" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1336 "intermediate/cchr.tab.c"
	break;
      case 18: /* "TOK_CONST" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1341 "intermediate/cchr.tab.c"
	break;
      case 19: /* "TOK_SYMB" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1346 "intermediate/cchr.tab.c"
	break;
      case 20: /* "TOK_OP" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1351 "intermediate/cchr.tab.c"
	break;
      case 21: /* "TOK_EXTERN" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1356 "intermediate/cchr.tab.c"
	break;
      case 22: /* "TOK_BSTRING" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1361 "intermediate/cchr.tab.c"
	break;
      case 23: /* "TOK_STRING" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1366 "intermediate/cchr.tab.c"
	break;
      case 24: /* "TOK_ESTRING" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1371 "intermediate/cchr.tab.c"
	break;
      case 26: /* "TOK_ASTER" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1376 "intermediate/cchr.tab.c"
	break;
      case 27: /* "TOK_BCHAR" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1381 "intermediate/cchr.tab.c"
	break;
      case 28: /* "TOK_CHAR" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1386 "intermediate/cchr.tab.c"
	break;
      case 29: /* "TOK_ECHAR" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1391 "intermediate/cchr.tab.c"
	break;
      case 30: /* "TOK_LOGICAL" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1396 "intermediate/cchr.tab.c"
	break;
      case 31: /* "TOK_HASH" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1401 "intermediate/cchr.tab.c"
	break;
      case 39: /* "input" */
#line 72 "cchr.y"
	{ destruct_cchr_t(&(yyvaluep->cchr)); };
#line 1406 "intermediate/cchr.tab.c"
	break;
      case 40: /* "tokenlist" */
#line 66 "cchr.y"
	{ destruct_expr_t(&(yyvaluep->expr)); };
#line 1411 "intermediate/cchr.tab.c"
	break;
      case 41: /* "etokenlist" */
#line 66 "cchr.y"
	{ destruct_expr_t(&(yyvaluep->expr)); };
#line 1416 "intermediate/cchr.tab.c"
	break;
      case 42: /* "stokenlist" */
#line 66 "cchr.y"
	{ destruct_expr_t(&(yyvaluep->expr)); };
#line 1421 "intermediate/cchr.tab.c"
	break;
      case 43: /* "functio" */
#line 66 "cchr.y"
	{ destruct_expr_t(&(yyvaluep->expr)); };
#line 1426 "intermediate/cchr.tab.c"
	break;
      case 44: /* "token" */
#line 66 "cchr.y"
	{ destruct_expr_t(&(yyvaluep->expr)); };
#line 1431 "intermediate/cchr.tab.c"
	break;
      case 45: /* "etoken" */
#line 66 "cchr.y"
	{ destruct_expr_t(&(yyvaluep->expr)); };
#line 1436 "intermediate/cchr.tab.c"
	break;
      case 46: /* "stoken" */
#line 66 "cchr.y"
	{ destruct_expr_t(&(yyvaluep->expr)); };
#line 1441 "intermediate/cchr.tab.c"
	break;
      case 47: /* "exprlist" */
#line 75 "cchr.y"
	{ destruct_exprlist_t(&(yyvaluep->elist)); };
#line 1446 "intermediate/cchr.tab.c"
	break;
      case 52: /* "arglist" */
#line 69 "cchr.y"
	{ destruct_token_t(&(yyvaluep->token)); };
#line 1451 "intermediate/cchr.tab.c"
	break;
      case 53: /* "extlist" */
#line 72 "cchr.y"
	{ destruct_cchr_t(&(yyvaluep->cchr)); };
#line 1456 "intermediate/cchr.tab.c"
	break;
      case 54: /* "literal" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1461 "intermediate/cchr.tab.c"
	break;
      case 55: /* "rname" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1466 "intermediate/cchr.tab.c"
	break;
      case 56: /* "rule" */
#line 72 "cchr.y"
	{ destruct_cchr_t(&(yyvaluep->cchr)); };
#line 1471 "intermediate/cchr.tab.c"
	break;
      case 57: /* "stmt" */
#line 72 "cchr.y"
	{ destruct_cchr_t(&(yyvaluep->cchr)); };
#line 1476 "intermediate/cchr.tab.c"
	break;
      case 58: /* "constrlist" */
#line 72 "cchr.y"
	{ destruct_cchr_t(&(yyvaluep->cchr)); };
#line 1481 "intermediate/cchr.tab.c"
	break;
      case 59: /* "carglist" */
#line 63 "cchr.y"
	{ destruct_constr_t(&(yyvaluep->constr)); };
#line 1486 "intermediate/cchr.tab.c"
	break;
      case 60: /* "constr" */
#line 63 "cchr.y"
	{ destruct_constr_t(&(yyvaluep->constr)); };
#line 1491 "intermediate/cchr.tab.c"
	break;
      case 61: /* "typelist" */
#line 63 "cchr.y"
	{ destruct_constr_t(&(yyvaluep->constr)); };
#line 1496 "intermediate/cchr.tab.c"
	break;
      case 62: /* "typelistc" */
#line 63 "cchr.y"
	{ destruct_constr_t(&(yyvaluep->constr)); };
#line 1501 "intermediate/cchr.tab.c"
	break;
      case 63: /* "type" */
#line 60 "cchr.y"
	{ free((yyvaluep->lit)); };
#line 1506 "intermediate/cchr.tab.c"
	break;

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (yyscan_t scanner, cchr_t *output);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */






/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (yyscan_t scanner, cchr_t *output)
#else
int
yyparse (scanner, output)
    yyscan_t scanner;
    cchr_t *output;
#endif
#endif
{
  /* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;
/* Location data for the look-ahead symbol.  */
YYLTYPE yylloc;

  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;

  /* The location stack.  */
  YYLTYPE yylsa[YYINITDEPTH];
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;
  /* The locations where the error started and ended.  */
  YYLTYPE yyerror_range[2];

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;
  yylsp = yyls;
#if YYLTYPE_IS_TRIVIAL
  /* Initialize the default location before parsing starts.  */
  yylloc.first_line   = yylloc.last_line   = 1;
  yylloc.first_column = yylloc.last_column = 0;
#endif

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;
	YYLTYPE *yyls1 = yyls;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yyls1, yysize * sizeof (*yylsp),
		    &yystacksize);
	yyls = yyls1;
	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);
	YYSTACK_RELOCATE (yyls);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;
  *++yylsp = yylloc;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location.  */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 89 "cchr.y"
    { *output=(yyvsp[(1) - (1)].cchr); ;}
    break;

  case 3:
#line 90 "cchr.y"
    { *output=(yyvsp[(1) - (2)].cchr); 
	   free((yyvsp[(2) - (2)].lit)); /* dumpCHR(output, 1); */ 
	   YYACCEPT; ;}
    break;

  case 4:
#line 95 "cchr.y"
    { cchr_init(&(yyval.cchr)); ;}
    break;

  case 5:
#line 96 "cchr.y"
    { (yyval.cchr)=(yyvsp[(1) - (2)].cchr); cchr_merge(&(yyval.cchr),&(yyvsp[(2) - (2)].cchr)); ;}
    break;

  case 6:
#line 99 "cchr.y"
    { (yyval.expr).list=(yyvsp[(1) - (2)].expr).list; (yyval.expr).occn=(yyvsp[(1) - (2)].expr).occn; alist_addall((yyval.expr).list,(yyvsp[(2) - (2)].expr).list); alist_free((yyvsp[(2) - (2)].expr).list); ;}
    break;

  case 7:
#line 100 "cchr.y"
    { alist_init((yyval.expr).list); (yyval.expr).occn=NULL; ;}
    break;

  case 8:
#line 103 "cchr.y"
    { (yyval.expr).list=(yyvsp[(1) - (2)].expr).list; alist_addall((yyval.expr).list,(yyvsp[(2) - (2)].expr).list); alist_free((yyvsp[(2) - (2)].expr).list); (yyval.expr).occn=NULL; ;}
    break;

  case 9:
#line 104 "cchr.y"
    { alist_init((yyval.expr).list); (yyval.expr).occn=NULL; ;}
    break;

  case 10:
#line 107 "cchr.y"
    { (yyval.expr).list=(yyvsp[(1) - (2)].expr).list; alist_addall((yyval.expr).list,(yyvsp[(2) - (2)].expr).list); alist_free((yyvsp[(2) - (2)].expr).list); (yyval.expr).occn=NULL; ;}
    break;

  case 11:
#line 108 "cchr.y"
    { alist_init((yyval.expr).list); (yyval.expr).occn=NULL; ;}
    break;

  case 12:
#line 111 "cchr.y"
    { 
    		alist_init((yyval.expr).list);
		    (yyvsp[(2) - (3)].token).data=(yyvsp[(1) - (3)].lit);
		    alist_add((yyval.expr).list,(yyvsp[(2) - (3)].token));
		    free((yyvsp[(3) - (3)].lit));
		    (yyval.expr).occn=NULL;
		;}
    break;

  case 13:
#line 118 "cchr.y"
    { 
   		    alist_init((yyval.expr).list);
		    token_t *tok;
		    alist_new((yyval.expr).list,tok);
		    alist_init(tok->args);
		    tok->type=TOKEN_TYPE_FUNC;
		    tok->data=(yyvsp[(1) - (2)].lit);
		    free((yyvsp[(2) - (2)].lit));
		    (yyval.expr).occn=NULL;
		;}
    break;

  case 14:
#line 129 "cchr.y"
    { alist_init((yyval.expr).list); token_t *tok; alist_new((yyval.expr).list,tok); tok->data=(yyvsp[(1) - (1)].lit); tok->type=TOKEN_TYPE_LIT; alist_init(tok->args); (yyval.expr).occn=NULL; ;}
    break;

  case 16:
#line 131 "cchr.y"
    { alist_init((yyval.expr).list); token_t *tok; alist_new((yyval.expr).list,tok); tok->data=(yyvsp[(1) - (1)].lit); tok->type=TOKEN_TYPE_LIT; alist_init(tok->args); (yyval.expr).occn=NULL; ;}
    break;

  case 17:
#line 132 "cchr.y"
    { alist_init((yyval.expr).list); token_t *tok; alist_new((yyval.expr).list,tok); tok->data=(yyvsp[(1) - (1)].lit); tok->type=TOKEN_TYPE_LIT; alist_init(tok->args); (yyval.expr).occn=NULL; ;}
    break;

  case 18:
#line 133 "cchr.y"
    { alist_init((yyval.expr).list); token_t *tok; alist_new((yyval.expr).list,tok); tok->data=(yyvsp[(1) - (3)].lit); tok->type=TOKEN_TYPE_LIT; alist_addall((yyval.expr).list,(yyvsp[(2) - (3)].expr).list); alist_new((yyval.expr).list,tok); tok->data=(yyvsp[(3) - (3)].lit); tok->type=TOKEN_TYPE_LIT; alist_free((yyvsp[(2) - (3)].expr).list); (yyval.expr).occn=NULL; ;}
    break;

  case 19:
#line 134 "cchr.y"
    { 
   		    alist_init((yyval.expr).list);
		    token_t *tok;
		    alist_new((yyval.expr).list,tok);
		    tok->data=(yyvsp[(1) - (1)].lit);
		    tok->type=TOKEN_TYPE_SYMB;
		    alist_init(tok->args);
		    (yyval.expr).occn=NULL; 
		  ;}
    break;

  case 20:
#line 143 "cchr.y"
    {
   		    alist_init((yyval.expr).list);
		    token_t *tok;
		    alist_new((yyval.expr).list,tok);
		    alist_init(tok->args);
		    tok->type=TOKEN_TYPE_FUNC;
		    tok->data=NULL;
		    free((yyvsp[(2) - (2)].lit));
		    free((yyvsp[(1) - (2)].lit));
		    (yyval.expr).occn=NULL; 
	  ;}
    break;

  case 21:
#line 154 "cchr.y"
    { 
    		alist_init((yyval.expr).list);
		    (yyvsp[(2) - (3)].token).data=NULL;
		    alist_add((yyval.expr).list,(yyvsp[(2) - (3)].token));
		    free((yyvsp[(3) - (3)].lit));
		    free((yyvsp[(1) - (3)].lit));
		    (yyval.expr).occn=NULL; 
		;}
    break;

  case 23:
#line 165 "cchr.y"
    { alist_init((yyval.expr).list); token_t *tok; alist_new((yyval.expr).list,tok); tok->data=(yyvsp[(1) - (1)].lit); tok->type=TOKEN_TYPE_LIT; (yyval.expr).occn=NULL; ;}
    break;

  case 24:
#line 166 "cchr.y"
    { alist_init((yyval.expr).list); token_t *tok; alist_new((yyval.expr).list,tok); tok->data=(yyvsp[(1) - (1)].lit); tok->type=TOKEN_TYPE_SYMB; alist_new((yyval.expr).list,tok); tok->data=malloc(2); strcpy(tok->data,"@"); tok->type=TOKEN_TYPE_LIT; (yyval.expr).occn=NULL; ;}
    break;

  case 26:
#line 170 "cchr.y"
    { alist_init((yyval.expr).list); token_t *tok; alist_new((yyval.expr).list,tok); tok->data=(yyvsp[(1) - (1)].lit); tok->type=TOKEN_TYPE_LIT; (yyval.expr).occn=NULL; ;}
    break;

  case 27:
#line 173 "cchr.y"
    { free((yyvsp[(1) - (1)].lit)); alist_init((yyval.elist).list); ;}
    break;

  case 28:
#line 174 "cchr.y"
    { (yyvsp[(1) - (1)].expr).occn=NULL; alist_init((yyval.elist).list); alist_add((yyval.elist).list,(yyvsp[(1) - (1)].expr)); ;}
    break;

  case 29:
#line 175 "cchr.y"
    { (yyvsp[(1) - (3)].expr).occn=(yyvsp[(3) - (3)].lit); alist_init((yyval.elist).list); alist_add((yyval.elist).list,(yyvsp[(1) - (3)].expr)); free((yyvsp[(2) - (3)].lit)); ;}
    break;

  case 30:
#line 176 "cchr.y"
    { (yyvsp[(3) - (3)].expr).occn=NULL; (yyval.elist)=(yyvsp[(1) - (3)].elist); alist_add((yyval.elist).list,(yyvsp[(3) - (3)].expr)); free((yyvsp[(2) - (3)].lit)); ;}
    break;

  case 31:
#line 177 "cchr.y"
    { (yyvsp[(3) - (5)].expr).occn=(yyvsp[(5) - (5)].lit); (yyval.elist)=(yyvsp[(1) - (5)].elist); alist_add((yyval.elist).list,(yyvsp[(3) - (5)].expr)); free((yyvsp[(2) - (5)].lit)); free((yyvsp[(4) - (5)].lit)); ;}
    break;

  case 32:
#line 179 "cchr.y"
    { (yyval.lit)=malloc(strlen((yyvsp[(1) - (3)].lit))+strlen((yyvsp[(2) - (3)].lit))+strlen((yyvsp[(3) - (3)].lit))+1); strcpy((yyval.lit),(yyvsp[(1) - (3)].lit)); strcat((yyval.lit),(yyvsp[(2) - (3)].lit)); strcat((yyval.lit),(yyvsp[(3) - (3)].lit)); free((yyvsp[(1) - (3)].lit)); free((yyvsp[(2) - (3)].lit)); free((yyvsp[(3) - (3)].lit)); ;}
    break;

  case 33:
#line 182 "cchr.y"
    { (yyval.lit)=malloc(strlen((yyvsp[(1) - (3)].lit))+strlen((yyvsp[(2) - (3)].lit))+strlen((yyvsp[(3) - (3)].lit))+1); strcpy((yyval.lit),(yyvsp[(1) - (3)].lit)); strcat((yyval.lit),(yyvsp[(2) - (3)].lit)); strcat((yyval.lit),(yyvsp[(3) - (3)].lit)); free((yyvsp[(1) - (3)].lit)); free((yyvsp[(2) - (3)].lit)); free((yyvsp[(3) - (3)].lit)); ;}
    break;

  case 34:
#line 185 "cchr.y"
    { (yyval.lit)=malloc(1); (yyval.lit)[0]=0; ;}
    break;

  case 35:
#line 186 "cchr.y"
    { 
				(yyval.lit)=malloc(strlen((yyvsp[(1) - (2)].lit))+strlen((yyvsp[(2) - (2)].lit))+1);
				strcpy((yyval.lit),(yyvsp[(1) - (2)].lit));
				strcat((yyval.lit),(yyvsp[(2) - (2)].lit));
				free((yyvsp[(1) - (2)].lit));
				free((yyvsp[(2) - (2)].lit));
			  ;}
    break;

  case 36:
#line 195 "cchr.y"
    { (yyval.lit)=malloc(1); (yyval.lit)[0]=0; ;}
    break;

  case 37:
#line 196 "cchr.y"
    { 
				(yyval.lit)=malloc(strlen((yyvsp[(1) - (2)].lit))+strlen((yyvsp[(2) - (2)].lit))+1);
				strcpy((yyval.lit),(yyvsp[(1) - (2)].lit));
				strcat((yyval.lit),(yyvsp[(2) - (2)].lit));
				free((yyvsp[(1) - (2)].lit));
				free((yyvsp[(2) - (2)].lit));
			  ;}
    break;

  case 38:
#line 205 "cchr.y"
    { (yyval.token).type = TOKEN_TYPE_FUNC; alist_init((yyval.token).args); (yyval.token).data=NULL; alist_add((yyval.token).args,(yyvsp[(1) - (1)].expr)); ;}
    break;

  case 39:
#line 206 "cchr.y"
    { (yyval.token)=(yyvsp[(1) - (3)].token); alist_add((yyval.token).args,(yyvsp[(3) - (3)].expr)); free((yyvsp[(2) - (3)].lit)); ;}
    break;

  case 40:
#line 208 "cchr.y"
    { cchr_init(&(yyval.cchr)); alist_add((yyval.cchr).exts,(yyvsp[(1) - (1)].lit)); ;}
    break;

  case 41:
#line 209 "cchr.y"
    { (yyval.cchr)=(yyvsp[(1) - (3)].cchr); alist_add((yyval.cchr).exts,(yyvsp[(3) - (3)].lit)); free((yyvsp[(2) - (3)].lit)); ;}
    break;

  case 47:
#line 218 "cchr.y"
    { (yyval.lit)=(yyvsp[(1) - (1)].lit); ;}
    break;

  case 48:
#line 219 "cchr.y"
    { (yyval.lit)=NULL; ;}
    break;

  case 49:
#line 222 "cchr.y"
    { cchr_genrule(&(yyval.cchr),(yyvsp[(1) - (9)].lit),&(yyvsp[(2) - (9)].elist),&(yyvsp[(4) - (9)].elist),&(yyvsp[(6) - (9)].elist),&(yyvsp[(8) - (9)].elist)); free((yyvsp[(3) - (9)].lit)); free((yyvsp[(5) - (9)].lit)); free((yyvsp[(7) - (9)].lit)); free((yyvsp[(9) - (9)].lit)); ;}
    break;

  case 50:
#line 223 "cchr.y"
    { cchr_genrule(&(yyval.cchr),(yyvsp[(1) - (7)].lit),NULL,&(yyvsp[(2) - (7)].elist),&(yyvsp[(4) - (7)].elist),&(yyvsp[(6) - (7)].elist)); free((yyvsp[(3) - (7)].lit)); free((yyvsp[(5) - (7)].lit)); free((yyvsp[(7) - (7)].lit)); ;}
    break;

  case 51:
#line 224 "cchr.y"
    { cchr_genrule(&(yyval.cchr),(yyvsp[(1) - (7)].lit),&(yyvsp[(2) - (7)].elist),NULL,&(yyvsp[(4) - (7)].elist),&(yyvsp[(6) - (7)].elist)); free((yyvsp[(3) - (7)].lit)); free((yyvsp[(5) - (7)].lit)); free((yyvsp[(7) - (7)].lit)); ;}
    break;

  case 52:
#line 225 "cchr.y"
    { cchr_genrule(&(yyval.cchr),(yyvsp[(1) - (7)].lit),&(yyvsp[(2) - (7)].elist),&(yyvsp[(4) - (7)].elist),NULL,&(yyvsp[(6) - (7)].elist)); free((yyvsp[(3) - (7)].lit)); free((yyvsp[(5) - (7)].lit)); free((yyvsp[(7) - (7)].lit)); ;}
    break;

  case 53:
#line 226 "cchr.y"
    { cchr_genrule(&(yyval.cchr),(yyvsp[(1) - (5)].lit),NULL,&(yyvsp[(2) - (5)].elist),NULL,&(yyvsp[(4) - (5)].elist)); free((yyvsp[(3) - (5)].lit)); free((yyvsp[(5) - (5)].lit)); ;}
    break;

  case 54:
#line 227 "cchr.y"
    { cchr_genrule(&(yyval.cchr),(yyvsp[(1) - (5)].lit),&(yyvsp[(2) - (5)].elist),NULL,NULL,&(yyvsp[(4) - (5)].elist)); free((yyvsp[(3) - (5)].lit)); free((yyvsp[(5) - (5)].lit)); ;}
    break;

  case 55:
#line 231 "cchr.y"
    { (yyval.cchr)=(yyvsp[(2) - (3)].cchr); free((yyvsp[(1) - (3)].lit)); free((yyvsp[(3) - (3)].lit)); ;}
    break;

  case 56:
#line 232 "cchr.y"
    { (yyval.cchr)=(yyvsp[(2) - (3)].cchr); free((yyvsp[(1) - (3)].lit)); free((yyvsp[(3) - (3)].lit)); ;}
    break;

  case 57:
#line 233 "cchr.y"
    { cchr_init(&(yyval.cchr)); macro_t *nw; alist_new((yyval.cchr).macros,nw); nw->name=(yyvsp[(2) - (4)].constr); nw->def=(yyvsp[(3) - (4)].expr); free((yyvsp[(1) - (4)].lit)); free((yyvsp[(4) - (4)].lit)); ;}
    break;

  case 58:
#line 234 "cchr.y"
    { cchr_init(&(yyval.cchr)); logical_t *nw; alist_new((yyval.cchr).logicals,nw); nw->name=(yyvsp[(2) - (4)].lit); nw->cb=(yyvsp[(3) - (4)].lit); free((yyvsp[(1) - (4)].lit)); free((yyvsp[(4) - (4)].lit)); ;}
    break;

  case 60:
#line 239 "cchr.y"
    { cchr_init(&(yyval.cchr)); (yyvsp[(1) - (2)].constr).args=(yyvsp[(2) - (2)].constr).args; alist_add((yyval.cchr).constrs,(yyvsp[(1) - (2)].constr)); ;}
    break;

  case 61:
#line 240 "cchr.y"
    { (yyval.cchr)=(yyvsp[(1) - (4)].cchr); (yyvsp[(3) - (4)].constr).args=(yyvsp[(4) - (4)].constr).args; alist_add((yyval.cchr).constrs,(yyvsp[(3) - (4)].constr)); free((yyvsp[(2) - (4)].lit)); ;}
    break;

  case 62:
#line 243 "cchr.y"
    { alist_init((yyval.constr).args); alist_init((yyval.constr).list); (yyval.constr).name=NULL; ;}
    break;

  case 63:
#line 244 "cchr.y"
    { alist_add((yyvsp[(1) - (2)].constr).args,(yyvsp[(2) - (2)].expr)); (yyval.constr)=(yyvsp[(1) - (2)].constr); ;}
    break;

  case 64:
#line 247 "cchr.y"
    { (yyval.constr)=(yyvsp[(2) - (3)].constr); (yyval.constr).name=(yyvsp[(1) - (3)].lit); free((yyvsp[(3) - (3)].lit)); alist_init((yyval.constr).args); ;}
    break;

  case 65:
#line 250 "cchr.y"
    { (yyval.constr).name=NULL; alist_init((yyval.constr).list); ;}
    break;

  case 66:
#line 251 "cchr.y"
    { (yyval.constr) = (yyvsp[(1) - (1)].constr); ;}
    break;

  case 67:
#line 254 "cchr.y"
    { (yyval.constr).name=NULL; alist_init((yyval.constr).list); alist_add((yyval.constr).list,(yyvsp[(1) - (1)].lit)); ;}
    break;

  case 68:
#line 255 "cchr.y"
    { (yyval.constr)=(yyvsp[(1) - (3)].constr); alist_add((yyval.constr).list,(yyvsp[(3) - (3)].lit)); free((yyvsp[(2) - (3)].lit)); ;}
    break;

  case 69:
#line 258 "cchr.y"
    { (yyval.lit) = (yyvsp[(1) - (1)].lit); ;}
    break;

  case 70:
#line 259 "cchr.y"
    { (yyval.lit) = realloc((yyvsp[(1) - (2)].lit),strlen((yyvsp[(1) - (2)].lit))+strlen((yyvsp[(2) - (2)].lit))+2); strcat((yyval.lit)," "); strcat((yyval.lit),(yyvsp[(2) - (2)].lit)); free((yyvsp[(2) - (2)].lit)); ;}
    break;

  case 71:
#line 260 "cchr.y"
    { (yyval.lit) = realloc((yyvsp[(1) - (2)].lit),strlen((yyvsp[(1) - (2)].lit))+strlen((yyvsp[(2) - (2)].lit))+2); strcat((yyval.lit)," "); strcat((yyval.lit),(yyvsp[(2) - (2)].lit)); free((yyvsp[(2) - (2)].lit)); ;}
    break;


/* Line 1267 of yacc.c.  */
#line 2190 "intermediate/cchr.tab.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (&yylloc, scanner, output, YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (&yylloc, scanner, output, yymsg);
	  }
	else
	  {
	    yyerror (&yylloc, scanner, output, YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }

  yyerror_range[0] = yylloc;

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval, &yylloc, scanner, output);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  yyerror_range[0] = yylsp[1-yylen];
  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;

      yyerror_range[0] = *yylsp;
      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp, yylsp, scanner, output);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;

  yyerror_range[1] = yylloc;
  /* Using YYLLOC is tempting, but would change the location of
     the look-ahead.  YYLOC is available though.  */
  YYLLOC_DEFAULT (yyloc, (yyerror_range - 1), 2);
  *++yylsp = yyloc;

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (&yylloc, scanner, output, YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval, &yylloc, scanner, output);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp, yylsp, scanner, output);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 263 "cchr.y"


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




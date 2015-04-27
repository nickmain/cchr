/****************************************************************************\
| CCHR - A CHR-in-C to C compiler                                            |
| output.h - header for minimal indenting output routines                    |
| written by Pieter Wuille                                                   |
\****************************************************************************/

#ifndef _output_h_
#define _output_h_ 1

#include <stdio.h>

#include "alist.h"


typedef struct {
  FILE *out;
  int linenum;
  char *idf;
  int nl;
  alist_declare(char*,les);
} output_t;

void output_unindent(output_t *output);
void output_unindent_till(output_t *output,int level);
void output_indent(output_t *output, char *in, char *out);
void output_string(output_t *output, char *str);
void output_chars(output_t *output, char *str, int len);
void output_char(output_t *output, int ch);
void output_init(output_t *output, FILE *out);
void output_set_line(output_t *output, int line);
int output_get_line(output_t *output);
void output_fmt(output_t *output, char *fmt, ...);
void output_destruct(output_t *output);

#endif

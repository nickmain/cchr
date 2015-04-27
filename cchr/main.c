/****************************************************************************\
| CCHR - A CHR-in-C to C compiler                                            |
| main.c - main compiler routines                                            |
| written by Pieter Wuille                                                   |
\****************************************************************************/ 

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include <errno.h>
#include <time.h>

#include "parsestr.h"
#include "semtree.h"
#include "analyse.h"
#include "codegen.h"
#include "output.h"
#include "timings.h"

#ifdef USE_EFENCE
#include <efence.h>
#endif

#define TIMING_FORMAT "%.2f"

int do_scan(FILE *file, int *line, cchr_t *chr);

/* process an input stream, considering it CCHR code */
int process_file_cchr(FILE *in, output_t *out, output_t *outh, int *line) {
	cchr_t cchr;
	int ok=1;
	timing_t total,sub;
	printf("  - parsing...");
	timing_start(&total);
	if (!do_scan(in,line,&cchr)) {
	        printf(" done, " TIMING_FORMAT "s\n",timing_get(&total));
		sem_cchr_t sem_cchr;
		printf("  - analysing...");
		timing_start(&sub);
		int oko=sem_generate_cchr(&sem_cchr,&cchr);
		timing_stop(&sub);
		destruct_cchr_t(&cchr);
		if (oko) {
		        printf(" done, " TIMING_FORMAT "s\n",timing_get(&total));
			printf("  - code generation...");
			timing_start(&sub);
			csm_generate(&sem_cchr,out,outh);
			timing_stop(&sub);
			printf(" done, " TIMING_FORMAT "s\n",timing_get(&sub));
		} else {
		        printf(" error\n");
			ok=0;
		}
		sem_cchr_destruct(&sem_cchr);
		timing_stop(&total);
		if (ok) printf("  - total: " TIMING_FORMAT "s\n",timing_get(&total));
	} else {
	        printf(" error\n");
		ok=0;
	}
	return ok;
}

/* process a single file. This function will read the passed file, byte per
 * byte, and call process_file_cchr when necessary */
int process_file(FILE *in, output_t *out, output_t *outh, int *line, char *inname, char *outname) {
	char wb[256];
	int ws=0; /* size of word buffer */
	char sb[256];
	int ss=0; /* size of space buffer */
	int c; /* character read */
	int ls=1; /* only spaces have occured after last newline */
	int ok=1; /* return value */
	int ncchr=0;
	printf("%s:\n",inname);
	timing_t filetime;
	timing_start(&filetime);
	output_fmt(out,"#line %i \"%s\"\n",*line,inname);
	output_fmt(outh,"#ifndef _CCHR_OUTPUT_H\n#define _CCHR_OUTPUT_H\n\n");
	while ((c=getc(in)) != EOF) { /* loop over all bytes in the source */
		if (c == '\n') {(*line)++;ls=1;} /* line-number counter */
		if (isalpha(c)) { /* for alphabetical characters */
			ls=0;
			if (ss>0) { /* if word+space buffer are filled */
				output_chars(out,wb,ws); /* write them out */
				ws=0;
				output_chars(out,sb,ss);
				ss=0;
			}
			wb[ws++]=c; /* add this character to word buffer */		
			continue;
		}
		if (c == '#' && ls) { /* for preprocessor statements */
			output_chars(out,wb,ws);
			ws=0;
			output_chars(out,sb,ss);
			ss=0;
			output_char(out,c);
			int bs=0;
			while ((c=getc(in)) != EOF) {
				output_char(out,c);
				if (c=='\n') (*line)++;
				if (c=='\n' && !bs) break;
				if (c=='\\') {
					bs=(!bs);
				} else {
					bs=0;
				}
			}
			ls=1;
			continue;
		}
		if (isspace(c)) { /* whitespace */
			if (ws) {
			  sb[ss++]=c;
			  continue;
			}
		} else {
			ls=0;
		}
		if (c == '{') { /* begin of a block */
			if (ws==4 && !strncmp(wb,"cchr",4)) {
				output_fmt(out,"#line %i \"%s\"\n",output_get_line(out)+1,outname);
				printf("- processing cchr block #%i:\n",(++ncchr));
				if (!process_file_cchr(in,out,outh,line)) ok=0;
				output_fmt(out,"#line %i \"%s\"\n",*line,inname);
				ws=0;
				ss=0;
				continue;
			}
		}
		output_chars(out,wb,ws);
		ws=0;
		output_chars(out,sb,ss);
		ss=0;
		output_char(out,c);
		
	}
	output_chars(out,wb,ws);
	output_chars(out,sb,ss);
	output_fmt(outh,"#endif\n");
	timing_stop(&filetime);
	printf("- %i lines written to %s\n",output_get_line(out),outname);
	printf("- total %s: " TIMING_FORMAT "s\n",inname,timing_get(&filetime));
	return ok;
}


int main(int argc, char *argv[])
{
	setbuf(stdout,NULL);
	srand(time(NULL) ^ getpid());
	int ok=1;
	timing_t totaltime;
	timing_start(&totaltime);
	for (int i=1; i<argc; i++) {
	    char *arg=argv[i];
	    int line=1;
	    char *oa=malloc(strlen(arg)+3);
	    char *oah=malloc(strlen(arg)+8);
	    strcpy(oa,arg);
	    if (strlen(arg)>4 && !strcmp(arg+strlen(arg)-4,".chr")) oa[strlen(arg)-4]=0;
	    if (strlen(arg)>5 && !strcmp(arg+strlen(arg)-5,".cchr")) oa[strlen(arg)-5]=0;
	    strcpy(oah,oa);
	    strcat(oa,".c");
	    strcat(oah,"_cchr.h");
	    FILE *in=fopen(arg,"r");
	    if (in) {
	      FILE *out=fopen(oa,"w");
	      FILE *outh=fopen(oah,"w");
	      output_t oo;
	      output_t ooh;
	      output_init(&oo,out);
	      output_init(&ooh,outh);
	      if (!process_file(in,&oo,&ooh,&line,arg,oa)) ok=0;
	      free(oa);
	      free(oah);
	      output_destruct(&oo);
	      output_destruct(&ooh);
	      fclose(out);
	      fclose(outh);
	      fclose(in);
	    } else {
	      fprintf(stderr,"Error opening %s: %s\n",arg,strerror(errno));
	    }
	}
	timing_stop(&totaltime);
	printf("total: " TIMING_FORMAT "s\n",timing_get(&totaltime));
	return (!ok);
}

/****************************************************************************\
| CCHR - A CHR-in-C to C compiler                                            |
| sem2csm.c - header for conversion of semantic tree to C output             |
| written by Pieter Wuille                                                   |
\****************************************************************************/ 

#ifndef SEM2CSM_H_
#define SEM2CSM_H_

#include <stdio.h>

#include "semtree.h"
#include "output.h"

void csm_generate(sem_cchr_t *in,output_t *out,output_t *header);

#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef enum { ADD,SUB,MULT,DIV,MOVE,I_MOVE,MOVE_I,CONST,JUMP,CJUMP,HALT } instr_t;

typedef struct {
  int next;
  instr_t instr;
  int arg1;
  int arg2;
} prog_t;
  
void ram(prog_t *prog, int *mem, int pos) {
  do {
    prog_t *p=&(prog[pos]);
    switch (p->instr) {
      case ADD: {
        mem[p->arg2] += mem[p->arg1];
	pos=p->next;
	break;
      }
      case SUB: {
        mem[p->arg2] -= mem[p->arg1];
	pos=p->next;
	break;
      }
      case MULT: {
        mem[p->arg2] *= mem[p->arg1];
	pos=p->next;
	break;
      }
      case DIV: {
        mem[p->arg2] /= mem[p->arg1];
	pos=p->next;
	break;
      }
      case MOVE: {
        mem[p->arg2] = mem[p->arg1];
	pos=p->next;
	break;
      }
      case I_MOVE: {
        mem[p->arg2] = mem[mem[p->arg1]];
	pos=p->next;
	break;
      }
      case MOVE_I: {
        mem[mem[p->arg2]] = mem[p->arg1];
	pos=p->next;
	break;
      }
      case CONST: {
        mem[p->arg2] = p->arg1;
	pos=p->next;
	break;
      }
      case JUMP: {
        pos = p->arg2;
	break;
      }
      case CJUMP: {
        if (mem[p->arg1]==0) {
	  pos=p->arg2;
	} else {
	  pos=p->next;
	}
	break;
      }
      case HALT: {
        return;
      }
    }
  } while(1);
}

int main(int argc, char **argv) {
  long l=argc > 1 ? strtol(argv[1],NULL,0) : 10000;
  prog_t progke[]={{0,HALT,0,0},{2,ADD,1,3},{3,SUB,1,2},{1,CJUMP,2,4},{0,HALT,0,0}};
  int mem[]={0,1,l,0};
  ram(progke,mem,1);
  printf("%i\n",mem[3]);
  return 0;
}

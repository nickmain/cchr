#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct {
  uint16_t worse;
  uint16_t equal;
} pos_t;

pos_t poss[6*6*6*6*6];

static const char *names[] = {"9","0","J","Q","K","A"};

void parse(FILE *file) {
  int n=0;
  double val=0.0;
  int v[120];
  int worse=0;
  do {
    char c[256];
    char *s=fgets(c,256,file);
    int i;
    double d;
    if (!s) {
      i=-1;
      d=1.0/0.0;
    } else {
      char *e=NULL;
      d=strtod(s,&e);
      e++;
      i=strtol(e,NULL,10);
    }
    if (d>val*1.000000001) {
      int j=0;
      while (j<n) {
        poss[v[j]].worse=worse;
        poss[v[j]].equal=n;
        j++;
      }
      worse+=n;
      n=0;
    }
    v[n++]=i;
    val=d;
    if (!s) break;
  } while(1);
}

double chance_for_better_than(int i) {
  return (7776-poss[i].equal-poss[i].worse)/7776.0;
}

int ggd(int a,int b) {
  do {
    if (a<b) {
      int c=a; a=b; b=c;
    }
    if (b==0) return a;
    a %= b;
  } while(1);
}

void printmask(int id,int mask) {
  int dc[5]={0,0,0,0,0};
  int ms[5]={0,0,0,0,0};
  for (int i=0; i<5; i++) {
    dc[i]=(id)%6;
    id /= 6;
    ms[i]=!((mask)%2);
    mask /= 2;
  }
  printf("[%s%s %s%s %s%s %s%s %s%s]",ms[0] ? "#" : "",names[dc[0]],ms[1] ? "#" : "",names[dc[1]],ms[2] ? "#" : "",names[dc[2]],ms[3] ? "#" : "",names[dc[3]],ms[4] ? "#" : "",names[dc[4]]);
}

void analyse(int accepted,int received, int *mask, int *num, int *den) {
  int degree=poss[accepted].worse;
  int dice[5]={0,0,0,0,0};
  int dc[5]={0,0,0,0,0};
  for (int i=0; i<5; i++) {
    dice[i]=(received)%6;
    received /= 6;
  }
  int bestm=0,bestb=7776-degree-poss[accepted].equal,bestt=7776;
  double best=(1.0*bestb)/bestt;
  for (int m=31; m>=1; m--) {
    int tot=0;
    int better=0;
    for (dc[0]=((m&1) ? dice[0] : 0); dc[0] <= ((m&1) ? dice[0] : 5); dc[0]++)  {
      for (dc[1]=((m&2) ? dice[1] : 0); dc[1] <= ((m&2) ? dice[1] : 5); dc[1]++)  {
        for (dc[2]=((m&4) ? dice[2] : 0); dc[2] <= ((m&4) ? dice[2] : 5); dc[2]++)  {
          for (dc[3]=((m&8) ? dice[3] : 0); dc[3] <= ((m&8) ? dice[3] : 5); dc[3]++)  {
            for (dc[4]=((m&16) ? dice[4] : 0); dc[4] <= ((m&16) ? dice[4] : 5); dc[4]++)  {
              tot++;
              if (poss[dc[0]+dc[1]*6+dc[2]*6*6+dc[3]*6*6*6+dc[4]*6*6*6*6].worse>degree) better++;
            }
          }
        }
      }
    }
    double nb=(1.0*better)/tot;
    if (nb>best*1.000000001) {
      best=nb;
      bestm=m;
      bestb=better;
      bestt=tot;
    }
  }
  int g=ggd(bestb,bestt);
  *mask=bestm;
  *num=bestb/g;
  *den=bestt/g;
}

int parsestr(char *c) {
  int v=0;
  int n=0;
  while (((*c) != 0) && n<5) {
    char cc=(*(c++));
    for (int i=0; i<6; i++) {
      if (names[i][0] == cc) {
        v=v*6+i;
        n++;
        break;
      }
    }
  }
  return v;
}

int main(int argc, char **argv) {
  FILE *f=fopen("chapeau.txt","r");
  parse(f);
  fclose(f);
  setbuf(stdout,NULL);
  printf("you're about to accept: ");
  char c[256];
  char *s1;
  char *s2;
  if (argc>1) {
    s1=argv[1];
    printf("%s\n",s1);
  } else {
    s1=fgets(c,256,stdin);
  }
  int accept=parsestr(s1);
  double chs=chance_for_better_than(accept);
  printf("you have %g%% chance to throw better from scratch\n",100.0*chs);
  int mask=0,num=0,den=0;
  double mch=2.0;
  int wid=0,wms=0;
  for (int rc=0; rc<7776; rc++) {
    analyse(accept,rc,&mask,&num,&den);
    double chs=(1.0*num)/den;
    if (chs<mch) {
      mch=chs;
      wid=rc;
      wms=mask;
    }
  }
  printf("Worst-case scenario: %g%% with ",100*mch);
  printmask(wid,wms);
  printf("\n");
  printf("you have received: ");
  if (argc>2) {
    s2=argv[2];
    printf("%s\n",s2);
  } else {
    s2=fgets(c,256,stdin);
  }
  int received=parsestr(s2);
  analyse(accept,received,&mask,&num,&den);
  printf("Chance for throwing better: %g%% (%i/%i) with ",(100.0*num)/den,num,den);
  printmask(received,mask); 
  printf("\n");
  return 0;
}

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

#include "lookup3.h"
#include "ht_cuckoo.h"

typedef struct {
  int key;
  int val;
} pair_t;

static const int dinges[]={27,74,17,33,94,18,46,83,65,2,32,53,28,85,99,47,28,82, 6,11,55,29,39,81,90,37,10,0,66,51,7,21};

/*#define PAIR_DEFINED(VAL) ((VAL)->key >= 0)
#define PAIR_UNDEF(VAL) {(VAL)->key = -1;}
#define PAIR_HASH1(VAL) hashbytes(&((VAL)->key),sizeof(int),0x16A09E66UL)
#define PAIR_HASH2(VAL) hashbytes(&((VAL)->key),sizeof(int),0x1BB67AE8UL)
#define PAIR_EQ(V1,V2) ((V1)->key==(V2)->key)

ht_cuckoo_code(inthash_t,pair_t,PAIR_HASH1,PAIR_HASH2,PAIR_EQ,PAIR_DEFINED,PAIR_UNDEF,PAIR_UNDEF) */

#define INT_UNUSED ((int)(0x80000000UL))

void addarrow(int x,int a,int *ht) {
  int b=ht[x];
  if (b != INT_UNUSED) {
    if (a<b) {
      addarrow(a,b,ht);
      return;
    }
    if (a>b) {
      addarrow(b,a,ht);
      return;
    }
  }
  ht[x]=a;
}

void test(void) {
  int *arr=malloc(sizeof(int)*100);
  for (int j=0; j<100; j++) arr[j]=INT_UNUSED;
  int *dpt=malloc(sizeof(int)*32);
  uint32_t dptu=0;
  for (int j=0; j<sizeof(dinges)/sizeof(dinges[0]); j++) {
    int depth=0;
    int val=dinges[j];
    while (dptu & (1<<depth)) {
      int a=val;
      int b=dpt[depth];
      addarrow(a,b,arr);
      dptu &= (~(1<<depth));
      depth++;
      val=a;
    }
    dpt[depth]=val;
    dptu |= (1<<depth);
  }
  free(arr);
  free(dpt);
}

int main(void) {
  for (int k=0; k<500; k++) {
    test();
  }
  return 0;
}

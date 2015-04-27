#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

uint64_t gcd(uint64_t a,uint64_t b) {
  begin:
  if (b==0) return a;
  if (a>=b) {
    a-=b;
    goto begin;
  }
  uint64_t c=b-a;
  b=a;
  a=c;
  goto begin;
}

int main(int argc, char **argv) {
  uint64_t a = (uint64_t)(argc>1 ? strtoull(argv[1],NULL,0) : 100000000);
  uint64_t b = (uint64_t)(argc>2 ? strtoull(argv[2],NULL,0) : 5);
  uint64_t r;
  r=gcd(a,b);
//  printf("gcd(%llu)\n",(unsigned long long)r);
  return 0;
}

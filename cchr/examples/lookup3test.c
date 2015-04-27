#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

uint32_t hashword(const uint32_t *k,size_t length,uint32_t initval);
uint32_t hashlittle( const void *key, size_t length, uint32_t initval);

int main(int argc, char **argv) {
  int len=(int)(argc > 1 ? strtol(argv[1],NULL,0) : 4);
  int loops=(int)(argc > 2 ? strtoull(argv[2],NULL,0)/((unsigned long long)len) : 1ULL);
  int type=(int)(argc > 3 ? strtol(argv[3],NULL,0) : 0);
  printf("Doing %i loops on %ibyte-size data, with type %i\n",loops,len,type);
  uint32_t data[16]={
    0x86726ecd,0x3da72e25,0xfaeb7ab0,0x6a6cf72a,
    0x813e466c,0xf886e2a5,0x73a116b6,0x0c1994db,
    0xc139fb13,0xce08d144,0xfc96a36c,0xfad8b9d1,
    0x79f06513,0x76a0c8a2,0x33f6752a,0x851964dd
  };
  uint32_t hash=0;
  switch (type) {
  case 0: 
    printf("Loop type: lookup3 hashword\n");
    while (loops--) {
      hash=hashword(data,len/4,hash);
    }
    break;
  case 1:
    printf("Loop type: lookup3 hashlittle\n");
    while (loops--) {
      hash=hashlittle(data,len,hash);
    }
    break;
  default:
    printf("Loop type: unknown\n");
  }
  printf("%08x\n",hash);
  return 0;
}

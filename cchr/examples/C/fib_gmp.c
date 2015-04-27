#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <gmp.h>

typedef struct {
  mpz_t v;
} bigint_t;

void fib_gmp(int upto) {
  bigint_t *nums=malloc(sizeof(bigint_t)*3);
  mpz_init_set_si(nums[0].v,1);
  mpz_init_set_si(nums[1].v,1);
  mpz_init_set_si(nums[2].v,1);
  for (int j=2; j<=upto; j++) {
    mpz_clear(nums[j%3].v);
    mpz_init(nums[j%3].v);
    mpz_add(nums[j%3].v,nums[(j+1)%3].v,nums[(j+2)%3].v);
  }
  gmp_printf("fib(%i,%Zd)\n",upto,nums[upto%3].v);
  for (int j=0; j<=2; j++) {
    mpz_clear(nums[j].v);
  }
  free(nums);
}

int main(int argc,char **argv) {
  int a=(argc>1 ? (int)strtol(argv[1],NULL,0) : 92);
  fib_gmp(a);
  return 0;
}

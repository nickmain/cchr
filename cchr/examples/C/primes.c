#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

int genPrimes(int upto) {
  int *primes=malloc((int)(sizeof(int)*upto*3.0/(2.0*log(upto))));
  primes[0]=2;
  int n=1;
  for (int j=3; j<=upto; j++) {
    int k=0;
    while (k<n) {
      int t=primes[k];
      if ((j%t)==0) break;
      k++;
    }
    if (k==n) {
      primes[n++]=j;
    }
  } 
  int ret=primes[n-1];
  free(primes);
  return ret;
}

int main(int argc, char** argv) {
  int n=argc>1 ? strtol(argv[1],NULL,0) : 7500;
  int r=genPrimes(n);
  printf("%i\n",r);
  return 0;
}

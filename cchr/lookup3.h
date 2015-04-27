#ifndef _lookup3_h_
#define _lookup3_h_ 1

#include <stdint.h>
#include <stdlib.h>

uint32_t hashword(const uint32_t *k,                   /* the key, an array of uint32_t values */
size_t          length,               /* the length of the key, in uint32_ts */
uint32_t        initval)         /* the previous hash, or an arbitrary value */
;

uint32_t hashlittle( const void *key, size_t length, uint32_t initval);

uint32_t hashbig( const void *key, size_t length, uint32_t initval);

#if BYTE_ORDER == LITTLE_ENDIAN
#define hashbytes hashlittle
#else
#define hashbytes hashbig
#endif

#endif

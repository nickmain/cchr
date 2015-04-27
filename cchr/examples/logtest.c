#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#ifdef USE_EFENCE
#include <efence.h>
#endif

#include "../logical.h"

logical_code(int,int_log_t)

int main(int argc,char **argv) {
	int_log_t v1=int_log_t_create();
	int_log_t v2=int_log_t_create();
	int_log_t v3=int_log_t_create();
	int_log_t_setval(v3,5);
	assert(int_log_t_hasval(v3));
	assert(int_log_t_getval(v3)==5);
	int_log_t_seteq(v1,v2);
	assert(int_log_t_testeq(v1,v2));
	assert(!int_log_t_testeq(v1,v3));
	assert(!int_log_t_testeq(v2,v3));
	assert(!int_log_t_hasval(v1));
	assert(!int_log_t_hasval(v2));
	assert(int_log_t_hasval(v3));
	assert(int_log_t_getval(v3)==5);
	int_log_t_seteq(v2,v3);
	assert(int_log_t_testeq(v1,v2));
	assert(int_log_t_testeq(v2,v3));
	assert(int_log_t_testeq(v3,v1));
	assert(int_log_t_getval(v1)==5);
	assert(int_log_t_getval(v2)==5);
	assert(int_log_t_getval(v3)==5);
	int_log_t_destruct(v3,NULL);
	assert(int_log_t_testeq(v1,v2));
	int_log_t_destruct(v2,NULL);
	assert(int_log_t_getval(v1)==5);
	int_log_t_destruct(v1,NULL);
	return 0;
}

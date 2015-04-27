/****************************************************************************\
| CCHR - A CHR-in-C to C compiler                                            |
| logical.h - builtin equivalence solver (using GUF)                         |
| written by Pieter Wuille                                                   |
\****************************************************************************/ 

#ifndef LOGICAL_H_
#define LOGICAL_H_ 1

#ifdef USE_EFENCE
#define INLINE
#else
#define INLINE inline
#endif

#ifndef DEBUG
#define LOG_DEBUG(...)
#else
#define LOG_DEBUG(...) __VA_ARGS__
#endif

#include "alist.h"

typedef enum  {
	LOGICAL_NODE_TYPE_NONROOT=0, /* not a root */
	LOGICAL_NODE_TYPE_ROOT=1, /* a root without value */
	LOGICAL_NODE_TYPE_VAL=2 /* a root with value */
} logical_node_type_t;

#define logical_header(in,tag,out) \
	LOG_DEBUG(static int _##out##_nextid = 1;) \
	typedef struct _##out##_struct_ *out; \
	struct _##out##_struct_ { \
		logical_node_type_t	_type; \
		union { \
			struct { \
				out par; /* parent (for type==NONROOT) */ \
			} nonroot; \
			struct { \
				in val; /* value (for _type==VAL) */ \
				tag extra; /* for all roots, evan when not TYPE_VAL */  \
				int rank; /* depth of this tree (for type != NONROOT) */ \
			} root; \
		} _data; \
		int _refcount; /* reference count */ \
		LOG_DEBUG(int _id;) \
	}; \
	out static INLINE out##_copy(out var); \
	out static INLINE out##_normalize(out var); \
	out static INLINE out##_create(); \
	void static INLINE out##_setval(out var,in value); \
	void static out##_seteq(out var1, out var2); \
	void static INLINE out##_destruct(out var); \
	int static INLINE out##_testeq(out var1, out var2); \
	int static INLINE out##_hasval(out var); \
	in static INLINE out##_getval(out var); \
	in static INLINE *out##_getvalp(out var); \
	tag static INLINE *out##_getextrap(out var); \
	
#define logical_code(in,tag,out,cb) \
	out static out##_normalize(out var) { \
		LOG_DEBUG( \
			if (var->_refcount<=0) { \
				LOG_DEBUG(fprintf(stderr,"[normalizing logical (%s) #%i => REFCOUNT==%i !!!]\n",#out,var->_id,var->_refcount);) \
				return NULL; \
			} \
			int oid=var->_id; \
		) \
		if (var->_type==LOGICAL_NODE_TYPE_NONROOT) { /* only when this is no root */ \
			out _top=var->_data.nonroot.par; /* temporary top variable */ \
			while (_top->_type==LOGICAL_NODE_TYPE_NONROOT) { /* first search the top */ \
				_top=_top->_data.nonroot.par; /* by recursively going up */ \
			} \
			while (var->_type==LOGICAL_NODE_TYPE_NONROOT) { /* then do this again (in var itself) */ \
				out _next=var->_data.nonroot.par; /* find who our former parent was */ \
				if (var->_refcount>0) { /* if this node should remain alive */ \
					var->_data.nonroot.par->_refcount--; /* decrease refcount of former parent */  \
					var->_data.nonroot.par=_top; /* make it point to the top */ \
					_top->_refcount++; /* and increase refcount of top */ \
				} else { /* otherwise */ \
					free(var); /* free memory for this node */ \
				} \
				var=_next; /* move to the next node */ \
			} \
			LOG_DEBUG(fprintf(stderr,"[normalized logical (%s) #%i => #%i(rc:%i)]\n",#out,oid,_top->_id,_top->_refcount);) \
			return _top; \
		} \
		return var; \
	} \
	out static INLINE out##_copy(out var) { \
		/*out var=out##_normalize(var);*/ \
		var->_refcount++; \
		LOG_DEBUG(fprintf(stderr,"[copy logical (%s) #%i => refcount=%i]\n",#out,var->_id,var->_refcount);) \
		return var; \
	} \
	out static INLINE out##_create() { \
		out ret; \
		ret=malloc(sizeof(*ret)); \
		ret->_type=LOGICAL_NODE_TYPE_ROOT; \
		ret->_refcount=1; \
		ret->_data.root.rank=0; \
		cb##_created(ret); \
		LOG_DEBUG(ret->_id=_##out##_nextid++;) \
		LOG_DEBUG(fprintf(stderr,"[created logical (%s) #%i]\n",#out,ret->_id);) \
		return ret; \
	} \
	void static INLINE out##_setval(out var,in value) { \
		var=out##_normalize(var); \
		(var)->_type=LOGICAL_NODE_TYPE_VAL; \
		(var)->_data.root.val=value; \
		cb##_changed(var); \
		LOG_DEBUG(fprintf(stderr,"[setval logical (%s #%i)]\n",#out,var->_id);) \
	} \
	void static INLINE out##_setvalp(out var,in *value) { \
		var=out##_normalize(var); \
		(var)->_type=LOGICAL_NODE_TYPE_VAL; \
		(var)->_data.root.val=(*value); \
		cb##_changed(var); \
		LOG_DEBUG(fprintf(stderr,"[setvalp logical (%s #%i]\n",#out,var->_id);) \
	} \
	void static out##_seteq(out var1, out var2) { \
		LOG_DEBUG(fprintf(stderr,"[seteq logical (%s) #%i <-> #%i]\n",#out,var1->_id,var2->_id);) \
		var1=out##_normalize(var1); \
		var2=out##_normalize(var2); \
		LOG_DEBUG(fprintf(stderr,"[seteq roots:  (%s) #%i <-> #%i]\n",#out,var1->_id,var2->_id);) \
		if (var1 != var2) { \
			if ((var1)->_data.root.rank<(var2)->_data.root.rank) { /* if rank2<rank1 */ \
				out _tmp=(var1); \
				(var1)=(var2); /* swap var1 and var2 */ \
				(var2)=_tmp; \
			} \
			cb##_merged(var1,var2); \
			if ((var2)->_type==LOGICAL_NODE_TYPE_VAL) { /* if to-be-child had a value */ \
				(var1)->_type=LOGICAL_NODE_TYPE_VAL; \
				(var1)->_data.root.val=(var2)->_data.root.val; /* this value is moved to parent */ \
			} \
			(var1)->_refcount++; /* its parent's refcount increased */ \
			(var2)->_type=LOGICAL_NODE_TYPE_NONROOT; /* child becomes type NONROOT */ \
			(var2)->_data.nonroot.par=var1; /* its parent is set */ \
			LOG_DEBUG(fprintf(stderr,"[seteq refc: (%s) #%i=%i #%i=%i]\n",#out,var1->_id,var1->_refcount,var2->_id,var2->_refcount);) \
			if ((var1)->_data.root.rank==(var2)->_data.root.rank) (var1)->_data.root.rank++; /* rank increase if necessary */ \
			cb##_changed(var2); \
			cb##_destrtag(var2); \
		} \
	} \
	void static INLINE out##_destruct(out var) { \
		int remtop=1; \
		LOG_DEBUG(fprintf(stderr,"[destruct logical (%s) #%i]\n",#out,var->_id);) \
		out top=out##_normalize(var); \
		if (top != var) { \
			var->_refcount--; \
			if (var->_refcount==0) { \
				free(var); \
			} else { \
				remtop=0; \
			} \
		} \
		if (remtop) { \
			if (top->_refcount==1) { \
				LOG_DEBUG(fprintf(stderr,"[destruct free:    (%s) #%i]\n",#out,top->_id);) \
				if (top->_type==LOGICAL_NODE_TYPE_VAL) { cb##_destrval(top); } \
				cb##_destrtag(top); \
				top->_refcount--; \
				free(top); \
			} else { \
				top->_refcount--; \
			} \
		} \
	} \
	int static INLINE out##_testeq(out var1, out var2) { \
		out var1b=out##_normalize(var1); \
		out var2b=out##_normalize(var2); \
		LOG_DEBUG(fprintf(stderr,"[testeq %p:#%i(#%i) %p:#%i(#%i)]\n",var1,var1->_id,var1b->_id,var2,var2->_id,var2b->_id);) \
		return ((var1b)==(var2b)); \
	} \
	int static INLINE out##_hasval(out var) { \
		var=out##_normalize(var); \
		return (var->_type==LOGICAL_NODE_TYPE_VAL); \
	} \
	in static INLINE out##_getval(out var) { \
		var=out##_normalize(var); \
		return (var)->_data.root.val; \
	} \
	tag static INLINE *out##_getextrap(out var) { \
		var=out##_normalize(var); \
		return &((var)->_data.root.extra); \
	} \
	in static INLINE *out##_getvalp(out var) { \
		var=out##_normalize(var); \
		return &((var)->_data.root.val); \
	}

#endif /*LOGICAL_H_*/

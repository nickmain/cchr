#ifndef _HT_CUCKOO_H_
#define _HT_CUCKOO_H_ 1

#include <stdint.h>

#define ht_cuckoo_code(hash_t,entry_t,gethash1,gethash2,eq,defined,init,unset) \
  typedef struct { \
    int size; \
    int used; \
    entry_t *data; \
  } hash_t;\
  void static inline hash_t ## _init(hash_t *ht) { \
    ht->size=0; \
    ht->used=0; \
    ht->data=NULL; \
  } \
  int static inline hash_t ## _count(hash_t *ht) { \
    return (ht->used); \
  } \
  entry_t static inline *hash_t ## _find(hash_t *ht, entry_t *entry) { \
    if (ht->size) { \
      uint32_t h1=gethash1((entry)),h2=gethash2((entry)); \
      h1 = (h1 >> (32-ht->size)); \
      h2 = (h2 >> (32-ht->size)) | (1 << ht->size); \
      if (defined((ht->data)+h1) && eq((entry),(ht->data)+h1)) return (&(ht->data[h1])); \
      if (defined((ht->data)+h2) && eq((entry),(ht->data)+h2)) return (&(ht->data[h2])); \
    } \
    return NULL; \
  } \
  int static inline hash_t ## _have(hash_t *ht, entry_t *entry) { \
    return ((hash_t ## _find(ht,entry)) != NULL); \
  } \
  int static inline hash_t ## _size(hash_t *ht) { \
    return (ht->size); \
  } \
  int static inline hash_t ## _mem(hash_t *ht) { \
    return (ht->size > 0); \
  } \
  void static inline hash_t ## _double(hash_t *ht) { \
    entry_t *nw=malloc(sizeof(entry_t)*(4 << (ht->size))); \
    for (int i=0; i<(2 << ht->size); i++) { \
      if (ht->size && defined((ht->data)+i)) { \
        uint32_t h=(i < (1 << ht->size)) ? gethash1((ht->data)+i) : gethash2((ht->data)+i); \
        h = (h >> (31-ht->size)) & 1; \
        nw[i<<1 | h]=ht->data[i]; \
        init(nw+(i<<1 | (h^1))); \
      } else { \
        init(nw+(i<<1)); \
	init(nw+(i<<1 | 1)); \
      } \
    } \
    ht->size++; \
    if (ht->data==NULL) { \
      /*fprintf(stderr,"[alloc " #hash_t " hashtable %p]\n",ht);*/ \
    } else { \
      free(ht->data); \
    } \
    ht->data=nw; \
  } \
  void static inline hash_t ## _set(hash_t *ht, entry_t *entry) { \
    entry_t *r=hash_t ## _find(ht,entry); \
    entry_t bak; \
    if (r) { \
      (*r)=(*entry); \
      return; \
    } \
    /*fprintf(stderr,"[adding element to %i-element " #hash_t " hash %p (size %i)]\n",ht->used,ht,ht->size);*/ \
    ht->used++; \
    if (ht->used>(5*(1<< (ht->size)))/6) { \
      hash_t ## _double(ht); \
    } \
    while (1) { \
      int maxiter=(23*ht->size+11)/2; \
      while (maxiter--) { \
        uint32_t h1=gethash1((entry)); \
        h1 = (h1 >> (32-ht->size)); \
        if (!(defined((ht->data)+h1))) { \
	  ht->data[h1]=(*entry); \
	  return; \
	} else { \
          bak=ht->data[h1]; \
	  ht->data[h1]=(*entry); \
	} \
        uint32_t h2=gethash2(&bak); \
        h2 = (h2 >> (32-ht->size)) | (1 << ht->size); \
        if (!(defined((ht->data)+h2))) { \
	  ht->data[h2]=bak; \
	  return; \
	} else { \
          (*entry)=ht->data[h2]; \
	  ht->data[h2]=bak; \
	} \
      } \
      hash_t ## _double(ht); \
    } \
  } \
  void static inline hash_t ## _unset(hash_t *ht, entry_t *entry) { \
    entry_t *r=hash_t ## _find(ht,entry); \
    if (r) { \
      /*fprintf(stderr,"[unsetting element in %i-element " #hash_t " hash %p (size %i)]\n",ht->used,ht,ht->size);*/ \
      ht->used--; \
      unset(r); \
    } else {\
      /*fprintf(stderr,"[not unsetting element in %i-element " #hash_t " hash %p (size %i)]\n",ht->used,ht,ht->size);*/ \
    } \
  } \
  void static inline hash_t ## _free(hash_t *ht) { \
    if (ht->data) { \
      /*fprintf(stderr,"[removing %i-element " #hash_t " hash %p (size %i)]\n",ht->used,ht,ht->size);*/ \
      for (int j=0; j<(2<<ht->size); j++) { \
        if (defined(ht->data+j)) { \
          unset((ht->data+j)); \
        } \
      } \
      ht->size=0; \
      ht->used=0; \
      free(ht->data); \
      ht->data=NULL; \
    } \
  } \
  void static inline hash_t ## _freecopy(hash_t *ht) { \
    if (ht->data) { \
      /*fprintf(stderr,"[removing %i-element " #hash_t " hash %p (size %i)]\n",ht->used,ht,ht->size);*/ \
      ht->size=0; \
      ht->used=0; \
      free(ht->data); \
      ht->data=NULL; \
    } \
  } \
  void static inline hash_t ## _copy(hash_t *from, hash_t *to) { \
    to->size=from->size; \
    to->used=from->used; \
    if (from->data) { \
      to->data=malloc(sizeof(entry_t)*(2<<(from->size))); \
      memcpy(to->data,from->data,sizeof(entry_t)*(2<<(from->size))); \
    } else { \
      to->data=NULL; \
    } \
  } \
  entry_t static inline * hash_t ## _first(hash_t *ht) { \
    if (ht->data) { \
      for (int j=0; j<(2<<(ht->size)); j++) { \
        if (defined(&(ht->data[j]))) { \
	  return (&(ht->data[j])); \
	} \
      } \
    } \
    return NULL; \
  } \
  entry_t static inline * hash_t ## _next(hash_t *ht, entry_t *entry) { \
    if (ht->data) { \
      for (int j=entry-ht->data+1; j<(2<<(ht->size)); j++) { \
        if (defined(&(ht->data[j]))) { \
	  return (&(ht->data[j])); \
	} \
      } \
    } \
    return NULL; \
  } \
  void static hash_t ## _addall(hash_t *to, hash_t *from) { \
    entry_t *ent=hash_t##_first(from); \
    while (ent) { \
      entry_t ent2=(*ent); \
      hash_t##_set(to,(&ent2)); \
      ent=hash_t##_next(from,ent); \
    } \
  } \

#endif

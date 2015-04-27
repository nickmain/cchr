#ifndef _HTL_CUCKOO_H_
#define _HTL_CUCKOO_H_ 1

/* afgewerkt: cuckoo hashtable met ingebouwde link-list voor iteratie */

#include <stdint.h>

#define htl_cuckoo_header(hash_t,entry_t) \
  typedef struct { \
    entry_t val; \
    uint32_t prev,next; \
  } hash_t ## _unit_t; \
  typedef struct { \
    int size; \
    int used; \
    hash_t##_unit_t *data; \
  } hash_t;\
  void static inline hash_t ## _init(hash_t *ht); \
  void static inline hash_t ## _copy(hash_t *dest,hash_t *src); \
  int static inline hash_t ## _count(hash_t *ht); \
  entry_t static inline *hash_t ## _find(hash_t *ht, entry_t *entry); \
  int static inline hash_t ## _have(hash_t *ht, entry_t *entry); \
  int static inline hash_t ## _size(hash_t *ht); \
  int static inline hash_t ## _mem(hash_t *ht); \
  void static inline hash_t ## _double(hash_t *ht); \
  void static inline hash_t ## _unsetx(hash_t *ht, entry_t *entry); \
  void static inline hash_t ## _unset(hash_t *ht, entry_t *entry); \
  void static inline hash_t ## _free(hash_t *ht); \
  void static inline hash_t ## _freecopy(hash_t *ht); \
  entry_t static inline * hash_t ## _first(hash_t *ht); \
  entry_t static inline * hash_t ## _next(hash_t *ht, entry_t *entry); \
  void static hash_t ## _addall(hash_t *to, hash_t *from); \

#define htl_cuckoo_code(hash_t,entry_t,gethash1,gethash2,eq,defined,init,unset) \
  void static inline hash_t ## _init(hash_t *ht) { \
    ht->size=0; \
    ht->used=0; \
    ht->data=NULL; \
  } \
  void static inline hash_t ## _copy(hash_t *dest,hash_t *src) { \
    dest->size=src->size; \
    dest->used=src->size; \
    if (dest->size) { \
      dest->data=malloc((1+(2<<src->size))*sizeof(*(dest->data))); \
      memcpy(dest->data,src->data,(1+(2<<src->size))*sizeof(*(dest->data))); \
    } else { \
      dest->data=NULL; \
    } \
  } \
  int static inline hash_t ## _count(hash_t *ht) { \
    return (ht->used); \
  } \
  hash_t##_unit_t static inline *hash_t ## _findx(hash_t *ht, entry_t *entry) { \
    if (ht->size) { \
      uint32_t h1=gethash1((entry)),h2=gethash2((entry)); \
      h1 = (h1 >> (32-ht->size)); \
      h2 = (h2 >> (32-ht->size)) | (1 << ht->size); \
      if (defined(&(ht->data[h1].val)) && eq((entry),&(ht->data[h1].val))) return (&(ht->data[h1])); \
      if (defined(&(ht->data[h2].val)) && eq((entry),&(ht->data[h2].val))) return (&(ht->data[h2])); \
    } \
    return NULL; \
  } \
  entry_t static inline *hash_t ## _find(hash_t *ht, entry_t *entry) { \
    hash_t##_unit_t *ret=hash_t##_findx(ht,entry); \
    if (ret) return &(ret->val); \
    return NULL; \
  } \
  int static inline hash_t ## _have(hash_t *ht, entry_t *entry) { \
    return ((hash_t ## _find(ht,entry)) != NULL); \
  } \
  int static inline hash_t ## _mem(hash_t *ht) { \
    return (ht->size > 0); \
  } \
  void static inline hash_t ## _double(hash_t *ht) { \
    hash_t##_unit_t *nw=malloc(sizeof(hash_t##_unit_t)*(1+(4 << (ht->size)))); \
    for (int i=0; i<(4 << ht->size); i++) { \
      init(&(nw[i].val)); \
    } \
    if (ht->size) { \
      uint32_t i=2 << ht->size; \
      uint32_t pr=4 << ht->size; \
      while (ht->data[i].next != (2 << ht->size)) { \
        i=ht->data[i].next; \
        uint32_t h=(i < (1 << ht->size)) ? (gethash1(&(ht->data[i].val))>>(31-ht->size)) : ((gethash2(&(ht->data[i].val))>>(31-ht->size))|(2<<(ht->size))); \
        /*fprintf(stderr,"[%i -> %i]\n",(int)i,(int)h);*/ \
        nw[h].val=ht->data[i].val; \
        nw[h].prev=pr; \
        nw[pr].next=h; \
        pr=h; \
      } \
      nw[pr].next=4 << ht->size; \
      nw[4 << ht->size].prev=pr; \
    } else { \
      nw[4 << ht->size].next=4 << ht->size; \
      nw[4 << ht->size].prev=4 << ht->size; \
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
    hash_t##_unit_t *r=hash_t ## _findx(ht,entry); \
    entry_t bak; \
    if (r) { \
      (r)->val=(*entry); \
      return; \
    } \
    /*fprintf(stderr,"[adding element to %i-element " #hash_t " hash %p (size %i)]\n",ht->used,ht,ht->size);*/ \
    ht->used++; \
    if (ht->used>(5*(1<< (ht->size)))/6) { \
      hash_t ## _double(ht); \
    } \
    uint32_t after=2 << ht->size; \
    while (1) { \
      int maxiter=(23*ht->size+11)/2; \
      while (maxiter--) { \
        uint32_t h1=gethash1((entry)); \
        h1 = (h1 >> (32-ht->size)); \
	/*fprintf(stderr,"[-> %i (%i it left)]\n",(int)h1,maxiter);*/ \
	if (defined(&(ht->data[h1].val))) { \
	  bak=ht->data[h1].val; \
          uint32_t oafter=ht->data[h1].prev; \
	  uint32_t obefore=ht->data[h1].next; \
          ht->data[oafter].next=obefore; \
	  ht->data[obefore].prev=oafter; \
          if (after==h1) after=oafter; \
          uint32_t before=ht->data[after].next; \
	  ht->data[h1].val=(*entry); \
          ht->data[h1].next=before; \
          ht->data[h1].prev=after; \
          ht->data[after].next=h1; \
          ht->data[before].prev=h1; \
          after=oafter; \
        } else { \
          uint32_t before=ht->data[after].next; \
	  ht->data[h1].val=(*entry); \
          ht->data[h1].next=before; \
          ht->data[h1].prev=after; \
          ht->data[after].next=h1; \
          ht->data[before].prev=h1; \
          return; \
        } \
        uint32_t h2=gethash2(&bak); \
        h2 = (h2 >> (32-ht->size)) | (1 << ht->size); \
	/*fprintf(stderr,"[-> %i (%i it left)]\n",(int)h1,maxiter);*/ \
	if (defined(&(ht->data[h2].val))) { \
	  (*entry)=ht->data[h2].val; \
          uint32_t oafter=ht->data[h2].prev; \
	  uint32_t obefore=ht->data[h2].next; \
          ht->data[oafter].next=obefore; \
	  ht->data[obefore].prev=oafter; \
          if (after==h2) after=oafter; \
          uint32_t before=ht->data[after].next; \
	  ht->data[h2].val=bak; \
          ht->data[h2].next=before; \
          ht->data[h2].prev=after; \
          ht->data[after].next=h2; \
          ht->data[before].prev=h2; \
          after=oafter; \
        } else { \
          uint32_t before=ht->data[after].next; \
	  ht->data[h2].val=bak; \
          ht->data[h2].next=before; \
          ht->data[h2].prev=after; \
          ht->data[after].next=h2; \
          ht->data[before].prev=h2; \
          return; \
        } \
      } \
      uint32_t afterh=(after==(2<<ht->size)) ? 0 : ( \
        (after < (1 << ht->size)) ? \
        ((uint32_t)gethash1(&(ht->data[after].val))) >> (31-ht->size) : \
        ((uint32_t)gethash2(&(ht->data[after].val))) >> (31-ht->size) | (2 << ht->size) \
      ); \
      hash_t ## _double(ht); \
      after=(after==(1<<ht->size)) ? 2<<ht->size : afterh; \
    } \
  } \
  /* entry must be a result of an earlier _find() */ \
  void static inline hash_t ## _unsetx(hash_t *ht, entry_t *entry) { \
    hash_t##_unit_t *r=(hash_t##_unit_t*)entry; \
    if (r) { \
      /*fprintf(stderr,"[unsetting element in %i-element " #hash_t " hash %p (size %i)]\n",ht->used,ht,ht->size);*/ \
      ht->used--; \
      ht->data[r->next].prev=r->prev; \
      ht->data[r->prev].next=r->next; \
      unset(&(r->val)); \
    } else {\
      /*fprintf(stderr,"[not unsetting element in %i-element " #hash_t " hash %p (size %i)]\n",ht->used,ht,ht->size);*/ \
    } \
  } \
  void static inline hash_t ## _unset(hash_t *ht, entry_t *entry) { \
    hash_t##_unit_t *r=hash_t ## _findx(ht,entry); \
    hash_t##_unsetx(ht,&(r->val)); \
  } \
  void static inline hash_t ## _free(hash_t *ht) { \
    if (ht->data) { \
      /*fprintf(stderr,"[removing %i-element " #hash_t " hash %p (size %i)]\n",ht->used,ht,ht->size);*/ \
      for (int j=0; j<(2<<ht->size); j++) { \
        if (defined(&(ht->data[j].val))) { \
          unset(&(ht->data[j].val)); \
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
      ht->size=0; \
      ht->used=0; \
      free(ht->data); \
      ht->data=NULL; \
    } \
  } \
  entry_t static inline * hash_t ## _first(hash_t *ht) { \
    if (ht->data) { \
      uint32_t pos=ht->data[2<<ht->size].next; \
      if (pos!=(2<<ht->size)) return &(ht->data[pos].val); \
    } \
    return NULL; \
  } \
  entry_t static inline * hash_t ## _next(hash_t *ht, entry_t *entry) { \
    if (ht->data) { \
      uint32_t pos=((hash_t##_unit_t*)entry)->next; \
      /*fprintf(stderr,"{pos=%lu nxt=%lu prv=%lu}",pos,ht->data[pos].next,ht->data[pos].prev);*/ \
      if (pos!=(2<<ht->size)) return &(ht->data[pos].val); \
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

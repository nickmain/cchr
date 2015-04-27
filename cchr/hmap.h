#ifndef _HMAP_H_
#define _HMAP_H_ 1

#include <stdint.h>
#include "htl_cuckoo.h"
#include "lookup3.h"

#define hmap_header(key_t,val_t,hmap_t) \
  typedef struct { \
    key_t key; \
    val_t val; \
    int act; \
    int iter; \
  } hmap_t ## _entry_t ;\
  htl_cuckoo_header(hmap_t##_hash_t,hmap_t##_entry_t); \
  typedef struct { \
    hmap_t ## _hash_t hash;\
    size_t count; \
  } hmap_t; \
  void static inline hmap_t ## _init(hmap_t *hm); \
  void static inline hmap_t ## _copy(hmap_t *dest, hmap_t *src); \
  int static inline hmap_t ## _count(hmap_t *ht); \
  val_t static inline *hmap_t ## _find(hmap_t *hm, key_t *key); \
  int static inline hmap_t ## _have(hmap_t *hm, key_t *key); \
  int static inline hmap_t ## _mem(hmap_t *hm); \
  void static inline hmap_t ## _set(hmap_t *hm, key_t *key, val_t *val); \
  void static inline hmap_t ## _unset(hmap_t *hm, key_t *key); \
  void static inline hmap_t ## _unsetx(hmap_t *hm, key_t *key); \
  key_t static inline * hmap_t ## _first(hmap_t *hm); \
  key_t static inline * hmap_t ## _next(hmap_t *hm, key_t *key); \
  key_t static inline * hmap_t ## _ufirst(hmap_t *hm); \
  key_t static inline * hmap_t ## _unext(hmap_t *hm, key_t *key); \
  void static inline hmap_t ## _stop(hmap_t *hm, key_t *key); \
  val_t static inline * hmap_t ## _valptr(hmap_t *hm, key_t *key); \
  void static hmap_t ## _addall(hmap_t *to, hmap_t *from); \
  void static hmap_t ## _free(hmap_t *hm); \
  void static hmap_t ## _freecopy(hmap_t *hm); 

#define hmap_code(key_t,val_t,hmap_t,eq,init,undef) \
  uint32_t static inline hmap_t ## _hash1_(hmap_t##_entry_t *val) { \
    return (uint32_t)hashbytes(&((val)->key),sizeof((val)->key),0x23C6EF37UL); \
  } \
  uint32_t static inline hmap_t ## _hash2_(hmap_t##_entry_t *val) { \
    return (uint32_t)hashbytes(&((val)->key),sizeof((val)->key),0x2A54FF53UL); \
  } \
  int static inline hmap_t ## _eq_(hmap_t##_entry_t *v1, hmap_t##_entry_t *v2) { \
    return (eq(&(v1->key),&(v2->key))); \
  } \
  int static inline hmap_t ## _def_(hmap_t##_entry_t *val) { \
    return (val->act || val->iter); \
  } \
  void static inline hmap_t ## _init_(hmap_t##_entry_t *v) { \
    v->act=0; \
    v->iter=0; \
    init(&((v)->val)); \
  } \
  void static inline hmap_t ## _undef_(hmap_t##_entry_t *v) { \
    v->act=0; \
    v->iter=0; \
    undef(&((v)->val)); \
  } \
  htl_cuckoo_code(hmap_t##_hash_t,hmap_t##_entry_t,hmap_t##_hash1_,hmap_t##_hash2_,hmap_t##_eq_,hmap_t##_def_,hmap_t##_init_,hmap_t##_undef_); \
  void static inline hmap_t ## _init(hmap_t *hm) { \
    hmap_t##_hash_t_init(&(hm->hash)); \
    hm->count=0; \
  } \
  void static inline hmap_t ## _copy(hmap_t *dest, hmap_t *src) { \
    if (src) { \
      dest->count=src->count; \
      hmap_t##_hash_t_copy(&(dest->hash),&(src->hash)); \
    } else { \
      hmap_t##_hash_t_init(&(dest->hash)); \
      dest->count=0; \
    } \
  } \
  int static inline hmap_t ## _count(hmap_t *hm) { \
    return hm->count; \
  } \
  hmap_t##_entry_t static inline *hmap_t ## _findx(hmap_t *hm, key_t *key) { \
    hmap_t##_entry_t ent={.key=(*key)}; \
    hmap_t##_entry_t *ret=hmap_t##_hash_t_find(&(hm->hash),&ent); \
    return ret; \
  } \
  val_t static inline *hmap_t ## _find(hmap_t *hm, key_t *key) { \
    hmap_t##_entry_t *ret=hmap_t##_findx(hm,key); \
    if (ret==NULL) return NULL; \
    return &(ret->val); \
  } \
  int static inline hmap_t ## _have(hmap_t *hm, key_t *key) { \
    return ((hmap_t ## _find(hm,key)) != NULL); \
  } \
  int static inline hmap_t ## _mem(hmap_t *hm) { \
    return hmap_t##_hash_t_mem(&(hm->hash)); \
  } \
  void static inline hmap_t ## _set(hmap_t *hm, key_t *key, val_t *val) { \
    hmap_t##_entry_t entry={.key=(*key)}; \
    hmap_t##_entry_t *r=hmap_t ## _hash_t_find(&(hm->hash),&entry); \
    if (r) { \
      r->val=(*val); \
      hm->count += (1-r->act); \
      r->act=1; \
    } else { \
      entry.key=(*key); \
      entry.val=(*val); \
      entry.act=1; \
      entry.iter=0; \
      hmap_t##_hash_t_set(&(hm->hash),&entry); \
      hm->count++; \
    } \
  } \
  void static inline hmap_t ## _unset(hmap_t *hm, key_t *key) { \
    hmap_t ## _entry_t *r=hmap_t ##_findx(hm,key); \
    hmap_t##_unsetx(hm,&(r->key)); \
  } \
  void static inline hmap_t ## _unsetx(hmap_t *hm, key_t *key) { \
    hmap_t ## _entry_t *r=(hmap_t ## _entry_t *)key; \
    if (r) { \
      hm->count -= r->act; \
      r->act=0; \
      if (r->iter==0) hmap_t##_hash_t_unsetx(&(hm->hash),r); \
    } \
  } \
  key_t static inline * hmap_t ## _first(hmap_t *hm) { \
    if (hm->count) { \
      hmap_t##_entry_t *ent=hmap_t##_hash_t_first(&(hm->hash)); \
      do { \
        if (ent==NULL) return NULL; \
        if (ent->act) break; \
        ent=hmap_t##_hash_t_next(&(hm->hash),ent); \
      } while(1); \
      ent->iter++; \
      return (&(ent->key)); \
    } \
    return NULL; \
  } \
  key_t static inline * hmap_t ## _ufirst(hmap_t *hm) { \
    if (hm->count) { \
      hmap_t##_entry_t *ent=hmap_t##_hash_t_first(&(hm->hash)); \
      do { \
        if (ent==NULL) return NULL; \
        if (ent->act) break; \
        ent=hmap_t##_hash_t_next(&(hm->hash),ent); \
      } while(1); \
      return (&(ent->key)); \
    } \
    return NULL; \
  } \
  key_t static inline * hmap_t ## _next(hmap_t *hm, key_t *key) { \
    hmap_t##_entry_t *ent=(hmap_t##_entry_t*)key; \
    hmap_t##_entry_t *next=hmap_t##_hash_t_next(&(hm->hash),ent); \
    ent->iter--; \
    if (ent->iter==0 && ent->act==0) hmap_t##_hash_t_unsetx(&(hm->hash),ent); \
    do { \
      if (next==NULL) return NULL; \
      if (next->act) break; \
      next=hmap_t##_hash_t_next(&(hm->hash),next); \
    } while(1); \
    next->iter++; \
    return (&(next->key)); \
  } \
  key_t static inline * hmap_t ## _unext(hmap_t *hm, key_t *key) { \
    hmap_t##_entry_t *ent=(hmap_t##_entry_t*)key; \
    hmap_t##_entry_t *next=hmap_t##_hash_t_next(&(hm->hash),ent); \
    do { \
      if (next==NULL) return NULL; \
      if (next->act) break; \
      next=hmap_t##_hash_t_next(&(hm->hash),next); \
    } while(1); \
    return (&(next->key)); \
  } \
  void static inline hmap_t ## _stop(hmap_t *hm, key_t *key) { \
    if (key==NULL) return; \
    hmap_t##_entry_t *ent=(hmap_t##_entry_t*)key; \
    ent->iter--; \
    if (ent->iter==0 && ent->act==0) hmap_t##_hash_t_unsetx(&(hm->hash),ent); \
  } \
  val_t static inline * hmap_t ## _valptr(hmap_t *hm, key_t *key) { \
    hmap_t##_entry_t *ent=(hmap_t##_entry_t*)key; \
    return (&(ent->val)); \
  } \
  void static hmap_t ## _addall(hmap_t *to, hmap_t *from) { \
    key_t *key=hmap_t##_first(from); \
    while (key) { \
      val_t *val=hmap_t##_valptr(from,key); \
      hmap_t##_set(to,key,val); \
      key=hmap_t##_next(from,key); \
    } \
  } \
  void static hmap_t ## _free(hmap_t *hm) { \
    if (hmap_t##_hash_t_count(&(hm->hash))-hm->count) { \
      fprintf(stderr,"warning: freeing hmap_" #hmap_t " with still %i elements being iterated over",hmap_t##_hash_t_count(&(hm->hash))-hm->count); \
    } \
    key_t *key=hmap_t##_first(hm); \
    while (key) { \
      hmap_t ## _unsetx(hm,key); \
      key=hmap_t##_next(hm,key); \
    } \
    hmap_t##_hash_t_free(&(hm->hash)); \
    hm->count=0; \
  } \
  void static hmap_t ## _freecopy(hmap_t *hm) { \
    if (hm) { \
      hmap_t##_hash_t_freecopy(&(hm->hash)); \
      hm->count=0; \
    } \
  } \

#endif

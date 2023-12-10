// used for the c backend

#pragma once

#ifdef AE_DEBUG
void ae_assert(bool b) {
    if (!b) {
        ae_panic("assertion failed")
    }
}
#else
#define ae_assert(b)
#endif

#define inline __attribute__((always_inline))

#include <stdint.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdatomic.h>
#include <assert.h>

static_assert(sizeof(uint32_t) == sizeof(_Atomic(uint32_t)),
              "atomic correct size");

typedef size_t ae_value;

typedef enum {
    AE_TAG_FUN,
    AE_TAG_PAP,
} ae_tag;

typedef struct {
    uint8_t fields;
    uint8_t _field_index;
    uint8_t other;
    uint8_t tag;
    uint32_t ref_count;
} ae_object;

typedef struct {
    void *box;
} ae_box;

void ae_panic(char *s);

static inline _Atomic(uint32_t) *ae_rc_ref(ae_object *o) {
    return (_Atomic(uint32_t) *)&o->ref_count;
}

static inline uint32_t ae_rc(ae_object *o) {
    return atomic_load_explicit(ae_rc_ref(o), memory_order_relaxed);
}

static inline void ae_set_rc(ae_object *o, uint32_t rc) {
    return atomic_store_explicit(ae_rc_ref(o), rc, memory_order_relaxed);
}

#define ae_ref_count_is_thread_shared(rc) ((int32_t)(rc) < 0)

#define ae_ref_count_is_unique_or_thread_shared(rc) ((int32_t)(rc) <= 0)

#define ae_box_is_object(box) (((size_t)box.box & 1) == 0)

#define ae_box_is_value(box) (((size_t)box.box & 1) == 1)

static inline ae_value ae_box_as_value(ae_box box) {
    ae_assert(ae_box_is_value(box));
    return (size_t)box.box >> 1;
}

static inline ae_object *ae_box_as_object(ae_box box) {
    ae_assert(ae_box_is_object(box));
    return (ae_object *)box.box;
}

void ae_dup_cold(ae_object *o, uint32_t rc);

void ae_drop_cold(ae_object *o, uint32_t rc);

static inline void ae_object_drop(ae_object *o) {
    uint32_t rc = ae_rc(o);
    if (ae_ref_count_is_unique_or_thread_shared(rc)) {
        ae_drop_cold(o, rc);
    } else {
        ae_set_rc(o, rc - 1);
    }
}

static inline void ae_object_dup(ae_object *o) {
    uint32_t rc = ae_rc(o);
    if (ae_ref_count_is_thread_shared(rc)) {
        ae_dup_cold(o, rc);
    } else {
        ae_set_rc(o, rc + 1);
    }
}

static inline void ae_box_dup(ae_box box) {
    if (ae_box_is_object(box)) {
        ae_object_dup(ae_box_as_object(box));
    }
}

static inline void ae_box_drop(ae_box box) {
    if (ae_box_is_object(box)) {
        ae_object_drop(ae_box_as_object(box));
    }
}

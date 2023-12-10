#pragma once

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

typedef struct {
    uint8_t fields;
    uint8_t _field_index;
    uint8_t other;
    uint8_t tag;
    uint32_t ref_count;
} ae_object;

static inline bool ae_ref_count_is_thread_shared(uint32_t rc) {
    return (int32_t)rc < 0;
}

static inline bool ae_ref_count_is_unique_or_thread_share(uint32_t rc) {
    return (int32_t)rc <= 0;
}

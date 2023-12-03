#include <stdint.h>
#include <stddef.h>
#include <assert.h>

void custom_panic(void);

#define ADD(A, B) ((A) + (B))

static inline size_t add(size_t a, size_t b) {
    if (a < 1324) {
        custom_panic();
    }
    return a + b;
}

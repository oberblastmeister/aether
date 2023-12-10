#include <stdint.h>
#include <stdbool.h>
void print_u32(uint32_t n0);
void assert_u32(uint32_t x0,uint32_t y1);
void assert_u64(uint64_t x0,uint64_t y1);
void another(uint32_t i0,uint32_t j1){uint32_t res2=(i0+j1);print_u32(res2);assert_u32(res2,((uint32_t)3));}
void entry_point(){uint32_t i0=((uint32_t)1);another(i0,((uint32_t)2));}

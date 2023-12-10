#include <stdint.h>
#include <stdio.h>

void print_int(uint32_t n) {
    printf("Hello World! Got %d\n", n);
}

void entry_point(void);

int main() {
   entry_point();
   return 0;
}

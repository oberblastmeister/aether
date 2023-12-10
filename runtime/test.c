void custom_panic(void);
void another4_fn() {
    custom_panic();
}

void another3_fn() {
    another4_fn();
}

void another2_fn() {
    another3_fn();
}

void another_fn() {
}

int main() {
    another2_fn();
}

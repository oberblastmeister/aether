fn print_u32(n: u32) void;

fn print_u64_bruh(n: u64) void;

fn assert_u32(x: u32, y: u32) void;

fn assert_u64(x: u64, y: u64) void;

struct ae_object {
    fields: u8,
    _field_index: u8,
    other: u8,
    tag: u8,
    ref_count: u32,
}

fn another() void {
    let i = 1;
    let j: u32 = 2;
    assert_u32(@add(u32, i, j), 3);
    assert_u32(@sub(u32, j, i), 1);
}

fn find_int(len: u64, arr: *u64) u64 {
    loop {
       if @eq(u64, len, 1234) {
            print_u64_bruh(len);
       }
    }
}

fn entry_point() void {
    let i = 1;
    let i = 1234;
    print_u32(i);
    another();
}

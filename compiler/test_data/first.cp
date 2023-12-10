fn print_u32(n: u32) void;

fn assert_u32(x: u32, y: u32) void;

fn assert_u64(x: u64, y: u64) void;

fn another(i: u32, j: u32) void {
    let res: u32 = @add(u32, i, j);
    print_u32(res);
    assert_u32(res, 3);
}

fn entry_point() void {
    let i: u32 = 1;
    another(i, 2);
}

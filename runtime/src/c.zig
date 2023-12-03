pub export fn custom_panic() void {
    @panic("wowowowowowowk");
}

pub usingnamespace @cImport({
    @cInclude("testing.h");
});

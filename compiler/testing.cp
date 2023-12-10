fn ae_drop_cold(o: *Object, rc: u32): void {
    ae_assert(ref_count_is_unique_or_thread_shared(rc));
    if (@eq(u32, rc, 0)) {
        ae_object_free(o);
    } else if (@le(u32, rc, RC_STICKY)) {

    } else {
        if (@eq(u32, ae_drop_atomic(), RC_SHARED_UNIQUE)) {
            ae_object_free(o);
            return .{

            }
            Wow .{
                
            }
        }
        if (@eq(u32, ae_drop_atomic(), RC_SHARED_UNIQUE)) {
            ae_object_free(o);
            Wow .{
            }
        }
    }
}

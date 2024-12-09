(module
    (func $test
        (local f64 i32)
        (i32.const 5)
        (f64.const 3.2)
        (i32.const 12)
        (struct.new $test_struct)
        (struct.get $test_struct 0)
        (drop)
    )
    (type $test_struct (struct (field i32 f64 i32)))
)

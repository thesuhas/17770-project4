(module
                (func $global_escape
                    (local $local_ref_1 (ref null $s1))
                    (local $local_ref_2 (ref null $s1))
                    (local $local_ref_1_2 (ref null $s1))

                    (local $local_ref_4 (ref null $s3))
                    (local $local_ref_5 (ref null $s3))

                    (local.set $local_ref_1 (struct.new $s1 (i32.const 1)))
                    (local.set $local_ref_2 (struct.new $s1 (i32.const 2)))

                    (local.get $local_ref_1)
                    (local.set $local_ref_1_2)

                    (block
                        (i32.const 0)
                        (i32.eqz)
                        (br_if 0)
                        (local.get $local_ref_2)
                        (local.set $local_ref_1_2) ;; now local_ref_1_2 is unknown
                    )

                    (local.get $local_ref_1_2)
                    (struct.get $s1 0)

                    (local.set $local_ref_4 (struct.new $s3 (i32.const 0) (i32.const 1) (f64.const 1.5)))
                    (local.set $local_ref_5 (struct.new $s3 (i32.const 2) (i32.const 15) (f64.const 3.2)))
                    (local.get $local_ref_4)
                    (local.get $local_ref_5)
                    (local.set $local_ref_4)
                    (local.set $local_ref_5) ;; not actually aliased, just swapped

                    drop
                )
                (type $s1 (struct (field i32)))
                (type $s2 (struct (field i32 i32)))
                (type $s3 (struct (field i32 i32 f64)))
            )

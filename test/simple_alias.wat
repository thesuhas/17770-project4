(module
                (global $global_ref (mut (ref null struct)) (ref.null struct))
                (func $global_escape
                    (local $local_ref_1 (ref null struct))
                    (local $local_ref_2 (ref null struct))
                    (local $local_ref_1_2 (ref null struct))

                    (local $local_ref_4 (ref null struct))
                    (local $local_ref_5 (ref null struct))

                    (local.set $local_ref_1 (struct.new $s1))
                    (local.set $local_ref_2 (struct.new $s1))

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

                    (local.set $local_ref_4 (struct.new $s3))
                    (local.set $local_ref_5 (struct.new $s3))
                    (local.get $local_ref_4)
                    (local.get $local_ref_5)
                    (local.set $local_ref_4)
                    (local.set $local_ref_5) ;; not actually aliased, just swapped

                    (global.set $global_ref (local.get $local_ref_1_2))
                )
                (type $s1 (struct (field i32)))
                (type $s2 (struct (field i32 i32)))
                (type $s3 (struct (field i32 i32 f64)))
            )

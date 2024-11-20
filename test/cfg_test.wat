(module
        (global $global_ref (mut (ref null struct)) (ref.null struct))
        (func $cfg_test
            i32.const 10 ;; 0
            i32.const 5
            i32.add
            
            block ;; 3
                i32.const 5
                i32.const 15
                i32.sub

                loop ;; 7
                    i32.const 12
                    i32.const 1
                    i32.add
                    br_if 1
                    i32.const 15
                    drop
                    br 0
                end
            end ;; 16

            i32.const 15
            i32.const 12 ;; 18
        )
        (type $struct_type (struct (field i32)))
    )
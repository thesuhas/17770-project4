(module
        (global $global_ref (mut (ref null struct)) (ref.null struct))
        (func $cfg_test
            i32.const 10 ;; SL 1
            i32.const 5 ;; SL 2
            i32.add ;; SL 1
            
            block
                i32.const 5 ;; SL 2
                i32.const 15 ;; SL 3
                i32.sub ;; SL 2

                loop ;; SL 2
                    i32.const 12 ;; SL 3
                    i32.const 1 ;; SL 4
                    i32.add ;; SL 3
                    br_if 1 ;; SL 2
                    i32.const 15 ;; SL 3
                    drop ;; SL 2
                    br 0 ;; SL 2
                end

                i32.const 5 ;; SL 3
                i32.const 10 ;; SL 4
                i32.add ;; SL 3
                drop ;; SL 2
                drop ;; SL 1
            end

            i32.const 15 ;; SL 3
            i32.const 12 ;; SL 4
            i32.add ;; SL 3
            drop
        )
        (type $struct_type (struct (field i32)))
    )

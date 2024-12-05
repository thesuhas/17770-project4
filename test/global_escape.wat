(module
                (global $global_ref (mut (ref null struct)) (ref.null struct))
                (func $global_escape
                    (local $struct_ref (ref null struct))
                    (local.set $struct_ref (struct.new $struct_type))
                    (global.set $global_ref (local.get $struct_ref))
                )
                (type $struct_type (struct (field i32)))
            )

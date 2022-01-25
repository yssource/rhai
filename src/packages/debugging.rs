#![cfg(feature = "debugging")]

use crate::def_package;
use crate::plugin::*;
#[cfg(feature = "no_std")]
use std::prelude::v1::*;

#[cfg(not(feature = "no_function"))]
use crate::{Dynamic, NativeCallContext};

#[cfg(not(feature = "no_function"))]
#[cfg(not(feature = "no_index"))]
use crate::Array;

#[cfg(not(feature = "no_function"))]
#[cfg(not(feature = "no_object"))]
use crate::Map;

def_package! {
    /// Package of basic debugging utilities.
    crate::DebuggingPackage => |lib| {
        lib.standard = true;

        combine_with_exported_module!(lib, "debugging", debugging_functions);
    }
}

#[export_module]
mod debugging_functions {
    #[cfg(not(feature = "no_function"))]
    #[cfg(not(feature = "no_index"))]
    pub fn stack_trace(ctx: NativeCallContext) -> Array {
        if let Some(global) = ctx.global_runtime_state() {
            global
                .debugger
                .call_stack()
                .iter()
                .rev()
                .map(
                    |frame @ crate::debugger::CallStackFrame {
                         fn_name: _fn_name,
                         args: _args,
                         source: _source,
                         pos: _pos,
                     }| {
                        let call = frame.to_string();

                        #[cfg(not(feature = "no_object"))]
                        {
                            let mut map = Map::new();
                            map.insert("call".into(), call.into());
                            map.insert("fn_name".into(), _fn_name.into());
                            if !_args.is_empty() {
                                map.insert(
                                    "args".into(),
                                    Dynamic::from_array(_args.clone().to_vec()),
                                );
                            }
                            if !_source.is_empty() {
                                map.insert("source".into(), _source.into());
                            }
                            if !_pos.is_none() {
                                map.insert(
                                    "line".into(),
                                    (_pos.line().unwrap() as crate::INT).into(),
                                );
                                map.insert(
                                    "position".into(),
                                    (_pos.position().unwrap_or(0) as crate::INT).into(),
                                );
                            }
                            Dynamic::from_map(map)
                        }
                        #[cfg(feature = "no_object")]
                        call.into()
                    },
                )
                .collect()
        } else {
            Array::new()
        }
    }
}
